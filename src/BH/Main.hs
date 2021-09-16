{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module BH.Main (
  module BH.Main.Types,
  readSwInfo,
{-  queryPorts,
  searchMacs,
  queryMacs,
  queryIPs,-}
  InfoDb(..),
)
where

import Data.Aeson
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe
import Data.List
import qualified Data.Set as S
import Control.Monad.State

import BH.Main.Types
import BH.Common
import BH.IP
import BH.Switch
import BH.Switch.Cisco

import BH.Cache
import BH.Telnet

-- FIXME: Use generic yaml reading func.
readSwInfo :: (MonadIO m, MonadError String m) => FilePath -> m SwInfo
readSwInfo file = toSwInfo <$> readYaml file
 where
  toSwInfo :: [SwData] -> SwInfo
  toSwInfo = M.fromList . map (\x -> (swName x, x))

-- TODO: I may use hash to determine changed db file. And then treat /that/
-- file as source and generate others from it.
-- | Search mac on all switches.
searchMacs ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  [MacAddr] ->
  m MacInfo
searchMacs macs = do
  Config{..} <- ask
  resolveMacIPs macIpMap <$> runReaderT (runTill maybeMacs go) swInfo
 where
  maybeMacs :: MacInfo -> Maybe [MacAddr]
  maybeMacs res = let found = M.keys . M.filter (not . S.null . macSwPorts) $ res
                in  case filter (`notElem` found) macs of
                      [] -> Nothing
                      xs -> Just xs
  go :: TelnetRunM TelnetParserResult [MacAddr] MacInfo ()
  go = do
    ms  <- asks telnetIn
    findMacInfo ms >>= putResult
    sendExit

searchIPs ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  [IP] ->
  m IPInfo
searchIPs ips = do
  Config{..} <- ask
  let macs = mapMaybe (flip M.lookup ipMacMap) ips
  macInfoToIPInfo <$> searchMacs macs

-- | This is not really "search", but just quering info about ports. It names
-- this way just to make show its relevant to 'searchX' family of functions.
searchPorts ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  [SwPort] -> m SwPortInfo
searchPorts swPorts = do
  Config{..} <- ask
  resolvePortIPs macIpMap <$> runReaderT (runOn onPorts swNames go) swInfo
 where
  -- FIXME: Change input to (S.Set PortNum). But for this to have any sense, i
  -- need to change it everywhere, includeing findX funcs.. [refactor]
  onPorts :: SwName -> [PortNum]
  onPorts sn = nub . map portSpec . filter ((== sn) . portSw) $ swPorts
  swNames :: [SwName]
  swNames = nub . map portSw $ swPorts
  go :: TelnetRunM TelnetParserResult [PortNum] SwPortInfo ()
  go = do
    portSw <- asks (swName . switchData)
    ps <- asks telnetIn
    res <- M.mapKeys (\p -> SwPort{portSpec = p, ..}) <$> findPortInfo ps
    putResult res
    sendExit

-- FIXME: In fact, in all queryX functions i need unique items. May be change
-- type to 'S.Set' to force uniqueness?
-- | Query several ports.
queryPorts ::
  (MonadReader Config m, MonadState SwPortInfo m, MonadError String m, MonadIO m) =>
  [SwPort] -> m SwPortInfo
queryPorts swPorts = do
  queried <- searchPorts swPorts
  modify (queried <>)
  return queried

-- | Query mac address in 'MacInfo' (presumably obtained from cache) and
-- update info, if found. Otherwise search for mac address.
queryMacs ::
  (MonadReader Config m, MonadState MacInfo m, MonadError String m, MonadIO m)
  => [MacAddr]
  -> m MacInfo
queryMacs macs0 = do
  -- TODO: Can this pattern be generalized?
  cached <- M.filterWithKey (\m _ -> m `elem` macs0) <$> get
  let f :: MacInfo -> [SwPort]
      f = nub . concatMap (S.toList . macSwPorts) . M.elems
  updated <- swPortInfoToMacInfo <$> searchPorts (f cached)
  modify (updated <>)
  let found = M.filterWithKey (\m _ -> m `elem` macs0) updated
      macs1 = macs0 \\ M.keys found
  liftIO $ print "Found in cache: "
  liftIO $ print found
  liftIO $ print $ "Yet to query: " ++ show macs1
  queried <- searchMacs macs1
  modify (queried <>)
  return (M.unionWith (<>) found queried)

-- FIXME: 'queryIPs' and 'queryMacs' seems identical. Can i generalize them?
queryIPs ::
  (MonadReader Config m, MonadState IPInfo m, MonadError String m, MonadIO m)
  => [IP]
  -> m IPInfo
queryIPs ips0 = do
  -- TODO: Can this pattern be generalized?
  cached <- M.filterWithKey (\i _ -> i `elem` ips0) <$> get
  let f :: IPInfo -> [SwPort]
      f = nub . concatMap (S.toList . ipSwPorts) . M.elems
  updated <- swPortInfoToIPInfo <$> searchPorts (f cached)
  modify (updated <>)
  let found = M.filterWithKey (\i _ -> i `elem` ips0) updated
      ips1 = ips0 \\ M.keys found
  liftIO $ print "Found in cache: "
  liftIO $ print found
  liftIO $ print $ "Yet to query: " ++ show ips1
  queried <- searchIPs ips1
  modify (queried <>)
  return (M.unionWith (<>) found (M.filterWithKey (\i _ -> i `elem` ips1) queried))

{-queryX ::
  (MonadReader Config m, MonadState IPInfo m, MonadError String m, MonadIO m)
  => [a]
  -> m (InfoDb a)
queryX xs = do
  -- TODO: Can this pattern be generalized?
  Config{..} <- ask
  cached <- M.filterWithKey (\x _ -> x `elem` xs) <$> get
  let f :: InfoDb a -> [SwPort]
      f = nub . concatMap (S.toList . xSwPorts) . M.elems
  updated <- swPortInfoToX <$> searchPorts (f cached)
  modify (updated <>)
  let found = M.filterWithKey (\x _ -> x `elem` xs) updated
      xs' = xs \\ M.keys found
  liftIO $ print "Found in cache: "
  liftIO $ print found
  liftIO $ print $ "Yet to query: " ++ show xs'
  queried <- searchX xs'
  modify (queried <>)
  return (M.unionWith (<>) found (M.filterWithKey (\x _ -> x `elem` xs) queried))-}

class ( Ord (IElem c)
      , Semigroup (IData c)
      , c ~ M.Map (IElem c) (IData c)
      , FromJSONKey (IElem c)
      , FromJSON (IData c)
      , ToJSONKey (IElem c)
      , ToJSON (IData c)
      , Show (IElem c)
      , Show (IData c))
    => InfoDb c where
  type IElem c
  type IData c
  getSwPorts :: c -> [SwPort]
  fromSwPortInfo :: SwPortInfo -> c
  search ::
    (MonadReader Config m, MonadError String m, MonadIO m)
    => [IElem c] -> m c
  query ::
    (MonadReader Config m, MonadState c m, MonadError String m, MonadIO m)
    => [IElem c] -> m c
  query xs = do
    cached <- M.filterWithKey (\x _ -> x `elem` xs) <$> get
{-    let f :: MacInfo -> [SwPort]
        f = nub . concatMap (S.toList . macSwPorts) . M.elems-}
    updated <- fromSwPortInfo <$> search (getSwPorts cached)
    modify (updated <>)
    let found = M.filterWithKey (\x _ -> x `elem` xs) updated
        xs' = xs \\ M.keys found
    liftIO $ print "Found in cache: "
    liftIO $ print found
    liftIO $ print $ "Yet to query: " ++ show xs'
    queried <- search xs'
    modify (queried <>)
    return (M.unionWith (<>) found queried)

instance InfoDb SwPortInfo where
  type IElem SwPortInfo = SwPort
  type IData SwPortInfo = PortData
  getSwPorts = M.keys
  fromSwPortInfo = id
  search = searchPorts
  query swPorts = do
    queried <- search swPorts
    modify (queried <>)
    return queried

instance IData MacInfo ~ MacData => InfoDb MacInfo where
  type IElem MacInfo = MacAddr
  type IData MacInfo = MacData
  getSwPorts = nub . concatMap (S.toList . macSwPorts) . M.elems
  fromSwPortInfo = swPortInfoToMacInfo
  search = searchMacs

instance InfoDb IPInfo where
  type IElem IPInfo = IP
  type IData IPInfo = IPData
  getSwPorts = nub . concatMap (S.toList . ipSwPorts) . M.elems
  fromSwPortInfo = swPortInfoToIPInfo
  search = searchIPs

data family D a


newtype instance D MacAddr = DM MacAddr
newtype instance D IP = DI IP

foo :: D IP
foo = DI (IP 1 2 3 4)

