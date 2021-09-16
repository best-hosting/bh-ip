{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module BH.Main (
  module BH.Main.Types,
  readSwInfo,
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

-- FIXME: Use 'newtype' instead of 'MacInfo'. But then i can not longer
-- constrain 'c ~ M.Map ..', so what to do? Use some kind of 'WrappedMap' ?
-- I may just define class 'MapLike' providing conversion function from a type
-- to a Map. And then use this conversion functions here. That way i won't
-- define instance for 'Map', but for my custom type.
class ( Ord (IElem c)
      , Semigroup (IData c)
      , c ~ M.Map (IElem c) (IData c)
      , ToSwPortInfo c
      , FromJSONKey (IElem c)
      , FromJSON (IData c)
      , ToJSONKey (IElem c)
      , ToJSON (IData c)
      , Show (IElem c)
      , Show (IData c))
    => InfoDb c where
  -- FIXME: Rename to 'InfoKey' and 'InfoData'.
  type IElem c
  type IData c
  search ::
    (MonadReader Config m, MonadError String m, MonadIO m)
    => [IElem c] -> m c
  query ::
    (MonadReader Config m, MonadState c m, MonadError String m, MonadIO m)
    => [IElem c] -> m c
  query xs = do
    cached <- M.filterWithKey (\x _ -> x `elem` xs) <$> get
    -- I can use just 'search' below, but then default implementation will
    -- depend on having 'SwPortInfo' defined. That's probably wrong, so i use
    -- concrete 'searchPorts' function.
    updated <- fromSwPortInfo <$> searchPorts (getSwPorts cached)
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
  search = searchPorts
  query swPorts = do
    queried <- search swPorts
    modify (queried <>)
    return queried

instance InfoDb MacInfo where
  type IElem MacInfo = MacAddr
  type IData MacInfo = MacData
  search = searchMacs

instance InfoDb IPInfo where
  type IElem IPInfo = IP
  type IData IPInfo = IPData
  search = searchIPs

