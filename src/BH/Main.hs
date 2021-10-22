{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BH.Main (
  module BH.Main.Types,
  readSwInfo,
  InfoDb(..),
  Searchable(..),
  searchPorts2,
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
import Data.Monoid

import BH.Main.Types
import BH.Common
import BH.IP
import BH.Switch
import BH.Switch.Cisco
import BH.Cache
import BH.Telnet
import BH.IP.Arp

-- FIXME: Use generic yaml reading func.
readSwInfo :: (MonadIO m, MonadError String m) => FilePath -> m SwInfo
readSwInfo file = toSwInfo <$> readYaml file
 where
  toSwInfo :: [SwData] -> SwInfo
  toSwInfo = M.fromList . map (\x -> (swName x, x))

resolvePortIPs = error "resolvePortIPs"

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

searchPorts2 ::
  (MonadReader Config m, MonadError String m, MonadIO m, MonadState (IPInfo, MacInfo, SwPortInfo) m) =>
  [SwPort] -> m ()
searchPorts2 swPorts = do
  Config{..} <- ask
  f <- mergePorts <$> runReaderT (runOn onPorts swNames go) swInfo
  modify f
 where
  -- FIXME: Change input to (S.Set PortNum). But for this to have any sense, i
  -- need to change it everywhere, includeing findX funcs.. [refactor]
  onPorts :: SwName -> [PortNum]
  onPorts sn = nub . map portSpec . filter ((== sn) . portSw) $ swPorts
  swNames :: [SwName]
  swNames = nub . map portSw $ swPorts
  go :: TelnetRunM TelnetParserResult [PortNum] (M.Map SwPort (PortState, S.Set MacAddr)) ()
  go = do
    portSw <- asks (swName . switchData)
    ps <- asks telnetIn
    res <- M.mapKeys (\p -> SwPort{portSpec = p, ..}) <$> findPortInfo2 ps
    putResult res
    sendExit

resolveMacIPs = error "resolveMacIPs"

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
  -- FIXME: Hardcoded vlan! [current]
  maybeMacs :: MacInfo -> Maybe [MacAddr]
  maybeMacs res = let found = M.keys . M.filter (isJust . getLast . macSwPort) $ res
                in  case filter (`notElem` found) macs of
                      [] -> Nothing
                      xs -> Just xs
  go :: TelnetRunM TelnetParserResult [MacAddr] MacInfo ()
  go = do
    ms  <- asks telnetIn
    findMacInfo ms >>= putResult
    sendExit

-- | Search mac on all switches.
searchMacs2 ::
  (MonadReader Config m, MonadError String m, MonadIO m, MonadState (IPInfo, MacInfo, SwPortInfo) m) =>
  [MacAddr] ->
  m ()
searchMacs2 macs = do
  Config{..} <- ask
  _ <- runReaderT (runTill maybeMacs go) swInfo
  return ()
 where
  -- FIXME: Hardcoded vlan! [current]
  maybeMacs :: M.Map MacAddr (SwPort, PortState) -> Maybe [MacAddr]
  maybeMacs res = let found = M.keys res
                  in  case filter (`notElem` found) macs of
                        [] -> Nothing
                        xs -> Just xs
  go :: TelnetRunM TelnetParserResult [MacAddr] (M.Map MacAddr (SwPort, PortState)) ()
  go = do
    portSw <- asks (swName . switchData)
    ms  <- asks telnetIn
    M.map (\p -> (SwPort{portSpec = p, ..}, Up)) <$> (findMacInfo2 ms) >>= putResult
    sendExit

searchIPs ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  [IP] ->
  m IPInfo
searchIPs ips = do
  Config{..} <- ask
  let macs = mapMaybe (flip M.lookup ipMacMap) ips
  searchMacs macs
  error "searchIPs"

-- FIXME: Make a newtype wrapper around (IPInfo, MacIpMap, SwPortInfo) ?
-- And then add function for obtaining 'InfoKey' indexed map from a generic db
-- type [current]
class c ~ M.Map (InfoKey c) (InfoData c) => Searchable c where
  -- FIXME: Rename to 'SearchKey' and 'SearchData' or 'SearchResult'
  type InfoKey c
  type InfoData c
  search ::
    (MonadReader Config m, MonadError String m, MonadIO m)
    => [InfoKey c] -> m c

instance Searchable MacInfo where
  type InfoKey MacInfo = MacAddr
  type InfoData MacInfo = MacData
  search = searchMacs

instance Searchable IPInfo where
  type InfoKey IPInfo = IP
  type InfoData IPInfo = IPData
  search = searchIPs

instance Searchable SwPortInfo where
  type InfoKey SwPortInfo  = SwPort
  type InfoData SwPortInfo = PortData
  search = searchPorts

class (
      ToSwPortInfo c
      , Searchable c
      , Ord (InfoKey c)
      , Semigroup (InfoData c)
      , Show (InfoKey c)
      , Show (InfoData c))
    => InfoDb c where
  query ::
    (MonadReader Config m, MonadState c m, MonadError String m, MonadIO m)
    => [InfoKey c] -> m c
  query xs = do
    cached <- M.filterWithKey (\x _ -> x `elem` xs) <$> get
    -- I can use just 'search' below, but then default implementation will
    -- depend on having 'SwPortInfo' defined. That's probably wrong, so i use
    -- concrete 'searchPorts' function.
    updated <- fromSwPortInfo <$> searchPorts (getSwPorts cached)
    liftIO $ print "updated:"
    liftIO $ print updated
    -- Always use 'unionWith (<>)', because left-biased 'union', which is used
    -- in 'Monoid Map' instance, does not merge map data elements.
    modify (M.unionWith (<>) updated)
    -- Search info in /merged/ db, not just updated "portion", because update
    -- may not have some data, which was present in cached version.
    found <- M.filterWithKey (\x _ -> x `elem` xs && x `elem` M.keys updated) <$> get
    let xs' = xs \\ M.keys found
    liftIO $ print "Found in cache: "
    liftIO $ print found
    liftIO $ print $ "Yet to query: " ++ show xs'
    queried <- search xs'
    modify (M.unionWith (<>) queried)
    let found' = M.filterWithKey (\x _ -> x `elem` xs) queried
    return (M.unionWith (<>) found found')

instance InfoDb SwPortInfo where
  query swPorts = do
    queried <- search swPorts
    modify (M.unionWith (<>) queried)
    return queried

instance InfoDb MacInfo where

instance InfoDb IPInfo where

