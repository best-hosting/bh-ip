{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module BH.Main (
  module BH.Main.Types,
  readSwInfo,
  queryPorts,
  searchMacs,
  verifyMacInfo,
  queryMacs,
  queryIPs,
)
where

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

-- FIXME: In fact, in all queryX functions i need unique items. May be change
-- type to 'S.Set' to force uniqueness?
-- | Query several ports.
queryPorts ::
  (MonadReader Config m, MonadState SwPortInfo m, MonadError String m, MonadIO m) =>
  [SwPort] -> m SwPortInfo
queryPorts swPorts = do
  Config{..} <- ask
  queried <- searchPorts swPorts
  modify (queried <>)
  return queried

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

-- | Query ports, where macs from 'MacInfo' where found and build new
-- (updated) 'MacInfo'.
verifyMacInfo ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  MacInfo -> m MacInfo
verifyMacInfo mInfo = do
  Config{..} <- ask
  let swPorts = nub . concatMap (S.toList . macSwPorts) . M.elems $ mInfo
  swPortInfoToMacInfo <$> searchPorts swPorts

verifyIPInfo ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  IPInfo -> m IPInfo
verifyIPInfo ipInfo = do
  Config{..} <- ask
  let swPorts = nub . concatMap (S.toList . ipSwPorts) . M.elems $ ipInfo
  swPortInfoToIPInfo <$> searchPorts swPorts

-- | Query mac address in 'MacInfo' (presumably obtained from cache) and
-- update info, if found. Otherwise search for mac address.
queryMacs ::
  (MonadReader Config m, MonadState MacInfo m, MonadError String m, MonadIO m)
  => [MacAddr]
  -> m MacInfo
queryMacs macs0 = do
  -- TODO: Can this pattern be generalized?
  Config{..} <- ask
  updated <- get >>= verifyMacInfo . M.filterWithKey (\m _ -> m `elem` macs0)
  modify (updated <>)
  let found = M.filterWithKey (\m _ -> m `elem` macs0) updated
      macs1 = macs0 \\ M.keys found
  liftIO $ print "Found in cache: "
  liftIO $ print found
  liftIO $ print $ "Yet to query: " ++ show macs1
  queried <- searchMacs macs1
  modify (queried <>)
  return (M.unionWith (<>) found queried)

-- FIXME: Use 'verify :: \[SwPort\] -> SwPortInfo' ? Or just use 'queryPorts'
-- instead in 'verifyMacInfo'? Can i merge 'verifyMacInfo' and 'verifyIPInfo'?
-- [current]
-- FIXME: 'queryIPs' and 'queryMacs' seems identical. Can i generalize them?
queryIPs ::
  (MonadReader Config m, MonadState IPInfo m, MonadError String m, MonadIO m)
  => [IP]
  -> m IPInfo
queryIPs ips0 = do
  -- TODO: Can this pattern be generalized?
  Config{..} <- ask
  updated <- get >>= verifyIPInfo . M.filterWithKey (\i _ -> i `elem` ips0)
  modify (updated <>)
  let found = M.filterWithKey (\i _ -> i `elem` ips0) updated
      ips1 = ips0 \\ M.keys found
  liftIO $ print "Found in cache: "
  liftIO $ print found
  liftIO $ print $ "Yet to query: " ++ show ips1
  queried <- searchIPs ips1
  modify (queried <>)
  return (M.unionWith (<>) found (M.filterWithKey (\i _ -> i `elem` ips1) queried))

