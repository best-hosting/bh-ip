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
  queried <- fmap (resolvePortIPs macIpMap) . flip runReaderT swInfo $ runOn onPorts swNames go
  modify (queried <>)
  return queried
 where
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


queryMacs3 ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  [MacAddr] ->
  m (M.Map MacAddr SwPortInfo)
queryMacs3 macs = do
  Config{..} <- ask
  macPorts <- flip runReaderT swInfo $ runTill maybeMacs go
  return (M.map (resolvePortIPs macIpMap) macPorts)
 where
  maybeMacs :: M.Map MacAddr SwPortInfo -> Maybe [MacAddr]
  maybeMacs res = let found = M.keys . M.filter (/= M.empty) $ res
                in  case filter (`notElem` found) macs of
                      [] -> Nothing
                      xs -> Just xs
  go :: TelnetRunM TelnetParserResult [MacAddr] (M.Map MacAddr SwPortInfo) ()
  go = do
    portSw <- asks (swName . switchData)
    ms  <- asks telnetIn
    res <- M.map (M.mapKeys (\p -> SwPort{portSpec = p, ..})) <$> findMacsPort ms
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

-- FIXME: Use 'verify :: [SwPort] -> SwPortInfo' ? Or just use 'queryPorts'
-- instead?
-- | Query ports, where macs from 'MacInfo' where found and build new
-- (updated) 'MacInfo'.
verifyMacInfo ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  MacInfo -> m MacInfo
verifyMacInfo mInfo = do
  Config{..} <- ask
  flip runReaderT swInfo $ runOn onPorts swNames go
 where
  swPorts :: [SwPort]
  swPorts = nub . concatMap (S.toList . macSwPorts) . M.elems $ mInfo
  -- FIXME: Change input to (S.Set PortNum). But for this to have any sense, i
  -- need to change it everywhere, includeing findX funcs.. [refactor]
  onPorts :: SwName -> [PortNum]
  onPorts sn = nub . map portSpec . filter ((== sn) . portSw) $ swPorts
  swNames :: [SwName]
  swNames = nub . map portSw $ swPorts
  go :: TelnetRunM TelnetParserResult [PortNum] MacInfo ()
  go = do
    portSw <- asks (swName . switchData)
    ps <- asks telnetIn
    res <- swPortInfoToMacInfo . M.mapKeys (\p -> SwPort{portSpec = p, ..})
      <$> findPortInfo ps
    putResult res
    sendExit

verifyIPInfo ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  IPInfo -> m IPInfo
verifyIPInfo ipInfo = do
  Config{..} <- ask
  flip runReaderT swInfo $ runOn onPorts swNames (go macIpMap)
 where
  swPorts :: [SwPort]
  swPorts = nub . concatMap (S.toList . ipSwPorts) . M.elems $ ipInfo
  -- FIXME: Change input to (S.Set PortNum). But for this to have any sense, i
  -- need to change it everywhere, includeing findX funcs.. [refactor]
  onPorts :: SwName -> [PortNum]
  onPorts sn = nub . map portSpec . filter ((== sn) . portSw) $ swPorts
  swNames :: [SwName]
  swNames = nub . map portSw $ swPorts
  go :: MacIpMap -> TelnetRunM TelnetParserResult [PortNum] IPInfo ()
  go macIpMap = do
    portSw <- asks (swName . switchData)
    ps <- asks telnetIn
    res <- swPortInfoToIPInfo . resolvePortIPs macIpMap . M.mapKeys (\p -> SwPort{portSpec = p, ..})
      <$> findPortInfo ps
    putResult res
    sendExit

-- | Query mac address in 'MacInfo' (presumably obtained from cache) and
-- update info, if found. Otherwise search for mac address.
queryMacs ::
  (MonadReader Config m, MonadState MacInfo m, MonadError String m, MonadIO m)
  => [MacAddr]
  -> m MacInfo
queryMacs macs0 = do
  -- TODO: Can this pattern be generalized?
  Config{..} <- ask
  updated <- get >>=
    fmap (resolveMacIPs macIpMap) . verifyMacInfo . M.filterWithKey (\m _ -> m `elem` macs0)
  modify (updated <>)
  let found = M.filterWithKey (\m _ -> m `elem` macs0) updated
      macs1 = macs0 \\ M.keys found
  liftIO $ print "Found in cache: "
  liftIO $ print found
  liftIO $ print $ "Yet to query: " ++ show macs1
  queried <- searchMacs macs1
  modify (queried <>)
  return (M.unionWith (<>) found queried)

queryIPs3 ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  [IP] ->
  m (M.Map IP SwPortInfo)
queryIPs3 ips = do
  Config{..} <- ask
  let macs = mapMaybe (flip M.lookup ipMacMap) ips
  macPorts <- queryMacs3 macs
  let go :: IP -> M.Map IP SwPortInfo -> M.Map IP SwPortInfo
      go ip acc = flip (M.insert ip) acc . fromMaybe mempty $ do
        mac <- M.lookup ip ipMacMap
        M.lookup mac macPorts
  return (foldr go mempty ips)

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

