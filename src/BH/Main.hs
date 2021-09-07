{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module BH.Main (
  module BH.Main.Types,
  readSwInfo,
  queryPort,
  queryPorts,
  queryMac,
  queryMacs,
  queryMacs2,
  queryIP,
  queryIPs,
)
where

import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe
import Data.List
import qualified Data.Set as S

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

-- TODO: Query Mac using nmap/ip neigh, if not found.[nmap][arp]
resolveIPs :: MacIpMap -> MacInfo -> MacInfo
resolveIPs macIpMap = M.foldrWithKey go mempty
 where
  go :: MacAddr -> MacData -> MacInfo -> MacInfo
  go mac y z =
    let ips = fromMaybe mempty (M.lookup mac macIpMap)
    in  M.insert mac (setL macIPsL ips y) z

-- FIXME: In fact, in all queryX functions i need unique items. May be change
-- type to 'S.Set' to force uniqueness?
-- | Query several ports.
queryPorts ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  [SwPort] ->
  m SwPortInfo
queryPorts switches = do
  Config{..} <- ask
  portMacs <- flip runReaderT swInfo
    $ runOn ports (nub . map portSw $ switches) queryPorts'
  liftIO $ putStrLn "Gathered ac map:"
  liftIO $ print portMacs
  liftIO $ putStrLn "Finally, ips..."
  return (resolveIPs2 macIpMap portMacs)
 where
  ports :: SwName -> [PortNum]
  ports sn = nub . map portSpec . filter ((== sn) . portSw) $ switches
  queryPorts' :: TelnetRunM TelnetParserResult [PortNum] SwPortInfo ()
  queryPorts' = do
    portSw <- asks (swName . switchData)
    ps  <- asks telnetIn
    sendCmd (cmd "terminal length 0")
    res <- M.mapKeys (\p -> SwPort{portSpec = p, ..}) <$> findPortInfo ps
    putResult res
    sendExit

-- | Query single port.
queryPort ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  SwPort ->
  m SwPortInfo
queryPort = queryPorts . (: [])

queryMac ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  MacAddr ->
  m (M.Map MacAddr SwPortInfo)
queryMac mac = queryMacs [mac]

resolveIPs2 :: MacIpMap -> M.Map a PortData -> M.Map a PortData
resolveIPs2 macIpMap = M.map $ modifyL portAddrsL (resolveIPs macIpMap)

queryMacs ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  [MacAddr] ->
  m (M.Map MacAddr SwPortInfo)
queryMacs macs = do
  Config{..} <- ask
  macPorts <- flip runReaderT swInfo $ runTill maybeMacs queryMacs'
  return (M.map (resolveIPs2 macIpMap) macPorts)
 where
  maybeMacs :: M.Map MacAddr SwPortInfo -> Maybe [MacAddr]
  maybeMacs res = let found = M.keys . M.filter (/= M.empty) $ res
                in  case filter (`notElem` found) macs of
                      [] -> Nothing
                      xs -> Just xs
  queryMacs' :: TelnetRunM TelnetParserResult [MacAddr] (M.Map MacAddr SwPortInfo) ()
  queryMacs' = do
    portSw <- asks (swName . switchData)
    ms  <- asks telnetIn
    res <- M.map (M.mapKeys (\p -> SwPort{portSpec = p, ..})) <$> findMacsPort ms
    putResult res
    sendExit

-- TODO: I may use hash to determine changed db file. And then treat /that/
-- file as source and generate others from it.
queryMacs2 ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  [MacAddr] ->
  m MacInfo
queryMacs2 macs = do
  Config{..} <- ask
  macInfo <- flip runReaderT swInfo $ runTill maybeMacs queryMacs'
  return (M.mapWithKey (\m d -> d{macIPs = fromMaybe mempty (M.lookup m macIpMap)}) macInfo)
 where
  maybeMacs :: MacInfo -> Maybe [MacAddr]
  maybeMacs res = let found = M.keys . M.filter (not . S.null . macSwPorts) $ res
                in  case filter (`notElem` found) macs of
                      [] -> Nothing
                      xs -> Just xs
  queryMacs' :: TelnetRunM TelnetParserResult [MacAddr] MacInfo ()
  queryMacs' = do
    portSw <- asks (swName . switchData)
    ms  <- asks telnetIn
    findMacInfo ms >>= putResult
    sendExit

queryIP ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  IP ->
  m (M.Map IP SwPortInfo)
queryIP = queryIPs . (: [])

queryIPs ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  [IP] ->
  m (M.Map IP SwPortInfo)
queryIPs ips = do
  Config{..} <- ask
  let macs = mapMaybe (flip M.lookup ipMacMap) ips
  macPorts <- queryMacs macs
  let go :: IP -> M.Map IP SwPortInfo -> M.Map IP SwPortInfo
      go ip acc = flip (M.insert ip) acc . fromMaybe mempty $ do
        mac <- M.lookup ip ipMacMap
        M.lookup mac macPorts
  return (foldr go mempty ips)

