{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module BH.Main (
  module BH.Main.Types,
  queryPort,
  queryPorts,
  queryMac,
  queryMacs,
  queryIP,
  queryIPs,
)
where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe

import BH.Main.Types
import BH.Common
import BH.IP
import BH.IP.Arp
import BH.Switch
import BH.Switch.Cisco

import qualified BH.Telnet2 as T2

-- | Query several ports.
queryPorts ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  [SwPort] ->
  m SwPortInfo
queryPorts switches = do
  Config{..} <- ask
  portMacs <- flip runReaderT swInfo
    $ T2.runOn getPorts (map portSw switches) queryPorts'
  liftIO $ putStrLn "Gathered ac map:"
  liftIO $ print portMacs
  liftIO $ putStrLn "Finally, ips..."
  return (resolveIPs2 macIpMap portMacs)
 where
  getPorts :: SwName -> [PortNum]
  getPorts sn = map portSpec . filter ((== sn) . portSw) $ switches
  queryPorts' :: T2.TelnetRunM TelnetParserResult [PortNum] SwPortInfo ()
  queryPorts' = do
    portSw <- asks (swName . T2.switchData)
    ports  <- asks T2.telnetIn
    T2.sendCmd (T2.cmd "terminal length 0")
    res <- M.mapKeys (\p -> SwPort{portSpec = p, ..}) <$> findPortInfo ports
    T2.putResult res
    T2.sendExit

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
  macPorts <- flip runReaderT swInfo $ T2.runTill getMacs queryMacs'
  return (M.map (resolveIPs2 macIpMap) macPorts)
 where
  getMacs :: M.Map MacAddr SwPortInfo -> Maybe [MacAddr]
  getMacs res
    | res == M.empty = Just macs
    | otherwise = case M.keys . M.filter (== M.empty) $ res of
                    [] -> Nothing
                    xs -> Just xs
  queryMacs' :: T2.TelnetRunM TelnetParserResult [MacAddr] (M.Map MacAddr SwPortInfo) ()
  queryMacs' = do
    portSw <- asks (swName . T2.switchData)
    macs  <- asks T2.telnetIn
    res <- M.map (M.mapKeys (\p -> SwPort{portSpec = p, ..})) <$> findMacsPort macs
    T2.putResult res
    T2.sendExit

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

