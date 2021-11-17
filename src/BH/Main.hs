{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module BH.Main (
  module BH.Main.Types,
  readSwInfo,
  searchPorts,
  searchMacs,
  searchIPs,
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
import qualified Data.Text as T
import qualified Shelly as Sh
import Control.DeepSeq
import Control.Exception
import Data.Time
import Data.Coerce
import Control.Arrow

import BH.Main.Types
import BH.Common
import BH.IP
import BH.Switch
import BH.Switch.Cisco
import BH.Cache
import BH.Telnet
import BH.IP.Arp

addPortMac :: Port -> (PortState, S.Set MacAddr) -> (IPInfo, MacInfo, PortInfo) -> (IPInfo, MacInfo, PortInfo)
addPortMac port (st, macs) z@(_, macInfo, portInfo)
  | st == Disabled  = modPort' (addPort d) port macs z
  | otherwise       = modPort' (addPort d) port macs
                      . modPort' (delPort remMacs) port remMacs
                      $ z
 where
  d :: PortData
  d = let portAddrs = M.fromSet (\m -> maybe M.empty macIPs (M.lookup m macInfo)) macs
          portState = pure st
      in  PortData{..}
  remMacs :: S.Set MacAddr
  remMacs =
    let oldMacs = maybe S.empty (M.keysSet . portAddrs) $ M.lookup port portInfo
    in  oldMacs `S.difference` macs

-- | Add 'IP' and its set of mac addresses. 'IPState' is always 'Answering',
-- because otherwise, how can i know IP macs?
addIPMac :: IP -> S.Set MacAddr -> (IPInfo, MacInfo, PortInfo) -> (IPInfo, MacInfo, PortInfo)
addIPMac ip macs z@(ipInfo, macInfo, _) =
  let ipMacPorts = M.fromSet (\m -> maybe M.empty macPorts (M.lookup m macInfo)) macs
      ipState = pure Answering
  in  modIP' (addIP IPData{..}) ip macs . modIP' (delIP remMacs) ip remMacs $ z
 where
  remMacs =
    let oldMacs = maybe S.empty (M.keysSet . ipMacPorts) $ M.lookup ip ipInfo
    in  oldMacs `S.difference` macs

-- FIXME: Split removal of missed IPs into separate function. This should be
-- done by default. [current]
mergeIP :: M.Map IP (S.Set MacAddr) -> (IPInfo, MacInfo, PortInfo) -> (IPInfo, MacInfo, PortInfo)
mergeIP xs z@(ipInfo, _, _) =
  let remIPs = M.keysSet ipInfo `S.difference` M.keysSet xs
  in  flip (M.foldrWithKey addIPMac) xs . flip (foldr (modIP (setIPState Unreachable))) remIPs $ z

addMacPort :: MacAddr -> (Port, PortState) -> (IPInfo, MacInfo, PortInfo) -> (IPInfo, MacInfo, PortInfo)
addMacPort mac (port, st) z@(_, macInfo, _) =
  -- In this function, unlike 'modPort'' and 'modIP'', i have two kinds of
  -- probably new information: new mac-port relation /and/ new 'PortState'.
  -- And, because 'modMac'' can't update 'PortState' in 'PortData', i should
  -- explicitly update port state using 'modPort''.
  let d = setL macPortsL newMacPorts . fromMaybe mempty $ M.lookup mac macInfo
  in  modMac' (addMac d) mac (S.empty, S.singleton port)
        . modPort (setPortState st) port
        . modMac' (delMac (S.empty, remPorts)) mac (S.empty, remPorts)
        $ z
 where
  newMacPorts :: M.Map Port (First PortState)
  newMacPorts = M.singleton port (pure st)
  remPorts :: S.Set Port
  remPorts =
    let oldMacPorts = maybe M.empty macPorts $ M.lookup mac macInfo
    in  M.keysSet $ M.difference oldMacPorts newMacPorts

mergeMacs :: M.Map MacAddr (Port, PortState) -> (IPInfo, MacInfo, PortInfo) -> (IPInfo, MacInfo, PortInfo)
mergeMacs = flip (M.foldrWithKey addMacPort)

-- FIXME: This function should remove IPs, whose all macs are 'Unreachable'
-- and whose do not have any port defined.
-- FIXME: Add function to remove Macs, which does not have IPs and undefined
-- port.
-- FIXME: Unreachable IPs may be on 'Up' ports, because this simply means,
-- that this IP is no longer used by this server. Though, 'Disabled' or
-- 'NotConnect' ports can't have 'Answering' IPs.
dbTidy :: (IPInfo, MacInfo, PortInfo) -> (IPInfo, MacInfo, PortInfo)
dbTidy = undefined

-- FIXME: Use generic yaml reading func.
readSwInfo :: (MonadIO m, MonadError String m) => FilePath -> m SwInfo
readSwInfo file = toSwInfo <$> readYaml file
 where
  toSwInfo :: [SwData] -> SwInfo
  toSwInfo = M.fromList . map (\x -> (swName x, x))

-- | This is not really "search", but just quering info about ports. It names
-- this way just to make show its relevant to 'searchX' family of functions.
searchPorts ::
  (MonadReader Config m, MonadError String m, MonadIO m, MonadState (IPInfo, MacInfo, PortInfo) m) =>
  [Port] -> m ()
searchPorts swPorts = do
  Config{..} <- ask
  xs <- runReaderT (runOn onPorts swNames go) swInfo
  liftIO $ print "Adding ports:"
  liftIO $ print xs
  modify (flip (M.foldrWithKey addPortMac) xs)
  -- FIXME: Call 'searchMacs' from here for all macs on ports, which new state
  -- is 'Disabled' or 'NotConnect'. [current]
  -- FIXME: Search for all IPs, to ensure, that new macs have up to date info
  -- [current]
 where
  -- FIXME: Change input to (S.Set PortNum). But for this to have any sense, i
  -- need to change it everywhere, includeing findX funcs.. [refactor]
  onPorts :: SwName -> [PortNum]
  onPorts sn = nub . map portSpec . filter ((== sn) . portName) $ swPorts
  swNames :: [SwName]
  swNames = nub . map portName $ swPorts
  go :: TelnetRunM TelnetParserResult [PortNum] (M.Map Port (PortState, S.Set MacAddr)) ()
  go = do
    portName <- asks (swName . switchData)
    ps <- asks telnetIn
    res <- M.mapKeys (\p -> Port{portSpec = p, ..}) <$> findPortInfo ps
    putResult res
    sendExit

-- TODO: Implement second update strategy: just save raw data (the one
-- 'mergeX' funcs accept), apply merges there and rebuild entire db. Then
-- compare the result. They should match..
--
-- TODO: I may use hash to determine changed db file. And then treat /that/
-- file as source and generate others from it.
-- FIXME: I should obtain full info. Here or not, but query over mac implies
-- full result, including both port and IP. And if some part is missing, i
-- should query it as well. [current]
-- | Search mac on all switches.
searchMacs ::
  (MonadReader Config m, MonadError String m, MonadIO m, MonadState (IPInfo, MacInfo, PortInfo) m) =>
  [MacAddr] ->
  m ()
searchMacs macs = do
  Config{..} <- ask
  xs <- runReaderT (runTill maybeMacs go) swInfo
  liftIO $ print "Adding macs:"
  liftIO $ print xs
  modify (flip (M.foldrWithKey addMacPort) xs)
  -- FIXME: If mac was not found, but /is/ present in db, i should remove it,
  -- unless it's port state is 'Disabled' [current]
  -- FIXME: Search for all IPs, to ensure, that new macs have up to date info
  -- [current]
 where
  maybeMacs :: M.Map MacAddr (Port, PortState) -> Maybe [MacAddr]
  maybeMacs res = let found = M.keys res
                  in  case filter (`notElem` found) macs of
                        [] -> Nothing
                        xs -> Just xs
  go :: TelnetRunM TelnetParserResult [MacAddr] (M.Map MacAddr (Port, PortState)) ()
  go = do
    portName <- asks (swName . switchData)
    ms  <- asks telnetIn
    M.map (\p -> (Port{portSpec = p, ..}, Up)) <$> (findMacInfo ms) >>= putResult
    sendExit

-- FIXME: If i'll switch to ip package i may call this with 'Either IP
-- IPRange' or smth. But for now i may just use empty list..
findIPInfo :: (MonadIO m, MonadError String m) => Either [T.Text] [IP] -> T.Text -> m (M.Map IP (S.Set MacAddr))
findIPInfo xs host = Sh.shelly (Sh.silently (go (nmapArgs xs))) >>= liftEither
 where
  nmapArgs :: Either [T.Text] [IP] -> [T.Text]
  nmapArgs (Left nets) = nets
  nmapArgs (Right ips) = map (T.pack . showIP) ips
  go :: [T.Text] -> Sh.Sh (Either String (M.Map IP (S.Set MacAddr)))
  go argv = do
    let cmd = host : T.words "nmap -sn -PR -oX nmap_arp_cache.xml" ++ argv
    liftIO $ putStrLn $ "Updating arp cache using `nmap` with.. " ++ T.unpack (T.unwords cmd)
    xml <- do
      Sh.run_ "ssh" cmd
      Sh.run "ssh" (host : T.words "cat ./nmap_arp_cache.xml")
    z <- liftIO . evaluate . force $ parseNmapXml xml
    liftIO $ print z
    --void $ Sh.run "ssh" (host : T.words "rm ./nmap_arp_cache.xml")
    return z

searchIPs ::
  (MonadReader Config m, MonadError String m, MonadIO m, MonadState (IPInfo, MacInfo, PortInfo) m) =>
  [IP] ->
  m ()
searchIPs ips = do
  Config{..} <- ask
  xs <- case ips of
    [] -> do
      t <- liftIO getCurrentTime
      if diffUTCTime t cacheTime > updateInterval
        then findIPInfo (Left ["213.108.248.0/21"]) nmapHost
        else return M.empty
    _ -> findIPInfo (Right ips) nmapHost
  liftIO $ print "Adding ips:"
  liftIO $ print xs
  modify (flip (M.foldrWithKey addIPMac) xs)
  -- FIXME: First, search for all macs of all IPs (both found and not found)
  -- to update port info. [current]
  -- FIXME: Then for all not found IPs set state to 'Unreachable'.
  -- If port state of 'Unreachable' IP is 'Up', unbind it from mac.
  -- If port state is 'Disabled', leave it as is.
  -- If port is undefined, remove it from db.
  -- [current]

{-queryLinuxArp2 ::
  (MonadIO m, MonadError String m, MonadReader Config m, MonadState (IPInfo, MacInfo, PortInfo) m) =>
  m ()
queryLinuxArp2 = do
  Config{..} <- ask
  t <- liftIO getCurrentTime
  if diffUTCTime t cacheTime > updateInterval
    then do
      mergeIP <$> findIPInfo [] nmapHost <*> get >>= put
      liftIO (writeFile timeFile (show t))
    else return ()-}


