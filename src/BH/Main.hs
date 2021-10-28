{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BH.Main (
  module BH.Main.Types,
  readSwInfo,
  searchPorts2,
  searchMacs2,
  searchIPs2,
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

import BH.Main.Types
import BH.Common
import BH.IP
import BH.Switch
import BH.Switch.Cisco
import BH.Cache
import BH.Telnet
import BH.IP.Arp

modPort2' :: (forall a. ModPort a
              => SwPort -- ^ FIXME: This should be selector. I.e. (SwPort, PortState) .
              -> a
              -> a) -- ^ modifies
      -> SwPort -- ^ selects. FIXME: This should be complete reference, i.e. (SwPort, PortState)
      -> PortState -- ^ selects
      -> S.Set MacAddr -- ^ selects references
      -> (IPInfo, MacInfo, SwPortInfo) -> (IPInfo, MacInfo, SwPortInfo)
modPort2' k port st macs z@(ipInfo, macInfo, swPortInfo) =
  let macSwPort = Just (port, st)
      macInfo' = S.foldr (insertAdjust (modifyL macSwPortL (k port)) MacData{macIPs = M.empty, ..}) macInfo macs
      xs = M.map macIPs . M.filterWithKey (const . (`S.member` macs)) $ macInfo
      ipInfo' = M.foldrWithKey
        (\mac -> flip $ M.foldrWithKey
          (\ip _ z ->
            M.adjust (modifyL ipMacPortsL (insertAdjust (k port) macSwPort mac)) ip z
          )
        )
        ipInfo xs
  in  ( ipInfo'
      , macInfo'
      , k port swPortInfo
      )

modPort2 :: (forall a. ModPort a => SwPort -> a -> a) -- ^ modifies
      -> SwPort -- ^ selects
      -> (IPInfo, MacInfo, SwPortInfo) -> (IPInfo, MacInfo, SwPortInfo)
modPort2 k port z@(_, _, swPortInfo) =
  let allMacs = fromMaybe S.empty (M.keysSet . portAddrs <$> M.lookup port swPortInfo)
  in  modPort2' k port Up allMacs z

addPortMac :: SwPort -> (PortState, S.Set MacAddr) -> (IPInfo, MacInfo, SwPortInfo) -> (IPInfo, MacInfo, SwPortInfo)
addPortMac port (portState, macs) z@(_, macInfo, swPortInfo) =
  let portAddrs = M.fromSet (\m -> fromMaybe M.empty (macIPs <$> M.lookup m macInfo)) macs
  in  modPort2' (addPort PortData{..}) port portState macs . modPort2' (delPort remMacs) port portState remMacs $ z
 where
  remMacs =
    let oldMacs = fromMaybe S.empty (M.keysSet . portAddrs <$> M.lookup port swPortInfo)
    in  oldMacs `S.difference` macs

mergePorts :: M.Map SwPort (PortState, S.Set MacAddr) -> (IPInfo, MacInfo, SwPortInfo) -> (IPInfo, MacInfo, SwPortInfo)
mergePorts = flip (M.foldrWithKey addPortMac)

modMac' :: (forall a. ModMac a
              => MacAddr
              -> a
              -> a) -- ^ modifies
      -> MacAddr -- ^ selects
      -> (S.Set IP, Maybe (SwPort, PortState)) -- ^ selects references
      -> (IPInfo, MacInfo, SwPortInfo) -> (IPInfo, MacInfo, SwPortInfo)
modMac' k mac (ips0, mp) z@(ipInfo, macInfo, swPortInfo) =
  -- FIXME: Default value hardcoded.
  let MacData{..} = fromMaybe (MacData{macIPs = M.empty, macSwPort = Nothing})
                      $ M.lookup mac macInfo
      ips = if isJust mp then M.keysSet macIPs `S.union` ips0 else ips0
      ipMacPorts = M.singleton mac mp
      -- I may bind 'MacAddr' to new 'IP'-s here (not yet bound). But i'll not
      -- add these new 'IP's to 'MacData': this should be done in 'k'
      -- callback, because i just call 'k' for entire 'macInfo'.
      -- FIXME: Hardcoded default 'IPState' 'Answering'. [current]
      -- FIXME: Hardcoded default 'PortState' 'Up'. [current]
      ipInfo' = foldr
        (\ip -> let ipState = fromMaybe Answering (M.lookup ip macIPs)
                in  insertAdjust (modifyL ipMacPortsL (k mac)) IPData{..} ip)
        ipInfo
        ips
      portState = fromMaybe Up (snd <$> mp)
      portAddrs = M.singleton mac macIPs
      swPortInfo' =
        maybe id
            (insertAdjust (modifyL portAddrsL (k mac)) PortData{..})
            (fst <$> mp)
          $ swPortInfo
  in  ( ipInfo'
      , k mac macInfo
      , swPortInfo'
      )

modMac :: (forall a. ModMac a
              => MacAddr
              -> a
              -> a) -- ^ modifies
      -> MacAddr -- ^ selects
      -> (IPInfo, MacInfo, SwPortInfo) -> (IPInfo, MacInfo, SwPortInfo)
modMac k mac z@(_, macInfo, _) =
  -- FIXME: Default value hardcoded.
  let MacData{..} = fromMaybe (MacData{macIPs = M.empty, macSwPort = Nothing}) (M.lookup mac macInfo)
  in  modMac' k mac (M.keysSet macIPs, macSwPort) z

addMacPort :: MacAddr -> (SwPort, PortState) -> (IPInfo, MacInfo, SwPortInfo) -> (IPInfo, MacInfo, SwPortInfo)
addMacPort mac p z@(_, macInfo, _) =
  let MacData{..} = fromMaybe (MacData{macIPs = M.empty, macSwPort = Just p}) $ M.lookup mac macInfo
      remPort = do
        oldPort <- fst <$> macSwPort
        if fst p /= oldPort
          then macSwPort
          else Nothing
  in  modMac' (addMac MacData{macSwPort = Just p, ..}) mac (S.empty, Just p) . modMac' (delMac (S.empty, remPort)) mac (S.empty, remPort) $ z

mergeMacs :: M.Map MacAddr (SwPort, PortState) -> (IPInfo, MacInfo, SwPortInfo) -> (IPInfo, MacInfo, SwPortInfo)
mergeMacs = flip (M.foldrWithKey addMacPort)

-- FIXME: I should take 'IPState' as argument.
modIP' :: (forall a. ModIP a => IP -> a -> a) -- ^ modifies
      -> IP -- ^ selects
      -> S.Set MacAddr -- ^ selects references
      -> (IPInfo, MacInfo, SwPortInfo) -> (IPInfo, MacInfo, SwPortInfo)
modIP' k ip macs z@(ipInfo, macInfo, swPortInfo) =
  let -- FIXME: Wrong! Values should be taken from current 'IPData', and if not found,
      -- should default to 'Answering' and 'Nothing'. But only if not found!
      -- [current]
      -- FIXME: References must be limited to existing references, if element
      -- exist. Or new references should be added for missing elements
      -- /correctly/ [current].
      macIPs = M.singleton ip Answering
      macInfo' = S.foldr
        (insertAdjust (modifyL macIPsL (k ip)) MacData{macSwPort = Nothing, ..})
        macInfo macs
      xs = M.map macSwPort . M.filterWithKey (const . (`S.member` macs)) $ macInfo
      swPortInfo' = M.foldrWithKey
        (\mac mp z -> case mp of
          Just (port, _)  -> M.adjust (modifyL portAddrsL (insertAdjust (k ip) macIPs mac)) port z
          Nothing         -> z
        )
        swPortInfo xs
  in  ( k ip ipInfo
      , macInfo'
      , swPortInfo'
      )

-- | Selects IP with all macs.
modIP :: (forall a. ModIP a => IP -> a -> a) -- ^ modifies
      -> IP -- ^ selects
      -> (IPInfo, MacInfo, SwPortInfo) -> (IPInfo, MacInfo, SwPortInfo)
modIP f ip z@(ipInfo, _, _) =
  let allMacs = fromMaybe S.empty (M.keysSet . ipMacPorts <$> M.lookup ip ipInfo)
  in  modIP' f ip allMacs z

addIPMac :: IP -> S.Set MacAddr -> (IPInfo, MacInfo, SwPortInfo) -> (IPInfo, MacInfo, SwPortInfo)
addIPMac ip macs z@(ipInfo, macInfo, _) =
  let ipMacPorts = M.fromSet (\m -> M.lookup m macInfo >>= macSwPort) macs
      ipState = Answering
  in  modIP' (addIP IPData{..}) ip macs . modIP' (delIP remMacs) ip remMacs $ z
 where
  remMacs =
    let oldMacs = fromMaybe S.empty (M.keysSet . ipMacPorts <$> M.lookup ip ipInfo)
    in  oldMacs `S.difference` macs

mergeIP2 :: M.Map IP (S.Set MacAddr) -> (IPInfo, MacInfo, SwPortInfo) -> (IPInfo, MacInfo, SwPortInfo)
mergeIP2 xs z@(ipInfo, _, _) = M.foldrWithKey addIPMac z xs

-- FIXME: Split removal of missed IPs into separate function. This should be
-- done by default. [current]
mergeIP :: M.Map IP (S.Set MacAddr) -> (IPInfo, MacInfo, SwPortInfo) -> (IPInfo, MacInfo, SwPortInfo)
mergeIP xs z@(ipInfo, _, _) =
  let remIPs = M.keysSet ipInfo `S.difference` M.keysSet xs
  in  flip (M.foldrWithKey addIPMac) xs . flip (foldr (modIP (setIPState Unreachable))) remIPs $ z

-- FIXME: This function should remove IPs, whose all macs are 'Unreachable'
-- and whose do not have any port defined.
-- FIXME: Add function to remove Macs, which does not have IPs and undefined
-- port.
-- FIXME: Unreachable IPs may be on 'Up' ports, because this simply means,
-- that this IP is no longer used by this server. Though, 'Disabled' or
-- 'NotConnect' ports can't have 'Answering' IPs.
dbTidy :: (IPInfo, MacInfo, SwPortInfo) -> (IPInfo, MacInfo, SwPortInfo)
dbTidy = undefined

-- FIXME: Use generic yaml reading func.
readSwInfo :: (MonadIO m, MonadError String m) => FilePath -> m SwInfo
readSwInfo file = toSwInfo <$> readYaml file
 where
  toSwInfo :: [SwData] -> SwInfo
  toSwInfo = M.fromList . map (\x -> (swName x, x))

-- | This is not really "search", but just quering info about ports. It names
-- this way just to make show its relevant to 'searchX' family of functions.
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

-- TODO: I may use hash to determine changed db file. And then treat /that/
-- file as source and generate others from it.
-- FIXME: I should obtain full info. Here or not, but query over mac implies
-- full result, including both port and IP. And if some part is missing, i
-- should query it as well. [current]
-- | Search mac on all switches.
searchMacs2 ::
  (MonadReader Config m, MonadError String m, MonadIO m, MonadState (IPInfo, MacInfo, SwPortInfo) m) =>
  [MacAddr] ->
  m ()
searchMacs2 macs = do
  Config{..} <- ask
  f <- mergeMacs <$> runReaderT (runTill maybeMacs go) swInfo
  modify f
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

-- | Call "nmap" on specified host for building 'MacIpMap' and 'IpMacMap'.
nmapCache2 :: (MonadIO m, MonadError String m) => T.Text -> m (M.Map IP (S.Set MacAddr))
nmapCache2 host = Sh.shelly (Sh.silently go) >>= liftEither
 where
  go :: Sh.Sh (Either String (M.Map IP (S.Set MacAddr)))
  go = do
    liftIO $ putStrLn "Updating arp cache using `nmap` 2..."
    xml <- do
      --Sh.run_ "ssh" (host : T.words "nmap -sn -PR -oX nmap_arp_cache.xml 213.108.248.0/21")
      Sh.run "ssh" (host : T.words "cat ./nmap_arp_cache.xml")
    z <- liftIO . evaluate . force $ parseNmapXml2 xml
    liftIO $ print z
    --z2 <- liftIO . evaluate . force $ parseNmapXml2 xml
    --void $ Sh.run "ssh" (host : T.words "rm ./nmap_arp_cache.xml")
    return z

nmapCache3 :: (MonadIO m, MonadError String m) => [IP] -> T.Text -> m (M.Map IP (S.Set MacAddr))
nmapCache3 ips0 host = Sh.shelly (Sh.silently go) >>= liftEither
 where
  go :: Sh.Sh (Either String (M.Map IP (S.Set MacAddr)))
  go = do
    liftIO $ putStrLn "Updating arp cache using `nmap` 2..."
    let ips | ips0 == []  = ["213.108.248.0/21"]
            | otherwise   = map (T.pack . showIP) ips0
    xml <- do
      Sh.run_ "ssh" (host : T.words "nmap -sn -PR -oX nmap_arp_cache.xml" ++ ips)
      Sh.run "ssh" (host : T.words "cat ./nmap_arp_cache.xml")
    z <- liftIO . evaluate . force $ parseNmapXml2 xml
    liftIO $ print z
    --z2 <- liftIO . evaluate . force $ parseNmapXml2 xml
    --void $ Sh.run "ssh" (host : T.words "rm ./nmap_arp_cache.xml")
    return z

searchIPs2 ::
  (MonadReader Config m, MonadError String m, MonadIO m, MonadState (IPInfo, MacInfo, SwPortInfo) m) =>
  [IP] ->
  m ()
searchIPs2 ips = do
  Config{..} <- ask
  f <- mergeIP2 <$> nmapCache3 ips nmapHost
  modify f

-- FIXME: Can i generalize following 'readCache' and 'updateArpCache'
-- functions to just read _any_ yaml cache and update it upon some conditions?
-- This cache may include regular ansible invetory. Also, replace 'readSwInfo'
-- with generic version of this. [cache]

-- FIXME: Do not query all IPs at once. I don't need this, really. I may just
-- nmap single IP in question and do all the other stuff with it. [current]
-- FIXME: Host should be obtained from 'Config'.
queryLinuxArp2 ::
  (MonadIO m, MonadError String m, MonadReader Config m, MonadState (IPInfo, MacInfo, SwPortInfo) m) =>
  m ()
queryLinuxArp2 = do
  Config{..} <- ask
  t <- liftIO getCurrentTime
  if diffUTCTime t cacheTime > updateInterval
    then do
      mergeIP <$> nmapCache2 nmapHost <*> get >>= put
      liftIO (writeFile timeFile (show t))
    else return ()

-- FIXME: Make a newtype wrapper around (IPInfo, MacIpMap, SwPortInfo) ?
-- And then add function for obtaining 'InfoKey' indexed map from a generic db
-- type [current]

