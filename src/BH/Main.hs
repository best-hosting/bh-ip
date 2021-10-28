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

import BH.Main.Types
import BH.Common
import BH.IP
import BH.Switch
import BH.Switch.Cisco
import BH.Cache
import BH.Telnet
import BH.IP.Arp

modPort' :: (forall a. ModPort a
              => Port -- ^ FIXME: This should be selector. I.e. (Port, PortState) .
              -> a
              -> a) -- ^ modifies
      -> Port -- ^ selects. FIXME: This should be complete reference, i.e. (Port, PortState)
      -> PortState -- ^ selects
      -> S.Set MacAddr -- ^ selects references
      -> (IPInfo, MacInfo, PortInfo) -> (IPInfo, MacInfo, PortInfo)
modPort' k port st macs z@(ipInfo, macInfo, swPortInfo) =
  let macPort = Just (port, st)
      macInfo' = S.foldr (insertAdjust (modifyL macPortL (k port)) MacData{macIPs = M.empty, ..}) macInfo macs
      xs = M.map macIPs . M.filterWithKey (const . (`S.member` macs)) $ macInfo
      ipInfo' = M.foldrWithKey
        (\mac -> flip $ M.foldrWithKey
          (\ip _ z ->
            M.adjust (modifyL ipMacPortsL (insertAdjust (k port) macPort mac)) ip z
          )
        )
        ipInfo xs
  in  ( ipInfo'
      , macInfo'
      , k port swPortInfo
      )

modPort :: (forall a. ModPort a => Port -> a -> a) -- ^ modifies
      -> Port -- ^ selects
      -> (IPInfo, MacInfo, PortInfo) -> (IPInfo, MacInfo, PortInfo)
modPort k port z@(_, _, swPortInfo) =
  let allMacs = fromMaybe S.empty (M.keysSet . portAddrs <$> M.lookup port swPortInfo)
  in  modPort' k port Up allMacs z

addPortMac :: Port -> (PortState, S.Set MacAddr) -> (IPInfo, MacInfo, PortInfo) -> (IPInfo, MacInfo, PortInfo)
addPortMac port (portState, macs) z@(_, macInfo, swPortInfo) =
  let portAddrs = M.fromSet (\m -> fromMaybe M.empty (macIPs <$> M.lookup m macInfo)) macs
  in  modPort' (addPort PortData{..}) port portState macs . modPort' (delPort remMacs) port portState remMacs $ z
 where
  remMacs =
    let oldMacs = fromMaybe S.empty (M.keysSet . portAddrs <$> M.lookup port swPortInfo)
    in  oldMacs `S.difference` macs

modMac' :: (forall a. ModMac a
              => MacAddr
              -> a
              -> a) -- ^ modifies
      -> MacAddr -- ^ selects
      -> (S.Set IP, Maybe (Port, PortState)) -- ^ selects references
      -- FIXME: If current operation if addition and 'IPData' will contain IP
      -- in non-default (not 'Answering') state result the db become
      -- inconsistent, because here, when adding 'IP' to references i'll
      -- _assume_, that state is 'Answering'. Thus, i should provide _all_
      -- info required for constructing references. [current]
      -> (IPInfo, MacInfo, PortInfo) -> (IPInfo, MacInfo, PortInfo)
modMac' k mac (ips0, mp) z@(ipInfo, macInfo, swPortInfo) =
  -- FIXME: Default value hardcoded.
  let MacData{..} = fromMaybe (MacData{macIPs = M.empty, macPort = Nothing})
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
      -> (IPInfo, MacInfo, PortInfo) -> (IPInfo, MacInfo, PortInfo)
modMac k mac z@(_, macInfo, _) =
  -- FIXME: Default value hardcoded.
  let MacData{..} = fromMaybe (MacData{macIPs = M.empty, macPort = Nothing}) (M.lookup mac macInfo)
  in  modMac' k mac (M.keysSet macIPs, macPort) z

addMacPort :: MacAddr -> (Port, PortState) -> (IPInfo, MacInfo, PortInfo) -> (IPInfo, MacInfo, PortInfo)
addMacPort mac p z@(_, macInfo, _) =
  let MacData{..} = fromMaybe (MacData{macIPs = M.empty, macPort = Just p}) $ M.lookup mac macInfo
      remPort = do
        oldPort <- fst <$> macPort
        if fst p /= oldPort
          then macPort
          else Nothing
  in  modMac' (addMac MacData{macPort = Just p, ..}) mac (S.empty, Just p) . modMac' (delMac (S.empty, remPort)) mac (S.empty, remPort) $ z

mergeMacs :: M.Map MacAddr (Port, PortState) -> (IPInfo, MacInfo, PortInfo) -> (IPInfo, MacInfo, PortInfo)
mergeMacs = flip (M.foldrWithKey addMacPort)

-- FIXME: I should take 'IPState' as argument.
modIP' :: (forall a. ModIP a => IP -> a -> a) -- ^ modifies
      -> IP -- ^ selects
      -> S.Set MacAddr -- ^ selects references
      -> (IPInfo, MacInfo, PortInfo) -> (IPInfo, MacInfo, PortInfo)
modIP' k ip macs z@(ipInfo, macInfo, swPortInfo) =
  let -- FIXME: Wrong! Values should be taken from current 'IPData', and if not found,
      -- should default to 'Answering' and 'Nothing'. But only if not found!
      -- [current]
      -- FIXME: References must be limited to existing references, if element
      -- exist. Or new references should be added for missing elements
      -- /correctly/ [current].
      macIPs = M.singleton ip Answering
      macInfo' = S.foldr
        (insertAdjust (modifyL macIPsL (k ip)) MacData{macPort = Nothing, ..})
        macInfo macs
      xs = M.map macPort . M.filterWithKey (const . (`S.member` macs)) $ macInfo
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
      -> (IPInfo, MacInfo, PortInfo) -> (IPInfo, MacInfo, PortInfo)
modIP f ip z@(ipInfo, _, _) =
  let allMacs = fromMaybe S.empty (M.keysSet . ipMacPorts <$> M.lookup ip ipInfo)
  in  modIP' f ip allMacs z

addIPMac :: IP -> S.Set MacAddr -> (IPInfo, MacInfo, PortInfo) -> (IPInfo, MacInfo, PortInfo)
addIPMac ip macs z@(ipInfo, macInfo, _) =
  let ipMacPorts = M.fromSet (\m -> M.lookup m macInfo >>= macPort) macs
      ipState = Answering
  in  modIP' (addIP IPData{..}) ip macs . modIP' (delIP remMacs) ip remMacs $ z
 where
  remMacs =
    let oldMacs = fromMaybe S.empty (M.keysSet . ipMacPorts <$> M.lookup ip ipInfo)
    in  oldMacs `S.difference` macs

-- FIXME: Split removal of missed IPs into separate function. This should be
-- done by default. [current]
mergeIP :: M.Map IP (S.Set MacAddr) -> (IPInfo, MacInfo, PortInfo) -> (IPInfo, MacInfo, PortInfo)
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
  modify (flip (M.foldrWithKey addPortMac) xs)
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
  modify (flip (M.foldrWithKey addMacPort) xs)
 where
  -- FIXME: Hardcoded vlan! [current]
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
    liftIO $ putStrLn "Updating arp cache using `nmap` 2..."
    xml <- do
      Sh.run_ "ssh" (host : T.words "nmap -sn -PR -oX nmap_arp_cache.xml" ++ argv)
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
  case ips of
    [] -> do
      t <- liftIO getCurrentTime
      if diffUTCTime t cacheTime > updateInterval
        then do
          xs <- findIPInfo (Left ["213.108.248.0/21"]) nmapHost
          modify (flip (M.foldrWithKey addIPMac) xs)
        else return ()
    _ ->  do
      xs <- findIPInfo (Left ["213.108.248.0/21"]) nmapHost
      modify (flip (M.foldrWithKey addIPMac) xs)

-- FIXME: Do not query all IPs at once. I don't need this, really. I may just
-- nmap single IP in question and do all the other stuff with it. [current]
-- FIXME: Host should be obtained from 'Config'.
-- FIXME: I may replace this function with just quering all relevant IPs/macs
-- at each run. Though, when doing search by mac/port i can't be sure, that
-- no new IPs are used there. Thus.. i still need to query all IPs, which may
-- be time-consuming. So, cache update time (used here) may be still relevant.
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

-- FIXME: Make a newtype wrapper around (IPInfo, MacIpMap, PortInfo) ?
-- And then add function for obtaining 'InfoKey' indexed map from a generic db
-- type [current]

