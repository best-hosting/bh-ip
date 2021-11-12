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

modPort' :: (forall a. ModPort a
              => Port
              -> a
              -> a) -- ^ modifies
      -> Port -- ^ selects.
      -> S.Set MacAddr -- ^ selects references
      -> (IPInfo, MacInfo, PortInfo) -> (IPInfo, MacInfo, PortInfo)
modPort' k port macs z@(ipInfo, macInfo, swPortInfo) =
  let -- FIXME: Why i can't just add /any/ 'MacData' and then apply modify
      -- function on top of default value? Then i don't need 'PortState' in
      -- 'modPort' args at all [current]
      -- FIXME: Revert 'Monoid' instance again. After all, it's better to have
      -- 'mempty' defined somewhere, than using some raw values here. Also,
      -- with lawfull 'Monoid' instance i can just user 'M.insertWith' instead
      -- of 'insertAdjust', because (mempty <> x == x <> mempty == x).
      macInfo' = S.foldr (insertAdjust3 (modifyL macPortL (k port))) macInfo macs
      xs = M.map macIPs . M.filterWithKey (const . (`S.member` macs)) $ macInfo
      -- If element (e.g. ip here) does not exist in 'IPInfo' it should be
      -- added as 'member' /without/ any regard to what 'portAddrs' contain.
      -- I.e.  'portAddrs' may incorrectly contain an ip element missed from
      -- 'IPInfo', and this element may have some 'IPState'. But 'portAddrs'
      -- should /not/ be considered as trusted source for creating new
      -- 'IPInfo' element: i should not create new 'IPInfo' element with the
      -- 'IPState' taken from 'portAddrs', instead i should just use 'mempty'.
      -- The reason is such state is already broken db, and elements should
      -- not be created basing on references to them.
      ipInfo' = M.foldrWithKey
        (\mac -> flip $ M.foldrWithKey
          (\ip _ z ->
            M.adjust (modifyL ipMacPortsL (insertAdjust3 (k port) mac)) ip z
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
  in  modPort' k port allMacs z

addPortMac :: Port -> (PortState, S.Set MacAddr) -> (IPInfo, MacInfo, PortInfo) -> (IPInfo, MacInfo, PortInfo)
addPortMac port (st, macs) z@(_, macInfo, swPortInfo) =
  let portAddrs = M.fromSet (\m -> fromMaybe M.empty (macIPs <$> M.lookup m macInfo)) macs
      portState = pure st
  in  modPort' (addPort PortData{..}) port macs . modPort' (delPort remMacs) port remMacs $ z
 where
  remMacs =
    let oldMacs = fromMaybe S.empty (M.keysSet . portAddrs <$> M.lookup port swPortInfo)
    in  oldMacs `S.difference` macs

modMac' :: (forall a. ModMac a
              => MacAddr
              -> a
              -> a) -- ^ modifies
      -> MacAddr -- ^ selects
      -> (S.Set IP, S.Set Port) -- ^ selects references
      -- FIXME: If current operation if addition and 'IPData' will contain IP
      -- in non-default (not 'Answering') state result the db become
      -- inconsistent, because here, when adding 'IP' to references i'll
      -- _assume_, that state is 'Answering'. Thus, i should provide _all_
      -- info required for constructing references. [current]
      -> (IPInfo, MacInfo, PortInfo) -> (IPInfo, MacInfo, PortInfo)
modMac' k mac (ips, ports) z@(ipInfo, macInfo, swPortInfo) =
  -- FIXME: Default value hardcoded.
  let MacData{..} = fromMaybe mempty $ M.lookup mac macInfo
      -- FIXME: 'PortRef' Monoid.. [current]
      -- I may bind 'MacAddr' to new 'IP'-s here (not yet bound). But i'll not
      -- add these new 'IP's to 'MacData': this should be done in 'k'
      -- callback, because i just call 'k' for entire 'macInfo'.
      -- FIXME: Hardcoded default 'IPState' 'Answering'. [current]
      -- FIXME: Hardcoded default 'PortState' 'Up'. [current]
      ipInfo' = foldr (insertAdjust3 (modifyL ipMacPortsL (k mac))) ipInfo ips
      swPortInfo' = foldr (insertAdjust3 (modifyL portAddrsL (k mac))) swPortInfo ports
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
  let MacData{..} = fromMaybe mempty (M.lookup mac macInfo)
  in  modMac' k mac (M.keysSet macIPs, M.keysSet macPort) z

addMacPort :: MacAddr -> (Port, PortState) -> (IPInfo, MacInfo, PortInfo) -> (IPInfo, MacInfo, PortInfo)
addMacPort mac port z@(_, macInfo, _) =
  let d = setL macPortL newMacPorts . fromMaybe mempty $ M.lookup mac macInfo
  in  modMac' (addMac d) mac (S.empty, S.singleton (fst port))
        . modMac' (delMac (S.empty, remPorts)) mac (S.empty, remPorts)
        $ z
 where
  MacData{..} = fromMaybe mempty $ M.lookup mac macInfo
  newMacPorts :: M.Map Port (First PortState)
  newMacPorts = M.fromList . map (second pure) $ [port]
  remPorts :: S.Set Port
  remPorts =
    let f x y = if x /= y then Just x else Nothing
    in  M.keysSet $ M.differenceWith f macPort newMacPorts

mergeMacs :: M.Map MacAddr (Port, PortState) -> (IPInfo, MacInfo, PortInfo) -> (IPInfo, MacInfo, PortInfo)
mergeMacs = flip (M.foldrWithKey addMacPort)

-- FIXME: I may define a typeclass, which abstrects over functions 'modPort',
-- 'modMac', 'modIP' for each respective type - 'IPInfo', 'MacInfo',
-- 'PortInfo'.
-- FIXME: I should take 'IPState' as argument.
-- FIXME: Rename 'swPortInfo' to just 'portInfo'.
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
      macInfo' = S.foldr (insertAdjust3 (modifyL macIPsL (k ip))) macInfo macs
      xs :: M.Map MacAddr (S.Set Port)
      xs = M.map (M.keysSet . macPort) . M.filterWithKey (const . (`S.member` macs)) $ macInfo
      swPortInfo' = M.foldrWithKey
        (\mac -> flip $ foldr (M.adjust (modifyL portAddrsL (insertAdjust3 (k ip) mac))))
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
  let ipMacPorts = M.fromSet (\m -> maybe M.empty macPort (M.lookup m macInfo)) macs
      ipState = pure Answering
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
  modify (flip (M.foldrWithKey addMacPort) xs)
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


