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
  -- FIXME: Hardcoded non-trivial logic: state 'Disabled' is in fact
  -- /requires/ empty macs, but this are neither enforced, nor visible from
  -- outside.
  | st == Disabled && macs == S.empty = modPort (setPortState Disabled) port z
  | st == Disabled  = error "State disabled and mac not emtpy? Nihuya sebe, i takoe vozmojno?"
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

-- | Add mac address on certain port. Port state is always 'Up', because
-- otherwise how can i know mac address on that port?
addMacPort :: MacAddr -> Port -> (IPInfo, MacInfo, PortInfo) -> (IPInfo, MacInfo, PortInfo)
addMacPort mac port z@(_, macInfo, _) =
  -- In this function, unlike 'modPort'' and 'modIP'', i have two kinds of
  -- probably new information: new mac-port relation /and/ new 'PortState'.
  -- And, because 'modMac'' can't update 'PortState' in 'PortData', i should
  -- explicitly update port state using 'modPort''.
  modMac' (addMac d) mac (S.empty, S.singleton port)
    . modPort (setPortState Up) port
    . modMac' (delMac (S.empty, remPorts)) mac (S.empty, remPorts)
    $ z
 where
  d :: MacData
  d = let macPorts = M.singleton port (pure Up)
      in  setL macPortsL macPorts . fromMaybe mempty $ M.lookup mac macInfo
  remPorts :: S.Set Port
  remPorts =
    let oldMacPorts = M.keysSet . maybe M.empty macPorts $ M.lookup mac macInfo
    in  S.filter (/= port) oldMacPorts

mergeMacs :: M.Map MacAddr Port -> (IPInfo, MacInfo, PortInfo) -> (IPInfo, MacInfo, PortInfo)
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
searchPorts ports = do
  Config{..} <- ask
  found <- runReaderT (runOn onPorts swNames go) swInfo
  liftIO $ print "Adding ports:"
  liftIO $ print found
  (_, _, portInfo) <- get
  forM_ ports $ \port ->
    case port `M.lookup` found of
      Just (st, macs) -> modify $ addPortMac port (st, macs)
      Nothing         -> modify . fromMaybe id $ do
        PortData{..} <- M.lookup port portInfo
        return (modPort (delPort (M.keysSet portAddrs)) port)
  -- FIXME: Call 'searchMacs' from here for all macs on ports, which new state
  -- is 'Disabled' or 'NotConnect'. [current]
  -- FIXME: Search for all IPs, to ensure, that new macs have up to date info
  -- [current]
 where
  -- FIXME: Change input to (S.Set PortNum). But for this to have any sense, i
  -- need to change it everywhere, includeing findX funcs.. [refactor]
  onPorts :: SwName -> [PortNum]
  onPorts sn = nub . map portSpec . filter ((== sn) . portName) $ ports
  swNames :: [SwName]
  swNames = nub . map portName $ ports
  go :: TelnetRunM TelnetParserResult [PortNum] (M.Map Port (PortState, S.Set MacAddr)) ()
  go = do
    portName <- asks (swName . switchData)
    ps <- asks telnetIn
    res <- M.mapKeys (\p -> Port{portSpec = p, ..}) <$> findPortInfo ps
    putResult res
    sendExit

type Db = (IPInfo, MacInfo, PortInfo)

-- TODO: Can i generalize and merge 'ModX' classes with this one? In fact,
-- this class claims, that i can perform some operations with certain key on
-- particular type 'Db'. With lookup that's pretty simple, but to perform
-- addition or delete, i need to be sure, that all subcomponents of 'Db' type
-- implement some other class ('ModMac' for 'MacAddr' key, 'ModIP' for 'IP'
-- key etc). But for that, i need, to
-- 1. Make generic class 'Mod', which may be parametrized by key (e.g. 'Mod
--    IP' or 'Mod MacAddr').
-- 2. Make instances of this class for 'Db' type subcomponents instead of some
--    /part/ of internal structure (e.g. 'Mod IP PortInfo' instead of 'ModIP
--    (M.Map IP (First IPState))' now). Will this work at all?
-- 3. Parametrize 'Db' components using type family.
class (Eq (DbData a), Monoid (DbData a)) => DbKey a where
  type DbData a
  dbLookup :: a -> Db -> Maybe (DbData a)

instance DbKey MacAddr where
  type DbData MacAddr = MacData
  dbLookup k (_, macInfo, _) = M.lookup k macInfo

instance DbKey IP where
  type DbData IP = IPData
  dbLookup k (ipInfo, _, _) = M.lookup k ipInfo

instance DbKey Port where
  type DbData Port = PortData
  dbLookup k (_, _, portInfo) = M.lookup k portInfo

runIfEmpty :: DbKey k => (k -> Db -> Db) -> k -> Db -> Db
runIfEmpty f k m = fromMaybe m $ do
  d <- dbLookup k m
  if d == mempty then return (f k m) else return m

-- TODO: Implement second update strategy: just save raw data (the one
-- 'mergeX' funcs accept), apply merges there and rebuild entire db. Then
-- compare the result. They should match..
--
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
  found <- runReaderT (runTill maybeMacs go) swInfo
  liftIO $ print "Adding macs:"
  liftIO $ print found
  (_, macInfo, _) <- get
  forM_ macs $ \mac ->
    case mac `M.lookup` found of
      Just port -> modify $ addMacPort mac port
      -- Following is the same `modify(M.filter (== mempty) macInfo)`
      Nothing   -> modify . fromMaybe id $ do -- modify $ runIfEmpty (modMac (delMac (S.empty, S.empty))) mac
        d <- M.lookup mac macInfo
        if d == mempty
          then return (modMac (delMac mempty) mac)
          else return id
  -- FIXME: If mac was not found, but /is/ present in db, i should remove it,
  -- unless it's port state is 'Disabled'. A: Check it, macs with port state
  -- 'Disabled' should have non-empty 'macPorts', but others missed macs will
  -- have empty 'macPorts'. [current]
  -- FIXME: Search for all IPs, to ensure, that new macs have up to date info
  -- [current]
 where
  maybeMacs :: M.Map MacAddr Port -> Maybe [MacAddr]
  maybeMacs res = let found = M.keys res
                  in  case filter (`notElem` found) macs of
                        [] -> Nothing
                        xs -> Just xs
  go :: TelnetRunM TelnetParserResult [MacAddr] (M.Map MacAddr Port) ()
  go = do
    portName <- asks (swName . switchData)
    ms  <- asks telnetIn
    M.map (\portSpec -> Port{..}) <$> findMacInfo ms >>= putResult
    sendExit

{-searchMacsRec :: 
  (MonadReader Config m, MonadError String m, MonadIO m, MonadState (IPInfo, MacInfo, PortInfo) m) =>
  [MacAddr] -> m ()
searchMacsRec macs = do
  (_, macInfo, _) <- get
  case mac `M.lookup` found of
    Just port -> do
      modify (addMacPort mac port)
      --searchIPsNoRec all
    Nothing   -> do
      d@MacData{..} <- M.lookup mac macInfo
      searchIPsNoRec macIPs
      searchPortsNoRec macPorts
      if d == mempty
        then modify (modMac delMac mac)
        else return ()
      _ 
      _ 
  if mac `M.member` found
    then modify (flip (M.foldrWithKey addMacPort) xs)
    else M.lookup mac macInfo-}

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
  found <- case ips of
    [] -> do
      t <- liftIO getCurrentTime
      if diffUTCTime t cacheTime > updateInterval
        then findIPInfo (Left ["213.108.248.0/21"]) nmapHost
        else return M.empty
    _ -> findIPInfo (Right ips) nmapHost
  liftIO $ print "Adding ips:"
  liftIO $ print found
  forM_ ips $ \ip ->
    case ip `M.lookup` found of
      Just macs -> modify $ addIPMac ip macs
      Nothing   -> modify (modIP (setIPState Unreachable) ip)
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


