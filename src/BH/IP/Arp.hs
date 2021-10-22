{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module BH.IP.Arp (
  ipNeigh,
  nmapCache,
  queryLinuxArp,
  queryLinuxArp2,
  mergePorts,
  --mergeMacs,
) where

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Attoparsec.Text as A
import Data.Char
import Data.Either.Combinators
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Yaml as Y
import qualified Shelly as Sh
import System.Directory
import Text.XML.Light
import Data.Time
import Data.Monoid
import Control.Arrow
import Control.Monad.State

import BH.Cache
import BH.Main.Types
import BH.Common
import BH.IP
import BH.Switch

-- TODO: Parse nmap xml with xeno [pkg].
-- TODO: Implement cache update strategy, where new entries are merged with
-- old cache. Note, though, that in this case i'd better build 'IpMacMap'
-- first, because it defines 1-1 relation and if some IP's mac address has
-- changed, it'll be correctly updated. If i use 1-many relation (like in
-- 'MacIpMap') finding this IP, who no longer has this mac, will be much more
-- tricky. [cache][arp]
--
-- I don't think, that i should merge old cache with new data. Old cache may
-- contain no longer valid entires (e.g. port is blocked or server shutdown).
-- Though, building 'IpMacMap' first may still be viable, because i may show
-- the difference between current situation and old cache during update.

-- TODO: "ip neigh" usually finds much less IPs, than nmap. Also, subsequent
-- nmap runs may miss one-two IPs. So, i should either just use "nmap" or use
-- both and merge results.[arp][nmap]

-- Use "ip neigh" for buliding arp/IP cache.

-- | Parse 'ip neigh' output in the form of "IP, Mac, record state". This
-- parser does /not/ consume trailing line character or spaces.
ipNeighP :: A.Parser (IP, MacAddr, T.Text)
ipNeighP =
  (,,)
    <$> lexemeA ipP <* A.count 3 (A.takeWhile1 (/= ' ') *> A.string " ")
    <*> lexemeA macP
    <*> (A.takeWhile1 (not . isSpace) A.<?> "mac state")
    A.<?> "ip neigh"

-- | Parse single line from 'ip neigh' output and initialize 'MacIpMap' and
-- 'IpMacMap' from it.
readIpNeighLine ::
  MonadError String m =>
  -- | Line of output text.
  T.Text ->
  -- | Current map values.
  (MacIpMap, IpMacMap) ->
  m (MacIpMap, IpMacMap)
readIpNeighLine ts z@(macMap, ipMap) = do
  (ip, mac, state) <- liftEither (A.parseOnly ipNeighP ts)
  if state == "STALE" || state == "REACHABLE"
    then return (M.insertWith (<>) mac (S.singleton ip) macMap, M.insert ip mac ipMap)
    else return z

-- | Call 'ip neigh' on specified host and initialize 'MacIpMap' and
-- 'IpMacMap' from it output.
ipNeigh :: (MonadIO m, MonadError String m) => T.Text -> m (MacIpMap, IpMacMap)
ipNeigh host = Sh.shelly (Sh.silently go) >>= liftEither
 where
  go :: Sh.Sh (Either String (MacIpMap, IpMacMap))
  go = do
    liftIO $ putStrLn "Updating arp cache using `nping` and `ip neighbour`..."
    Sh.run_
      "ssh"
      (host : T.words "nping --quiet -N --rate=100 -c1 213.108.248.0/21")
    liftIO $ threadDelay 5000000
    z <-
      Sh.runFoldLines
        (pure mempty)
        (\mz ts -> mz >>= readIpNeighLine ts)
        "ssh"
        (host : T.words "ip neighbour show nud reachable nud stale")
        >>= liftIO . evaluate . force
    Sh.run_ "ssh" (host : T.words "ip neighbour flush all")
    return z

-- Use "nmap" for buliding arp/IP cache.

-- TODO: nmap xml contains vendor determined from mac address. This may be
-- used for calssifying mac address for virtual and physical.

-- | Parse 'address' element from nmap xml. Parse errors and missed (but
-- expected) xml attributes are treated as errors and will be preserved in
-- 'Left'. But attempting to parse xml "address" record of /wrong address
-- type/ will just result in 'Nothing'. This allows to use this parser
-- uniformly for parsing IP and mac addresses and still catching all real
-- parse errors.
xmlAddrP :: MonadError String m => String -> A.Parser a -> Element -> m (Maybe a)
xmlAddrP at addrP addr = do
  addrtype <-
    maybeErr
      ("Can't find 'addrtype' attribute in 'address' xml element: '" <> show addr <> "'")
      $ findAttr (blank_name{qName = "addrtype"}) addr
  if addrtype == at
    then do
      x <-
        maybeErr
          ("Can't find 'addr' attribute in 'address' xml element: '" <> show addr <> "'")
          (findAttr (blank_name{qName = "addr"}) addr)
      catchError
        (fmap Just . liftEither . A.parseOnly addrP $ T.pack x)
        (\e -> throwError $ "Error during parsing 'address' element: '" <> show x <> "'\n" <> e)
    else return Nothing

-- | Parse mac and IP addresses from nmap xml 'host' element.
xmlHostAddressP :: MonadError String m => Element -> m (MacAddr, [IP])
xmlHostAddressP host = do
  let addrs = findChildren (blank_name{qName = "address"}) host
  ips <- catMaybes <$> mapM (xmlAddrP "ipv4" ipP) addrs
  macs <- catMaybes <$> mapM (xmlAddrP "mac" macP) addrs
  case macs of
    [] -> throwError $ "No mac address found in xml element: '" <> show host <> "'"
    [mac] -> return (mac, ips)
    _ -> throwError $ "Several mac addresses found in xml element: '" <> show host <> "'"

xmlHostAddressP2 :: MonadError String m => Element -> m (M.Map IP (S.Set MacAddr))
xmlHostAddressP2 host = do
  let addrs = findChildren (blank_name{qName = "address"}) host
  macs <- S.fromList . catMaybes <$> mapM (xmlAddrP "mac" macP) addrs
  ips  <- catMaybes <$> mapM (xmlAddrP "ipv4" ipP) addrs
  return . M.fromList . map (, macs) $ ips

-- | Check that, 'reason' attribute of xml 'status' element (from inside xml
-- 'host' element) is equal to specified value.
xmlHostStatusIs :: MonadError String m => Element -> String -> m Bool
xmlHostStatusIs host rs =
  let sts = findChildren (blank_name{qName = "status"}) host
   in case sts of
        [] -> throwError $ "No 'status' xml element found in host: '" <> show host <> "'"
        [status] -> do
          reason <-
            maybeErr
              ("Can't find 'reason' attribute in 'status' xml element: '" <> show status <> "'")
              $ findAttr (blank_name{qName = "reason"}) status
          return (reason == rs)
        _ -> throwError $ "Several 'status' xml elements found in host: '" <> show host <> "'"

parseNmapXml :: MonadError String m => T.Text -> m (MacIpMap, IpMacMap)
parseNmapXml t =
  let xml = blank_element{elContent = parseXML t}
      hosts =
        findChildren (blank_name{qName = "nmaprun"}) xml
          >>= findChildren (blank_name{qName = "host"})
   in foldM go mempty hosts
 where
  go :: MonadError String m => (MacIpMap, IpMacMap) -> Element -> m (MacIpMap, IpMacMap)
  go z@(macMap, ipMap) host = do
    b <- host `xmlHostStatusIs` "arp-response"
    if b
      then do
        (mac, ips) <- xmlHostAddressP host
        return (M.insertWith (<>) mac (S.fromList ips) macMap, foldr (`M.insert` mac) ipMap ips)
      else return z

parseNmapXml2 :: MonadError String m => T.Text -> m (M.Map IP (S.Set MacAddr))
parseNmapXml2 t =
  let xml = blank_element{elContent = parseXML t}
      hosts =
        findChildren (blank_name{qName = "nmaprun"}) xml
          >>= findChildren (blank_name{qName = "host"})
   in foldM go mempty hosts
 where
  go :: MonadError String m => M.Map IP (S.Set MacAddr) -> Element -> m (M.Map IP (S.Set MacAddr))
  go z host = do
    b <- host `xmlHostStatusIs` "arp-response"
    if b
      then M.unionWith (<>) z <$> xmlHostAddressP2 host
      else return z

mergeIP :: M.Map IP (S.Set MacAddr) -> (IPInfo, MacInfo, SwPortInfo) -> (IPInfo, MacInfo, SwPortInfo)
mergeIP xs z@(ipInfo, _, _) =
  let remIPs = M.keysSet ipInfo `S.difference` M.keysSet xs
  in  flip (M.foldrWithKey addIPMac) xs . flip (foldr (modIP (setIPState Unreachable))) remIPs $ z

addIPMac :: IP -> S.Set MacAddr -> (IPInfo, MacInfo, SwPortInfo) -> (IPInfo, MacInfo, SwPortInfo)
addIPMac ip macs z@(ipInfo, macInfo, _) =
  let ipMacPorts = M.fromSet (\m -> M.lookup m macInfo >>= getLast . macSwPort) macs
      ipState = Last (Just Answering)
  in  modIP' (addIP IPData{..}) ip macs . modIP' (delIP remMacs) ip remMacs $ z
 where
  remMacs =
    let oldMacs = fromMaybe S.empty (M.keysSet . ipMacPorts <$> M.lookup ip ipInfo)
    in  oldMacs `S.difference` macs

class ModPort a where
  setPortState :: PortState -> SwPort -> a -> a
  delPort :: S.Set MacAddr -> SwPort -> a -> a
  addPort :: PortData -> SwPort -> a -> a

instance ModPort SwPortInfo where
  setPortState s = M.adjust (\d -> d{portState = Last (Just s)})
  delPort macs port swPortInfo = fromMaybe swPortInfo $ do
    PortData{..} <- M.lookup port swPortInfo
    let f | S.size macs /= S.size (M.keysSet portAddrs) =
              M.adjust (modifyL portAddrsL (\z -> foldr M.delete z macs))
          | otherwise = M.delete
    return (f port swPortInfo)
  addPort pd p = M.insert p pd

-- FIXME: Or i may write instance for 'M.Map MacAddr (Maybe (SwPort,
-- PortState)) instead, because i send selection (S.Set MacAddr) in any case..
-- That's depends on does 'MacAddr -- Nothing' has sense or not. And it seems
-- it does.
instance ModPort (Maybe (SwPort, PortState)) where
  setPortState s _ mx = maybe mx (\(p, _) -> Just (p, s)) mx
  delPort _ _ _ = Nothing
  addPort PortData{..} port _ = do
    s <- getLast portState
    return (port, s)

modPort :: (forall a. ModPort a => SwPort -> a -> a) -- ^ modifies
      -> SwPort -- ^ selects
      -> (IPInfo, MacInfo, SwPortInfo) -> (IPInfo, MacInfo, SwPortInfo)
modPort k x = modPort' (const k) x (const True)

modPort' :: (forall a. ModPort a
              => S.Set MacAddr -- ^ selection
              -> SwPort
              -> a
              -> a) -- ^ modifies
      -> SwPort -- ^ selects
      -> (MacAddr -> Bool) -- ^ restricts spread
      -> (IPInfo, MacInfo, SwPortInfo) -> (IPInfo, MacInfo, SwPortInfo)
modPort' k port p z@(ipInfo, macInfo, swPortInfo) = fromMaybe z $ do
  xs <- M.filterWithKey (\k _ -> p k) . portAddrs <$> M.lookup port swPortInfo
  let macs = M.keysSet xs
      ipInfo' = M.foldrWithKey
        (\mac -> flip $ M.foldrWithKey
          (\ip _ z ->
            M.adjust (modifyL ipMacPortsL (M.adjust (k macs port) mac)) ip z
          )
        )
        ipInfo xs
      macInfo' = S.foldr (M.adjust (modifyL macSwPortL (k macs port))) macInfo macs
  return
    ( ipInfo'
    , macInfo'
    , k macs port swPortInfo
    )

mergePorts :: M.Map SwPort (PortState, S.Set MacAddr) -> (IPInfo, MacInfo, SwPortInfo) -> (IPInfo, MacInfo, SwPortInfo)
mergePorts = flip (M.foldrWithKey addPortMac)

addPortMac :: SwPort -> (PortState, S.Set MacAddr) -> (IPInfo, MacInfo, SwPortInfo) -> (IPInfo, MacInfo, SwPortInfo)
addPortMac port (st, macs) z@(_, macInfo, swPortInfo) =
  let portAddrs = M.fromSet (\m -> fromMaybe M.empty (macIPs <$> M.lookup m macInfo)) macs
      portState = Last (Just st)
  in  modPort2' (addPort PortData{..}) port st macs . modPort2' (delPort remMacs) port st remMacs $ z
 where
  remMacs =
    let oldMacs = fromMaybe S.empty (M.keysSet . portAddrs <$> M.lookup port swPortInfo)
    in  oldMacs `S.difference` macs

modPort2 :: (forall a. ModPort a => SwPort -> a -> a) -- ^ modifies
      -> SwPort -- ^ selects
      -> (IPInfo, MacInfo, SwPortInfo) -> (IPInfo, MacInfo, SwPortInfo)
modPort2 k port z@(_, _, swPortInfo) =
  let allMacs = fromMaybe S.empty (M.keysSet . portAddrs <$> M.lookup port swPortInfo)
  in  modPort2' k port Up allMacs z

modPort2' :: (forall a. ModPort a
              => SwPort
              -> a
              -> a) -- ^ modifies
      -> SwPort -- ^ selects
      -> PortState
      -> S.Set MacAddr -- ^ selects references
      -> (IPInfo, MacInfo, SwPortInfo) -> (IPInfo, MacInfo, SwPortInfo)
modPort2' k port st macs z@(ipInfo, macInfo, swPortInfo) =
  let macSwPort = Last (Just (port, st))
      macInfo' = S.foldr (insertAdjust (modifyL macSwPortL (k port)) MacData{macIPs = M.empty, ..}) macInfo macs
      xs = M.map macIPs . M.filterWithKey (const . (`S.member` macs)) $ macInfo
      ipInfo' = M.foldrWithKey
        (\mac -> flip $ M.foldrWithKey
          (\ip _ z ->
            M.adjust (modifyL ipMacPortsL (insertAdjust (k port) (getLast macSwPort) mac)) ip z
          )
        )
        ipInfo xs
  in  ( ipInfo'
      , macInfo'
      , k port swPortInfo
      )

class ModMac a where
  delMac :: (S.Set IP, Maybe SwPort) -> MacAddr -> a -> a
  addMac :: MacData -> MacAddr -> a -> a

instance ModMac (M.Map MacAddr (M.Map IP IPState)) where
  addMac d mac = M.insert mac (macIPs d)
  delMac (ips, _) mac portAddrs = fromMaybe portAddrs $ do
    m <- M.lookup mac portAddrs
    let f | ips == M.keysSet m =
              M.adjust (\z -> foldr M.delete z ips)
          | otherwise = M.delete
    return (f mac portAddrs)

instance ModMac (M.Map MacAddr (Maybe (SwPort, PortState))) where
  addMac d mac = M.insert mac (getLast . macSwPort $ d)

instance ModMac MacInfo where
  addMac = flip M.insert

modMac' :: (forall a. ModMac a
              => MacAddr
              -> a
              -> a) -- ^ modifies
      -> MacAddr -- ^ selects
      -> (S.Set IP, Maybe SwPort) -- ^ selects references
      -> (IPInfo, MacInfo, SwPortInfo) -> (IPInfo, MacInfo, SwPortInfo)
modMac' k mac (ips, mport) z@(ipInfo, macInfo, swPortInfo) =
  let
      ipState = Last (Just Answering)
      ipInfo' = foldr (insertAdjust (modifyL ipMacPortsL (k mac)) IPData{ipMacPorts = M.empty, ..}) ipInfo ips
      portState = Last (Just Up)
      swPortInfo' = maybe id (insertAdjust (modifyL portAddrsL (k mac)) PortData{portAddrs = M.empty, ..}) mport $ swPortInfo
  in  ( ipInfo'
      , k mac macInfo
      , swPortInfo
      )

insertAdjust :: Ord a => (b -> b) -> b -> a -> M.Map a b -> M.Map a b
insertAdjust f def x = M.insertWith (const f) x (f def)

-- | Selects IP with all macs.
modIP :: (forall a. ModIP a => IP -> a -> a) -- ^ modifies
      -> IP -- ^ selects
      -> (IPInfo, MacInfo, SwPortInfo) -> (IPInfo, MacInfo, SwPortInfo)
modIP f ip z@(ipInfo, _, _) =
  let allMacs = fromMaybe S.empty (M.keysSet . ipMacPorts <$> M.lookup ip ipInfo)
  in  modIP' f ip allMacs z

modIP' :: (forall a. ModIP a => IP -> a -> a) -- ^ modifies
      -> IP -- ^ selects
      -> S.Set MacAddr -- ^ selects references
      -> (IPInfo, MacInfo, SwPortInfo) -> (IPInfo, MacInfo, SwPortInfo)
modIP' k ip macs z@(ipInfo, macInfo, swPortInfo) =
  let macIPs = M.singleton ip Answering
      macInfo' = S.foldr
        (insertAdjust (modifyL macIPsL (k ip)) MacData{macSwPort = Last Nothing, ..})
        macInfo macs
      xs = M.map macSwPort . M.filterWithKey (const . (`S.member` macs)) $ macInfo
      swPortInfo' = M.foldrWithKey
        (\mac (Last mp) z -> case mp of
          Just (port, _)  -> M.adjust (modifyL portAddrsL (insertAdjust (k ip) macIPs mac)) port z
          Nothing         -> z
        )
        swPortInfo xs
  in  ( k ip ipInfo
      , macInfo'
      , swPortInfo'
      )

class ModIP a where
  setIPState :: IPState -> IP -> a -> a
  delIP :: S.Set MacAddr -> IP -> a -> a
  addIP :: IPData -> IP -> a -> a

instance ModIP IPInfo where
  setIPState s = M.adjust (\d -> d{ipState = Last (Just s)})
  delIP macs ip ipInfo = fromMaybe ipInfo $ do
    IPData{..} <- M.lookup ip ipInfo
    let f | macs /= M.keysSet ipMacPorts =
              --M.adjust (modifyL ipMacPortsL (M.filterWithKey (\k _ -> k `S.notMember` macs)))
              M.adjust (modifyL ipMacPortsL (\z -> foldr M.delete z macs))
          | otherwise = M.delete
    return (f ip ipInfo)
  addIP d ip = M.insert ip d

instance ModIP (M.Map IP IPState) where
  setIPState s = M.adjust (const s)
  delIP _ = M.delete
  addIP d ip = case getLast (ipState d) of
    Just s  -> M.insert ip s
    Nothing -> id

-- FIXME: This function should remove IPs, whose all macs are 'Unreachable'
-- and whose do not have any port defined.
-- FIXME: Add function to remove Macs, which does not have IPs and undefined
-- port.
-- FIXME: Unreachable IPs may be on 'Up' ports, because this simply means,
-- that this IP is no longer used by this server. Though, 'Disabled' or
-- 'NotConnect' ports can't have 'Answering' IPs.
dbTidy :: (IPInfo, MacInfo, SwPortInfo) -> (IPInfo, MacInfo, SwPortInfo)
dbTidy = undefined

-- | Call "nmap" on specified host for building 'MacIpMap' and 'IpMacMap'.
nmapCache :: (MonadIO m, MonadError String m) => T.Text -> m (MacIpMap, IpMacMap)
nmapCache host = Sh.shelly (Sh.silently go) >>= liftEither
 where
  go :: Sh.Sh (Either String (MacIpMap, IpMacMap))
  go = do
    liftIO $ putStrLn "Updating arp cache using `nmap`..."
    xml <- do
      Sh.run_ "ssh" (host : T.words "nmap -sn -PR -oX nmap_arp_cache.xml 213.108.248.0/21")
      Sh.run "ssh" (host : T.words "cat ./nmap_arp_cache.xml")
    z <- liftIO . evaluate . force $ parseNmapXml xml
    void $ Sh.run "ssh" (host : T.words "rm ./nmap_arp_cache.xml")
    return z

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

-- Read and initialize mac/IP cache.

-- FIXME: Can i generalize following 'readCache' and 'updateArpCache'
-- functions to just read _any_ yaml cache and update it upon some conditions?
-- This cache may include regular ansible invetory. Also, replace 'readSwInfo'
-- with generic version of this. [cache]

-- | Read arp cache file.
readCache :: (MonadIO m, MonadError String m) => FilePath -> m MacIpMap
readCache cacheFile = do
  b <- liftIO (doesFileExist cacheFile)
  -- FIXME: Update cache, if it's too old. [cache]
  if b
    then
      catchError
        (liftIO (Y.decodeFileEither cacheFile) >>= liftEither . mapLeft show)
        (\e -> liftIO (print e) >> return mempty)
    else return mempty

-- | Update arp cache file, if necessary, and build corresponding maps.
updateArpCache :: (MonadIO m, MonadError String m, MonadReader Config m) => FilePath -> T.Text -> MacIpMap -> m (MacIpMap, IpMacMap)
updateArpCache cacheFile host cache
  | cache /= mempty = return (cache, M.foldrWithKey rebuild mempty cache)
  | otherwise = do
    maps@(macMap, _) <- nmapCache host
    --map2 <- nmapCache2 host
    --maps@(macMap, _) <- ipNeigh host
    liftIO $ Y.encodeFile cacheFile macMap
    return maps
 where
  -- Rebuild 'IpMacMap' from cached 'MacIpMap'.
  rebuild :: MacAddr -> S.Set IP -> IpMacMap -> IpMacMap
  rebuild mac ips zm0 = foldr (`M.insert` mac) zm0 ips

-- FIXME: Do not query all IPs at once. I don't need this, really. I may just
-- nmap single IP in question and do all the other stuff with it. [current]
queryLinuxArp ::
  (MonadIO m, MonadError String m, MonadReader Config m) =>
  -- | Path to yaml cache.
  FilePath ->
  -- | ssh hostname of host, from which to query.
  T.Text ->
  m (MacIpMap, IpMacMap)
queryLinuxArp cacheFile host = do
  Config{..} <- ask
  t <- liftIO getCurrentTime
  if diffUTCTime t cacheTime > updateInterval
    then do
      liftIO (writeFile timeFile (show t))
      updateArpCache cacheFile host M.empty
    else readYaml cacheFile >>= updateArpCache cacheFile host

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

queryMikrotikArp :: T.Text -> IO MacIpMap
queryMikrotikArp host =
  Sh.shelly . Sh.silently $
    Sh.runFoldLines M.empty (\zs -> go zs . T.words) "ssh" [host, "/ip", "arp", "print"]
 where
  go :: MacIpMap -> [T.Text] -> MacIpMap
  go zs (_ : _ : x : y : _) =
    either (const zs) (\(w, t) -> uncurry (M.insertWith (<>)) (w, S.fromList t) zs) $ do
      ip <- A.parseOnly ipP x
      ma <- A.parseOnly macP y
      return (ma, [ip])
  go zs _ = zs
