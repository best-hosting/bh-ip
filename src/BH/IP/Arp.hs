{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module BH.IP.Arp
    ( MacIpMap
    , IpMacMap
    , queryLinuxArp
    )
  where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.IO.Class
import qualified Data.Map as M
import qualified Data.Set as S
import System.Environment
import Control.Monad.Trans.Cont
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.Maybe
import qualified Shelly as Sh
import Control.Concurrent
import qualified Data.Yaml as Y
import System.Directory
import Text.HTML.TagSoup
import qualified Data.Attoparsec.Text as A
import Control.Monad (join)
import Control.Applicative
import Control.Applicative.Combinators
import Data.Char
import Text.XML.Light
import Data.List
import Data.Maybe
import Control.Monad.Except
import Control.Exception
import Control.DeepSeq

import BH.Common
import BH.IP
import BH.Switch


-- FIXME: Use Set, so uniquiness won't be a problem.
type MacIpMap       = M.Map MacAddr [IP]
type IpMacMap       = M.Map IP MacAddr

-- | Parse 'ip neigh' output in the form of "IP, Mac, record state". This
-- parser does /not/ consume trailing line character or spaces.
ipNeighP :: A.Parser (IP, MacAddr, T.Text)
ipNeighP = (,,)
    <$> lexemeA ipP  <* A.count 3 (A.takeWhile1 (/= ' ') *> A.string " ")
    <*> lexemeA macP
    <*> (A.takeWhile1 (not . isSpace) A.<?> "mac state")
    A.<?> "ip neigh"

-- | Parse single line from 'ip neigh' output and initialize 'MacIpMap' and
-- 'IpMacMap' from it.
readIpNeighLine :: T.Text -> (MacIpMap, IpMacMap) -> Either String (MacIpMap, IpMacMap)
readIpNeighLine ts z@(macMap, ipMap) = do
    (ip, mac, state) <- A.parseOnly ipNeighP ts
    if state == "STALE" || state == "REACHABLE"
      then return (M.insertWith addIp mac [ip] macMap, M.insert ip mac ipMap)
      else return z

-- | Call 'ip neigh' on specified host and initialize 'MacIpMap' and
-- 'IpMacMap' from it output.
ipNeigh :: MonadIO m => T.Text -> ExceptT String m (MacIpMap, IpMacMap)
ipNeigh host = ExceptT . Sh.shelly . Sh.silently $ do
    liftIO $ putStrLn "Updating arp cache using `nping` and `ip neighbour`..."
    Sh.run_ "ssh"
            (host : T.words "nping --quiet -N --rate=100 -c1 213.108.248.0/21")
    liftIO $ threadDelay 5000000
    z <- Sh.runFoldLines (pure mempty) (\mz ts -> mz >>= readIpNeighLine ts) "ssh"
            (host : T.words "ip neighbour show nud reachable nud stale")
    Sh.run_ "ssh" (host : T.words "ip neighbour flush all")
    return z

queryMikrotikArp :: T.Text -> IO MacIpMap
queryMikrotikArp host   = Sh.shelly . Sh.silently $
    Sh.runFoldLines M.empty (\zs -> go zs . T.words) "ssh" [host, "/ip", "arp", "print"]
  where
    go :: MacIpMap -> [T.Text] -> MacIpMap
    go zs (_ : _ : x : y : _) =
        either (const zs) (\(w, t) -> uncurry (M.insertWith addIp) (w, t) zs) $ do
          ip <- A.parseOnly ipP x
          ma <- A.parseOnly macP y
          return (ma, [ip])
    go zs _                   = zs

{--- Use 'ip neigh'.
queryLinuxArp :: FilePath   -- ^ Path to yaml cache.
              -> T.Text     -- ^ ssh hostname of host, from which to query.
              -> ExceptT String IO (MacIpMap, IpMacMap)
queryLinuxArp file host = do
    b <- liftIO $ doesFileExist file
    -- FIXME: Update cache, if it's too old.
    if b
      then catchE (ExceptT (Y.decodeFileEither file) >>= \x -> (x, M.empty)) $ \e -> do
        liftIO (print e)
        updateArpCache
      else updateArpCache
  where
    updateArpCache :: ExceptT String IO (MacIpMap, IpMacMap)
    updateArpCache  = do
        --mi <- nmapCache
        maps@(macMap, _) <- ipNeighCache host
        liftIO $ Y.encodeFile file macMap
        return maps-}

-- Use 'ip neigh'.
queryLinuxArp :: FilePath   -- ^ Path to yaml cache.
              -> T.Text     -- ^ ssh hostname of host, from which to query.
              -> ExceptT String IO (MacIpMap, IpMacMap)
queryLinuxArp cacheFile host =
    readCache cacheFile >>= updateArpCache cacheFile host

-- | Read arp cache file.
readCache :: FilePath -> ExceptT String IO MacIpMap
readCache cacheFile = do
    b <- liftIO $ doesFileExist cacheFile
    -- FIXME: Update cache, if it's too old.
    if b
      then catchE (ExceptT $ Y.decodeFileEither cacheFile) $ \e ->
             liftIO (print e) >> return mempty
      else return mempty

-- | Update arp cache file, if necessary, and build corresponding maps.
updateArpCache :: FilePath -> T.Text -> MacIpMap -> ExceptT String IO (MacIpMap, IpMacMap)
updateArpCache cacheFile host cache
  | cache /= mempty = return (cache, M.foldrWithKey rebuild mempty cache)
  | otherwise = do
    maps@(macMap, _) <- nmapCache host
    --maps@(macMap, _) <- ipNeigh host
    liftIO $ Y.encodeFile cacheFile macMap
    return maps
  where
    -- Rebuild 'IpMacMap' from cached 'MacIpMap'.
    rebuild :: MacAddr -> [IP] -> IpMacMap -> IpMacMap
    rebuild mac ips zm0 = foldr (\ip zm -> M.insert ip mac zm) zm0 ips

{-nmapP :: A.Parser (T.Text, IP, MacAddr)
nmapP =
    lexemeA (A.string "<host>") *> lexemeA (A.string "<status ")
    A.string "reason="
        *> A.string "arp-response"
    lexemeA (A.string "address ") *> A.string "addr="
        *> lexemeA (between (A.string "\"") (A.string "\"") ipP)
    lexemeA (A.string "address ") *> A.string "addr="
        *> lexemeA (between (A.string "\"") (A.string "\"") macP)-}

-- TODO: nmap xml contains vendor determined from mac address. This may be
-- used for calssifying mac address for virtual and physical.

-- TODO: May be add 'vlan' field to 'IP' ? And 'vendor' field to 'MacAddr' type ?

{-nmapXmlHostP :: A.Parser (IP, MacAddr)
nmapXmlHostP =
    lexemeA (A.string "<host>") *> lexemeA (A.string "<status ")-}

t1 :: ExceptT String IO (MacIpMap, IpMacMap)
t1 = do
    c <- liftIO $ T.readFile "nmap_arp_cache.xml"
    except (parseNmapXml c)

t2 :: IO [Content]
t2 = do
    c <- T.readFile "nmap_arp_cache.xml"
    return (parseXML c)

t3 :: Content -> Bool
t3 (Elem e) = qName (elName e) == "nmaprun"
t3 _ = False

--t4' :: [Content] -> [String]
t4' es = t4 (blank_element{elContent = es})

--t4 :: Element -> [String]
t4 e = do
    host <- findChildren (blank_name{qName = "nmaprun"}) e
              >>= findChildren (blank_name{qName = "host"})
    status <- findChildren (blank_name{qName = "status"}) host
    if checkStatusP "arp-response" status
      then maybe mzero return (xmlAddrsP host)
      else mzero

{-nmapXmlP :: Element -> ExceptT String [] (MacAddr, [IP])
nmapXmlP e = do
    host <- lift $ findChildren (blank_name{qName = "nmaprun"}) e
                     >>= findChildren (blank_name{qName = "host"})
    b <- except $ host `xmlHostStatusIs` "arp-response"
    if b
      then except $ xmlHostAddressP host
      else mzero-}

{-parseNmapXml :: T.Text -> Either String [(MacAddr, [IP])]
parseNmapXml t =
    let xml = blank_element{elContent = parseXML t}
        hosts = findChildren (blank_name{qName = "nmaprun"}) xml
                  >>= findChildren (blank_name{qName = "host"})
    in  foldM go [] hosts
  where
    --go :: MonadError String m => [(MacAddr, [IP])] -> Element -> m [(MacAddr, [IP])]
    go zs host = do
        b <- host `xmlHostStatusIs` "arp-response"
        if b
          then (: zs) <$> xmlHostAddressP host
          else return zs-}

parseNmapXml :: T.Text -> Either String (MacIpMap, IpMacMap)
parseNmapXml t =
    let xml = blank_element{elContent = parseXML t}
        hosts = findChildren (blank_name{qName = "nmaprun"}) xml
                  >>= findChildren (blank_name{qName = "host"})
    in  foldM go mempty hosts
  where
    go :: MonadError String m => (MacIpMap, IpMacMap) -> Element -> m (MacIpMap, IpMacMap)
    go z@(macMap, ipMap) host = do
        b <- host `xmlHostStatusIs` "arp-response"
        if b
          then do
            (mac, ips) <- xmlHostAddressP host
            return (M.insertWith addIp mac ips macMap, foldr (\ip zs -> M.insert ip mac zs) ipMap ips)
          else return z

-- FIXME: Use 'MonadError' from Control.Monad.Except instead of explicit
-- 'Either' or 'ExceptT'. Also, i should annotate errors, because now it's
-- impossible to say, where e.g. error with mac parsing happens.

-- | Check that, 'reason' attribute of xml 'status' element (from inside xml
-- 'host' element) is equal to specified value.
xmlHostStatusIs :: MonadError String m => Element -> String -> m Bool
xmlHostStatusIs host rs =
    let sts = findChildren (blank_name{qName = "status"}) host
    in  case sts of
          []        -> throwError $ "No 'status' xml element found in host: '" <> show host <> "'"
          [status]  -> do
            reason <- maybeErr2
                        ("Can't find 'reason' attribute in 'status' xml element: '" <> show status <> "'")
                        $ findAttr (blank_name{qName = "reason"}) status
            return (reason == rs)
          _         -> throwError $ "Several 'status' xml elements found in host: '" <> show host <> "'"

xmlAddrsP :: Element -> Maybe (String, [String])
xmlAddrsP host =
    let addrs = findChildren (blank_name{qName = "address"}) host
        ips   = mapMaybe xmlIpP addrs
    in  case mapMaybe xmlMacP addrs of
          [mac] -> return (mac, ips)
          _     -> mempty

checkStatusP :: String -> Element -> Bool
checkStatusP reason e = fromMaybe False $ do
    r <- findAttr (blank_name{qName = "reason"}) e
    return (r == reason)

xmlIpP :: Element -> Maybe String
xmlIpP e = do
    addrtype <- findAttr (blank_name{qName = "addrtype"}) e
    if addrtype == "ipv4"
      then findAttr (blank_name{qName = "addr"}) e
      else mempty

maybeErr :: String -> Maybe a -> Either String a
maybeErr err = maybe (Left err) return

maybeErr2 :: MonadError String m => String -> Maybe a -> m a
maybeErr2 err = maybe (throwError err) return

-- | Parse mac and IP addresses from nmap xml 'host' element.
xmlHostAddressP :: MonadError String m => Element -> m (MacAddr, [IP])
xmlHostAddressP host = do
    let addrs = findChildren (blank_name{qName = "address"}) host
    ips  <- catMaybes <$> mapM (xmlAddrP "ipv4" ipP) addrs
    macs <- catMaybes <$> mapM (xmlAddrP "mac" macP) addrs
    case macs of
      []    -> throwError $ "No mac address found in xml element: '" <> show host <> "'"
      [mac] -> return (mac, ips)
      _     -> throwError $ "Several mac addresses found in xml element: '" <> show host <> "'"

-- | Parse 'address' element from nmap xml. Parse errors and missed (but
-- expected) xml attributes are treated as errors and will be preserved in
-- 'Left'. But attempting to parse xml "address" record of /wrong address
-- type/ will just result in 'Nothing'. This allows to use this parser
-- uniformly for parsing IP and mac addresses and still catching all real
-- parse errors.
xmlAddrP :: MonadError String m => String -> A.Parser a -> Element -> m (Maybe a)
xmlAddrP at addrP e = do
    addrtype <- maybeErr2
                  ("Can't find 'addrtype' attribute in 'address' xml element: '" <> show e <> "'")
                  $ findAttr (blank_name{qName = "addrtype"}) e
    if addrtype == at
      then do
        x <- maybeErr2 ("Can't find 'addr' attribute in 'address' xml element: '" <> show e <> "'")
          (findAttr (blank_name{qName = "addr"}) e)
        catchError
          (fmap Just . liftEither . A.parseOnly addrP $ T.pack x)
          (\e -> throwError $ "Error during parsing 'address' element: '" <> show x <> "'\n" <> e)
      else return Nothing

xmlMacP :: Element -> Maybe String
xmlMacP e = do
    addrtype <- findAttr (blank_name{qName = "addrtype"}) e
    if addrtype == "mac"
      then findAttr (blank_name{qName = "addr"}) e
      else mempty

-- | Parse host status in nmap xml.
nmapXmlHostStatusP :: A.Parser T.Text
nmapXmlHostStatusP =
    let statusChars = liftA2 (||) isAlpha (== '-')
    in  lexemeA (A.string "<status ") *> A.string "reason="
          *> (between (A.string "\"") (A.string "\"") (A.takeWhile1 statusChars A.<?> "status chars"))
          <*  A.takeWhile (/= '/') <* A.string "/>"
          A.<?> "nmap xml address"

-- | Parse mac or IP address in nmap xml.
nmapXmlAddressP :: A.Parser a -> A.Parser a
nmapXmlAddressP addrP =
    lexemeA (A.string "<address ") *> A.string "addr="
      *> lexemeA (between (A.string "\"") (A.string "\"") addrP)
      <* A.takeWhile (/= '/') <* A.string "/>"
      A.<?> "nmap xml address"

{-findXmlElementP :: A.Parser T.Text -> A.Parser a -> A.Parser a
findXmlElementP nameP valueP =
    let anyNameP = A.takeWhile1 isAlpha
        anyValueP = A.takeWhile1 (/= '"')
        xmlElementP nameP valueP
    <|> lexemeA (xmlElementP anyNameP anyValueP) *> findXmlElementP nameP valueP-}

xmlElementP :: A.Parser T.Text -> A.Parser a -> A.Parser a
xmlElementP nameP valueP =
    nameP *> A.string "=" *> (between (A.string "\"") (A.string "\"") valueP)

nmapCache :: T.Text -> ExceptT String IO (MacIpMap, IpMacMap)
nmapCache host = ExceptT . Sh.shelly . Sh.silently $ do
    liftIO $ putStrLn "Updating arp cache using `nmap`..."
    xml <- do
      Sh.run_ "ssh" (host : T.words "nmap -sn -PR -oX nmap_arp_cache.xml 213.108.248.0/21")
      Sh.run  "ssh" (host : T.words "cat ./nmap_arp_cache.xml")
    z <- liftIO . evaluate . force $ parseNmapXml xml
    --Sh.run "ssh" (host : T.words "rm ./nmap_arp_cache.xml")
    return z
  where
    go :: [Tag T.Text] -> Either String (MacAddr, [IP])
    go xs =
        let oneHost = filter (~== ("<address>" :: String)) . takeWhile (~/= ("</host>" :: String))
        in  case oneHost xs of
              [x, y]  -> do
                          ip  <- A.parseOnly ipP (fromAttrib "addr" x)
                          mac <- A.parseOnly macP (fromAttrib "addr" y)
                          return (mac, [ip])
              _       -> Left "Unrecognized ip, mac pair in nmap output."

{-

{-parseMikrotikArp :: [T.Text -> MacIpMap
parseMikrotikArp zs t = case T.words t of
    (_ : _ : x : y : _) ->
      either (const zs) (\(w, y) -> uncurry (M.insertWith addIp) (w, y) zs) $ do
        ip <- A.parseOnly ipP x
        ma <- parseMacAddr y
        return (ma, [ip])
    _                   -> zs-}

parseMikrotikArp :: T.Text -> Either String (MacAddr, [IP])
parseMikrotikArp    = go . T.words
  where
    go :: [T.Text] -> Either String (MacAddr, [IP])
    go (_ : _ : x : y : _) = do
      ip <- A.parseOnly ipP x
      ma <- parseMacAddr y
      return (ma, [ip])
    go _ = Left "Huy"

parseLinuxArp :: T.Text -> Either String (MacAddr, [IP])
parseLinuxArp   = go . T.words
  where
    go :: [T.Text] -> Either String (MacAddr, [IP])
    go (_ : x : _ : y : _) = do
      ip <- A.parseOnly ipP x
      ma <- parseMacAddr y
      return (ma, [ip])
    go _ = Left "Huy"


parseLinuxArp :: [T.Text] -> Either String (MacAddr, [IP])
parseLinuxArp (_ : x : _ : y : _) = do
      ip <- A.parseOnly ipP x
      ma <- parseMacAddr y
      return (ma, [ip])
parseLinuxArp _ = Left "Huy"

{-parseLinuxArp :: MacIpMap -> T.Text -> MacIpMap
parseLinuxArp zs t = case T.words t of
    (_ : x : _ : y : _) -> either (const zs) (\(w, y) -> uncurry (M.insertWith addIp) (w, y) zs) $ do
      ip <- A.parseOnly ipP x
      ma <- parseMacAddr y
      return (ma, [ip])
    _                   -> zs-}-}

addIp :: [IP] -> [IP] -> [IP]
addIp xs zs0 = foldr (\x zs -> if x `elem` zs then zs else x : zs) zs0 xs

