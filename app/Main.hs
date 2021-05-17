{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Loops
import qualified Data.ByteString.Char8 as B8
import           Network.Simple.TCP
import qualified Network.Telnet.LibTelnet as TL
import qualified Network.Telnet.LibTelnet.Options as TL
import qualified Data.List as L
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map as M
import System.IO.Unsafe
import System.Environment
import Control.Monad.Trans.Cont
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad
import Data.Maybe
import Data.Foldable
import qualified Shelly as Sh
import Control.Concurrent
import qualified Data.Yaml as Y
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding as A
import System.Directory
import Text.HTML.TagSoup

import BH.IP
import BH.Switch

parseShowMacAddrTable :: T.Text -> [MacAddr]
parseShowMacAddrTable = foldr go [] . T.lines
  where
    go :: T.Text -> [MacAddr] -> [MacAddr]
    go t zs = case (T.words t) of
        (_ : x : _)  -> either (const zs) (: zs) (parseMacAddr x)
        _            -> zs

getMacs2 :: TelnetCmd [SwPort] (M.Map SwPort [MacAddr]) ()
getMacs2 ts0 = do
    curSn <- asks (swName . switchInfo)
    ps    <- asks (filter ((== curSn) . portSw) . telnetIn)
    foldM (flip go) ts0 ps >>= sendTelnetExit
  where
    go :: SwPort -> T.Text -> ContT () (ReaderT (TelnetInfo [SwPort] (M.Map SwPort [MacAddr])) IO) T.Text
    go pid@SwPort{port = PortNum pn, portSpec = ps} ts = do
        sendAndParseTelnetCmd parse
          (defCmd ("show mac address-table interface " <> ps <> T.pack (show pn))) ts
      where
        parse xs mz = let ys = parseShowMacAddrTable xs
                      in  if null ys
                            then Partial mz
                            else Final (Just (M.singleton pid ys) <> mz) (last $ T.lines ts)


queryMikrotikArp :: T.Text -> IO MacIpMap
queryMikrotikArp host   = Sh.shelly . Sh.silently $
    Sh.runFoldLines M.empty (\zs -> go zs . T.words) "ssh" [host, "/ip", "arp", "print"]
  where
    go :: MacIpMap -> [T.Text] -> MacIpMap
    go zs (_ : _ : x : y : _) =
        either (const zs) (\(w, y) -> uncurry (M.insertWith addIp) (w, y) zs) $ do
          ip <- parseIP x
          ma <- parseMacAddr y
          return (ma, [ip])
    go zs _                   = zs

macIpMapFile :: FilePath
macIpMapFile    = "mac-ip-cache.yml"

-- Use 'ip neigh'.
queryLinuxArp :: T.Text -> ExceptT String IO MacIpMap
queryLinuxArp host   = do
    b <- liftIO $ doesFileExist macIpMapFile
    if b
      then catchE (ExceptT $ Y.decodeFileEither macIpMapFile) $ \e -> do
        liftIO (print e)
        updateArpCache
      else updateArpCache
  where
    updateArpCache :: ExceptT String IO MacIpMap
    updateArpCache  = do
        --mi <- nmapCache
        mi <- ipNeighCache
        liftIO $ Y.encodeFile macIpMapFile mi
        return mi
    ipNeighCache :: ExceptT String IO MacIpMap
    ipNeighCache = ExceptT . Sh.shelly . Sh.silently $ do
        liftIO $ print "Updating arp cache using `nping` and `ip neighbour`..."
        Sh.run_ "ssh"
                (host : T.words "nping --quiet -N --rate=100 -c1 213.108.248.0/21")
        liftIO $ threadDelay 5000000
        mi <- Sh.runFoldLines (return M.empty) (\zs -> go zs . T.words) "ssh"
                (host : T.words "ip neighbour show nud reachable nud stale")
        Sh.run_ "ssh" (host : T.words "ip neighbour flush all")
        return mi
      where
        go :: Either String MacIpMap -> [T.Text] -> Either String MacIpMap
        go mzs (x : _ : _ : _ : y : s : _)
          | s == "REACHABLE" || s == "STALE" = do
            zs <- mzs
            ip <- parseIP x
            ma <- parseMacAddr y
            return (M.insertWith addIp ma [ip] zs)
          | otherwise   = mzs
        go mzs _        = Left "Unrecognized `ip neigh` output line."
    nmapCache :: ExceptT String IO MacIpMap
    nmapCache = do
        liftIO $ print "Updating arp cache using `nmap`..."
        nxml <- liftIO . Sh.shelly . Sh.silently $ do
          Sh.run_ "ssh" (host : T.words "nmap -sn -PR -oX nmap_arp_cache.xml 213.108.248.0/21")
          Sh.run  "ssh" (host : T.words "cat ./nmap_arp_cache.xml")
        let emi = mapM go
                    . sections (~== ("<status state=\"up\" reason=\"arp-response\">" :: String))
                    . parseTags
                    $ nxml
        M.fromListWith addIp <$> ExceptT (return emi)
      where
        go :: [Tag T.Text] -> Either String (MacAddr, [IP])
        go xs =
            let oneHost = filter (~== ("<address>" :: String)) . takeWhile (~/= ("</host>" :: String))
            in  case oneHost xs of
                  [x, y]  -> do
                              ip  <- parseIP (fromAttrib "addr" x)
                              mac <- parseMacAddr (fromAttrib "addr" y)
                              return (mac, [ip])
                  _       -> Left "Unrecognized ip, mac pair in nmap output."



{-

{-parseMikrotikArp :: [T.Text -> MacIpMap
parseMikrotikArp zs t = case T.words t of
    (_ : _ : x : y : _) ->
      either (const zs) (\(w, y) -> uncurry (M.insertWith addIp) (w, y) zs) $ do
        ip <- parseIP x
        ma <- parseMacAddr y
        return (ma, [ip])
    _                   -> zs-}

parseMikrotikArp :: T.Text -> Either String (MacAddr, [IP])
parseMikrotikArp    = go . T.words
  where
    go :: [T.Text] -> Either String (MacAddr, [IP])
    go (_ : _ : x : y : _) = do
      ip <- parseIP x
      ma <- parseMacAddr y
      return (ma, [ip])
    go _ = Left "Huy"

parseLinuxArp :: T.Text -> Either String (MacAddr, [IP])
parseLinuxArp   = go . T.words
  where
    go :: [T.Text] -> Either String (MacAddr, [IP])
    go (_ : x : _ : y : _) = do
      ip <- parseIP x
      ma <- parseMacAddr y
      return (ma, [ip])
    go _ = Left "Huy"


parseLinuxArp :: [T.Text] -> Either String (MacAddr, [IP])
parseLinuxArp (_ : x : _ : y : _) = do
      ip <- parseIP x
      ma <- parseMacAddr y
      return (ma, [ip])
parseLinuxArp _ = Left "Huy"

{-parseLinuxArp :: MacIpMap -> T.Text -> MacIpMap
parseLinuxArp zs t = case T.words t of
    (_ : x : _ : y : _) -> either (const zs) (\(w, y) -> uncurry (M.insertWith addIp) (w, y) zs) $ do
      ip <- parseIP x
      ma <- parseMacAddr y
      return (ma, [ip])
    _                   -> zs-}-}

addIp :: [IP] -> [IP] -> [IP]
addIp xs zs0 = foldr (\x zs -> if x `elem` zs then zs else x : zs) zs0 xs

main :: IO ()
main    = do
    swInfo <- parseSwInfo <$> T.readFile "authinfo.txt"
    print swInfo
    swports <- parseArgs swInfo <$> getArgs
    print swports
    let sw = head . M.keys $ swports
    res <- runExceptT $ do
      Just mm <- flip runReaderT swInfo $ run (M.keys swports) getMacs2 (portSw sw)
      --mm <-  flip runReaderT swInfo $ Main.run
      --mm <-  flip runReaderT swInfo $ runOn
      liftIO $ print $ "Gathered ac map:"
      liftIO $ print mm
      arp1 <- queryLinuxArp "certbot"
      liftIO $ print "Finally, ips..."
      liftIO $ print (macsToIPs arp1 mm)
    case res of
      Right () -> return ()
      Left err -> print err

parseArgs :: M.Map SwName SwInfo -> [String] -> PortMacMap
parseArgs swInfo = foldr go M.empty
  where
    go :: String -> PortMacMap -> PortMacMap
    go xs z = let (sn, '/' : sp) = span (/= '/') xs
                  swn = SwName (T.pack sn)
                  ps = maybe "huy" defaultPortSpec (M.lookup swn swInfo)
              in  M.insert
                    (SwPort {portSw = swn, portSpec = ps, port = PortNum (read sp)})
                    Nothing
                    z

getIPs :: PortMacMap -> MacIpMap -> [IP]
getIPs portMac macIp = foldr go [] portMac
  where
    go :: Maybe [MacAddr] -> [IP] -> [IP]
    go Nothing zs   = zs
    go (Just ms) zs = foldr goMacs zs ms
    goMacs :: MacAddr -> [IP] -> [IP]
    goMacs m zs = maybe zs (++ zs) (M.lookup m macIp)

macsToIPs :: MacIpMap -> M.Map SwPort [MacAddr] -> M.Map SwPort [IP]
macsToIPs macIp = M.map (foldr go [])
  where
    go :: MacAddr -> [IP] -> [IP]
    go m zs = fromMaybe [] (M.lookup m macIp) ++ zs

