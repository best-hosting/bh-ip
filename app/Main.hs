{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.IO.Class
import qualified Data.Map as M
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

import BH.IP
import BH.Switch
import BH.Telnet

getMacs :: TelnetCmd [SwPort] (Maybe (M.Map SwPort [MacAddr])) ()
getMacs t0 = do
    curSn <- asks (swName . switchInfo)
    ps    <- asks (filter ((== curSn) . portSw) . telnetIn)
    foldM (flip go) t0 ps >>= sendExit
  where
    go :: SwPort -> T.Text -> ContT () (ReaderT (TelnetInfo [SwPort] (Maybe (M.Map SwPort [MacAddr]))) IO) T.Text
    -- FIXME: func type.
    --go :: SwPort -> T.Text -> TelnetCmd [SwPort] (Maybe (M.Map SwPort [MacAddr]) T.Text
    go pid@SwPort{portSpec = pn} ts =
        sendAndParse (parse <$> parseMacAddrTable)
          (defCmd $ "show mac address-table interface " <> ciscoPortNum pn) ts
      where
        parse :: [PortInfoEl] -> Maybe (M.Map SwPort [MacAddr])
        parse xs = if null xs
                      then Nothing
                      else Just $ M.singleton pid (map (either error id . parseMacAddr . elMac) xs)


queryMikrotikArp :: T.Text -> IO MacIpMap
queryMikrotikArp host   = Sh.shelly . Sh.silently $
    Sh.runFoldLines M.empty (\zs -> go zs . T.words) "ssh" [host, "/ip", "arp", "print"]
  where
    go :: MacIpMap -> [T.Text] -> MacIpMap
    go zs (_ : _ : x : y : _) =
        either (const zs) (\(w, t) -> uncurry (M.insertWith addIp) (w, t) zs) $ do
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
        liftIO $ putStrLn "Updating arp cache using `nping` and `ip neighbour`..."
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
        go _ _          = Left "Unrecognized `ip neigh` output line."
    nmapCache :: ExceptT String IO MacIpMap
    nmapCache = do
        liftIO $ putStrLn "Updating arp cache using `nmap`..."
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
      Just mm <- flip runReaderT swInfo $ run (M.keys swports) getMacs (portSw sw)
      --mm <-  flip runReaderT swInfo $ Main.run
      --mm <-  flip runReaderT swInfo $ runOn
      liftIO $ putStrLn "Gathered ac map:"
      liftIO $ print mm
      arp1 <- queryLinuxArp "certbot"
      liftIO $ putStrLn "Finally, ips..."
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
                    -- FIXME: Wrong port speed.
                    (SwPort {portSw = swn, portSpec = PortNum {portSpeed = FastEthernet, portSlot = 0, portNumber = read sp}})
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

