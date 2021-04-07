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


data TelnetShowMac = ShowMacAddressTable PortId | Huy
  deriving (Show, Eq)

type TelnetIORef a = IORef (TelnetRef a)

class Eq a => HasTelnetRef a where
    telnetRef2 :: TelnetIORef a
    emptyref :: a

instance HasTelnetRef TelnetShowMac where
    {-# NOINLINE telnetRef2 #-}
    telnetRef2   = unsafePerformIO . newIORef
                    $ TelnetRef { switchInfo = undefined
                                , telnetState = Unauth
                                , macMap = M.empty
                                }
    emptyref = Huy

telnetRef :: IORef (TelnetRef TelnetShowMac)
{-# NOINLINE telnetRef #-}
telnetRef   = unsafePerformIO . newIORef
                $ TelnetRef { switchInfo = undefined
                            , telnetState = Unauth
                            , macMap = M.empty
                            }



{-t :: ContT Int IO Int
t   = do
        z <- resetT $ do
            x <- shiftT $ (\k -> liftIO (k 7 >>= k))
            let y = x + 1
            return y
        let w = z * 10
        return w
-}

getMacs :: TL.HasTelnetPtr t => (t, T.Text) -> TelnetCtx TelnetRef TelnetShowMac ()
getMacs (con, ts) = do
    tRef <- ask
    liftIO $ do
    r0@TelnetRef{switchInfo = swInfo, telnetState = s0, macMap = mm0} <- readIORef tRef
    (s', mm') <- if "Invalid input detected" `T.isInfixOf` ts
            then flip runStateT mm0 . flip runReaderT swInfo $ go Exit
            else flip runStateT mm0 . flip runReaderT swInfo $ go s0
    atomicWriteIORef tRef r0{telnetState = s', macMap = mm'}
  where
    go :: (TelnetState TelnetShowMac) -> ReaderT SwInfo (StateT PortMacMap IO) (TelnetState TelnetShowMac)
    go s@Enabled
        | "#" `T.isSuffixOf` ts = do
            curSw <- asks swName
            mm    <- get
            let isPortUndef pid ms = portSw pid == curSw && isNothing ms
            case (M.toList . M.filterWithKey isPortUndef $ mm) of
              ((pid, _) : _) -> do
                -- I need this, because response to previous 'show' command
                -- may contain both command result ("Mac Address Table") and
                -- cmd line prompt (with '#'). Thus, calling 'getMacs2' now
                -- with 'ShowMacAddressTable p' state will result in parsing
                -- previous port info as info for new (just selected) port
                -- 'p'.
                liftIO $ TL.telnetSend con . B8.pack $ "\n"
                return (Command (ShowMacAddressTable pid))
              [] -> go Exit
        | otherwise = return s
    go s@(Command (ShowMacAddressTable pid))
        | "Mac Address Table" `T.isInfixOf` ts || "Mac Address" `T.isInfixOf` ts = do
            liftIO $ putStrLn $ "save port " ++ show pid
            modify (M.insert pid (Just (parseShowMacAddrTable ts)))
            go Enabled
        | "#" `T.isSuffixOf` ts = do
            portSpec <- asks defaultPortSpec
            let SwPort pn = port pid
            liftIO $ do
              putStrLn $ "show port " ++ show pn
              TL.telnetSend con . B8.pack $
                    "show mac address-table interface "
                    ++ T.unpack portSpec ++ show pn ++ "\n"
            return s
        | otherwise = return s
    go s@Exit
        | "#" `T.isSuffixOf` ts   = do
            liftIO $ TL.telnetSend con . B8.pack $ "exit\n"
            return s
        | otherwise             = return s
    go _ = undefined
    parseShowMacAddrTable :: T.Text -> [MacAddr]
    parseShowMacAddrTable = foldr go [] . T.lines
      where
        go :: T.Text -> [MacAddr] -> [MacAddr]
        go t zs = case (T.words t) of
            (_ : x : _)  -> either (const zs) (: zs) (parseMacAddr x)
            _            -> zs

telnetH :: Socket -> TL.EventHandler
telnetH _ t (TL.Received b)
  = do
    putStr "R: "
    B8.putStrLn b
    flip runReaderT telnetRef . evalContT $ (telnetLogin t (T.decodeLatin1 b) >>= getMacs)
telnetH s _ (TL.Send b)
  = putStr "S: " *> B8.putStrLn b *> print (L.map ord (B8.unpack b)) *> send s b
telnetH _ _ (TL.Do o)
  = putStr $ "DO " ++ show o ++ "\n"
telnetH _ _ (TL.Dont o)
  = putStr $ "DON'T " ++ show o ++ "\n"
telnetH _ _ (TL.Will o)
  = putStr $ "Will " ++ show o ++ "\n"
telnetH _ _ (TL.Wont o)
  = putStr $ "WON'T " ++ show o ++ "\n"
telnetH _ _ (TL.Iac i)
  = putStr $ "IAC " ++ show i ++ "\n"
telnetH _ _ _ = pure ()

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

run :: ReaderT (M.Map SwName SwInfo) (ExceptT String IO) PortMacMap
run = do
    swports <- macMap <$> liftIO (readIORef telnetRef)
    forM_ (M.keys swports) $ \(PortId {portSw = sw}) -> do
        mSwInfo <- asks (M.lookup sw)
        case mSwInfo of
          Just swInfo@SwInfo{hostName = h} -> liftIO $ do
            print $ "Connect to " ++ show h
            atomicModifyIORef telnetRef $ \r ->
                ( r { switchInfo = swInfo
                    , telnetState = Unauth
                    }
                , ()
                )
            connect h "23" (\(s, _) -> handle s)
          Nothing -> fail $ "No auth info for switch: '" ++ show sw ++ "'"
    macMap <$> liftIO (readIORef telnetRef)
  where
    handle :: Socket -> IO ()
    handle sock = do
        telnet <- TL.telnetInit telnetOpts [] (telnetH sock)
        whileJust_ (recv sock 4096) $ \bs -> do
            let bl = B8.length bs
            putStr $ "Socket (" ++ show bl ++ "): "
            B8.putStrLn bs
            TL.telnetRecv telnet bs

    telnetOpts :: [TL.OptionSpec]
    telnetOpts  = [ TL.OptionSpec TL.optEcho True True
                  --, TL.OptionSpec TL.optLineMode True True
                  ]


main :: IO ()
main    = do
    swInfo <- parseSwInfo <$> T.readFile "authinfo.txt"
    print swInfo
    swports <- parseArgs <$> getArgs
    print swports
    atomicModifyIORef telnetRef (\r -> (r{macMap = swports}, ()))
    res <- runExceptT $ do
      mm <-  flip runReaderT swInfo $ run
      liftIO $ print $ "Gathered ac map:"
      liftIO $ print mm
      arp1 <- queryLinuxArp "certbot"
      liftIO $ print "Finally, ips..."
      liftIO $ mapM_ (putStrLn . show) (getIPs mm arp1)
    case res of
      Right () -> return ()
      Left err -> print err

parseArgs :: [String] -> PortMacMap
parseArgs = foldr go M.empty
  where
    go :: String -> PortMacMap -> PortMacMap
    go xs z = let (sn, '/' : sp) = span (/= '/') xs
              in  M.insert
                    (PortId {portSw = SwName (T.pack sn), port = SwPort (read sp)})
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

