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

data SwPort         = SwPort Int
  deriving (Eq, Ord, Show)
newtype SwName      = SwName T.Text
 deriving (Eq, Ord, Show)

data PortId         = PortId {portSw :: SwName, port :: SwPort}
  deriving (Eq, Ord, Show)

data TelnetState    = Unauth
                    | AuthUsername
                    | Password
                    | Logged
                    | EnableRequested
                    | EnablePassword
                    | Enabled
                    | ShowMacAddressTable PortId
                    | Exit
  deriving (Eq, Show)

newtype MacAddr     = MacAddr T.Text
  deriving (Eq, Ord)

instance Show MacAddr where
    showsPrec d (MacAddr t) = showMac (T.unpack t)
      where
        showMac :: String -> ShowS
        showMac t r = foldr (\(n, x) zs ->
                                if n > 1 && n `mod` 2 == 1 then ':' : x : zs else x : zs)
                            r
                        . zip [1..12]
                        $ t

parseMacAddr :: T.Text -> Either String MacAddr
parseMacAddr t = MacAddr <$> T.foldr go end t 1
  where
    go :: Char -> (Int -> Either String T.Text) -> Int -> Either String T.Text
    go c zf n
      | n > 12              = Left "Too many chars for mac address."
      | isHexDigit c        = let mz = zf (n + 1) in (T.cons . toLower $ c) <$> mz
      | c `elem` [':', '.'] = zf n
      | otherwise           = Left "Not a mac address."
    end :: Int -> Either String T.Text
    end n
      | n /= 13             = Left "Too few chars for mac address."
      | otherwise           = Right T.empty


type PortMacMap     = M.Map PortId (Maybe [MacAddr])
data TelnetRef      = TelnetRef
                        { switchInfo :: SwInfo
                        , telnetState :: TelnetState
                        , macMap :: PortMacMap
                        }
  deriving (Show)

data SwInfo         = SwInfo
                        { swName   :: SwName
                        , hostName :: HostName
                        , userName :: T.Text
                        , password :: T.Text
                        , enablePasword :: T.Text
                        , defaultPortSpec :: T.Text
                        }
  deriving (Show)

newtype IP          = IP (Int, Int, Int, Int)
  deriving (Eq)

instance Show IP where
    showsPrec d ip = (showIP ip ++)
      where
        showIP :: IP -> String
        showIP (IP (o1, o2, o3, o4)) =
            show o1 ++ "." ++ show o2 ++ "." ++ show o3 ++ "." ++ show o4

parseIP :: T.Text -> Either String IP
parseIP t = do
    os <- mapM parseOctet . T.split (== '.') $ t
    case os of
      [o1, o2, o3, o4]  -> Right (IP (o1, o2, o3, o4))
      _                 -> Left "Too few or too many octets."
  where
    parseOctet :: T.Text -> Either String Int
    parseOctet ds = case reads (T.unpack ds) of
        [(d, "")]
          | 0 <= d && d <= 255  -> Right d
          | otherwise           -> Left $ "Incorrect IP octet '" ++ show d ++ "'"
        []                      -> Left "Can't read IP octet."

type MacIpMap       = M.Map MacAddr [IP]

type PortMap        = M.Map PortId [(MacAddr, [IP])]

telnetRef :: IORef TelnetRef
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

telnetLogin :: TL.HasTelnetPtr t => t -> T.Text -> ContT () IO (t, T.Text)
telnetLogin con ts = shiftT $ \k -> liftIO $ do
    r@TelnetRef {switchInfo = swInfo, telnetState = s0} <- readIORef telnetRef
    ms' <- runMaybeT . runReaderT (go s0) $ swInfo
    case ms' of
      Just s'
        | s' == Enabled -> atomicWriteIORef telnetRef r{telnetState = s'} >> k (con, ts)
        | otherwise     -> atomicWriteIORef telnetRef r{telnetState = s'}
      Nothing           -> k (con, ts)
  where
    -- | Return 'Nothing' if i don't understand state. And 'Just state'
    -- otherwise.
    go :: TelnetState -> ReaderT SwInfo (MaybeT IO) TelnetState
    go Unauth = go AuthUsername
    go s@AuthUsername
        | "Username" `T.isInfixOf` ts || "User Name" `T.isInfixOf` ts = do
            SwInfo{userName = user} <- ask
            liftIO $ TL.telnetSend con . B8.pack $ T.unpack user ++ "\n"
            return s
        | "Password" `T.isInfixOf` ts = go Password
        | otherwise                 = return s
    go s@Password
        | "Password" `T.isInfixOf` ts = do
            SwInfo{password = pw} <- ask
            liftIO $ TL.telnetSend con . B8.pack $ T.unpack pw ++ "\n"
            return s
        | ">" `T.isSuffixOf` ts       = go Logged
        | "#" `T.isSuffixOf` ts       = return Enabled
        | otherwise                 = return s
    go s@Logged
        | ">" `T.isSuffixOf` ts       = do
            liftIO $ TL.telnetSend con . B8.pack $ "enable\n"
            return s
        | "Password" `T.isInfixOf` ts = go EnablePassword
        | otherwise                 = return s
    go s@EnablePassword
        | "Password" `T.isInfixOf` ts = do
            SwInfo{enablePasword = enpw} <- ask
            liftIO $ TL.telnetSend con . B8.pack $ T.unpack enpw ++ "\n"
            return s
        | "#" `T.isSuffixOf` ts       = return Enabled
        | otherwise                 = return s
    go _ = fail "Unknown state"

getMacs :: TL.HasTelnetPtr t => (t, T.Text) -> ContT () IO ()
getMacs (con, ts) = liftIO $ do
    r0@TelnetRef{switchInfo = swInfo, telnetState = s0, macMap = mm0} <- readIORef telnetRef
    (s', mm') <- if "Invalid input detected" `T.isInfixOf` ts
            then flip runStateT mm0 . flip runReaderT swInfo $ go Exit
            else flip runStateT mm0 . flip runReaderT swInfo $ go s0
    atomicWriteIORef telnetRef r0{telnetState = s', macMap = mm'}
  where
    go :: TelnetState -> ReaderT SwInfo (StateT PortMacMap IO) TelnetState
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
                return (ShowMacAddressTable pid)
              [] -> go Exit
        | otherwise = return s
    go s@(ShowMacAddressTable pid)
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
    evalContT (telnetLogin t (T.decodeLatin1 b) >>= getMacs)
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

queryArp :: Sh.FoldCallback MacIpMap -> T.Text -> IO MacIpMap
queryArp go host   = Sh.shelly . Sh.silently $
    Sh.runFoldLines M.empty go "ssh" [host, "/ip", "arp", "print"]

parseMikrotikArp :: MacIpMap -> T.Text -> MacIpMap
parseMikrotikArp zs t = case T.words t of
    (_ : _ : x : y : _) ->
      either (const zs) (\(w, y) -> uncurry (M.insertWith addIp) (w, y) zs) $ do
        ip <- parseIP x
        ma <- parseMacAddr y
        return (ma, [ip])
    _                   -> zs

parseLinuxArp :: MacIpMap -> T.Text -> MacIpMap
parseLinuxArp zs t = case T.words t of
    (_ : x : _ : y : _) -> either (const zs) (\(w, y) -> uncurry (M.insertWith addIp) (w, y) zs) $ do
      ip <- parseIP x
      ma <- parseMacAddr y
      return (ma, [ip])
    _                   -> zs

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
    emm <- runExceptT . flip runReaderT swInfo $ run
    case emm of
      Right mm -> do
        print $ "Gathered ac map:"
        print mm
        arp1 <- queryArp parseMikrotikArp "r1"
        print arp1
        print "Finally, ips..."
        mapM_ (putStrLn . show) (getIPs mm arp1)
      Left err -> print err

parseSwInfo :: T.Text -> M.Map SwName SwInfo
parseSwInfo   = M.fromList . map go .  T.lines
  where
    go :: T.Text -> (SwName, SwInfo)
    go t =  let [sn, hn, un, pw, enpw, ds] = T.splitOn ", " t
            in  ( SwName sn
                , SwInfo    { swName   = SwName sn
                            , hostName = T.unpack hn
                            , userName = un
                            , password = pw
                            , enablePasword = enpw
                            , defaultPortSpec = ds
                            }
                )

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

