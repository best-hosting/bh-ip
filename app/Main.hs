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

data PortId         = PortId {swName :: SwName, swPort :: SwPort}
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
                        { switch :: SwName
                        , switchAuth :: AuthInfo
                        , telnetState :: TelnetState
                        , macMap :: PortMacMap
                        }
  deriving (Show)

data AuthInfo       = AuthInfo
                        { hostName :: HostName
                        , userName :: T.Text
                        , password :: T.Text
                        , enablePasword :: T.Text
                        }
  deriving (Show)

newtype IP          = IP T.Text
  deriving (Show)

parseIp :: T.Text -> Either String IP
parseIp t = IP <$> T.foldr go (Right T.empty) t
  where
    go :: Char -> Either String T.Text -> Either String T.Text
    go c mz
      | isDigit c   = T.cons c <$> mz
      | c == '.'    = T.cons '.' <$> mz
      | otherwise   = Left "Not an IP address."

type MacIpMap       = M.Map MacAddr [IP]

type PortMap        = M.Map PortId [(MacAddr, [IP])]

telnetRef :: IORef TelnetRef
{-# NOINLINE telnetRef #-}
telnetRef   = unsafePerformIO . newIORef
                $ TelnetRef { switch = undefined
                            , switchAuth = undefined
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
    r@TelnetRef {switchAuth = ai, telnetState = s0} <- readIORef telnetRef
    ms' <- runMaybeT . runReaderT (go s0) $ ai
    case ms' of
      Just s'
        | s' == Enabled -> atomicWriteIORef telnetRef r{telnetState = s'} >> k (con, ts)
        | otherwise     -> atomicWriteIORef telnetRef r{telnetState = s'}
      Nothing           -> k (con, ts)
  where
    -- | Return 'Nothing' if i don't understand state. And 'Just state'
    -- otherwise.
    go :: TelnetState -> ReaderT AuthInfo (MaybeT IO) TelnetState
    go Unauth = go AuthUsername
    go s@AuthUsername
        | "Username" `T.isInfixOf` ts = do
            AuthInfo{userName = user} <- ask
            liftIO $ TL.telnetSend con . B8.pack $ T.unpack user ++ "\n"
            return s
        | "Password" `T.isInfixOf` ts = go Password
        | otherwise                 = return s
    go s@Password
        | "Password" `T.isInfixOf` ts = do
            AuthInfo{password = pw} <- ask
            liftIO $ TL.telnetSend con . B8.pack $ T.unpack pw ++ "\n"
            return s
        | ">" `T.isSuffixOf` ts       = go Logged
        | otherwise                 = return s
    go s@Logged
        | ">" `T.isSuffixOf` ts       = do
            liftIO $ TL.telnetSend con . B8.pack $ "enable\n"
            return s
        | "Password" `T.isInfixOf` ts = go EnablePassword
        | otherwise                 = return s
    go s@EnablePassword
        | "Password" `T.isInfixOf` ts = do
            AuthInfo{enablePasword = enpw} <- ask
            liftIO $ TL.telnetSend con . B8.pack $ T.unpack enpw ++ "\n"
            return s
        | "#" `T.isSuffixOf` ts       = return Enabled
        | otherwise                 = return s
    go _ = fail "Unknown state"

getMacs :: TL.HasTelnetPtr t => (t, T.Text) -> ContT () IO ()
getMacs (con, ts) = liftIO $ do
    r0@TelnetRef{switch = curSw, telnetState = s0, macMap = mm0} <- readIORef telnetRef
    (s', mm') <- if "Invalid input detected" `T.isInfixOf` ts
            then flip runStateT mm0 . flip runReaderT curSw $ go Exit
            else flip runStateT mm0 . flip runReaderT curSw $ go s0
    atomicWriteIORef telnetRef r0{telnetState = s', macMap = mm'}
  where
    go :: TelnetState -> ReaderT SwName (StateT PortMacMap IO) TelnetState
    go s@Enabled
        | "#" `T.isSuffixOf` ts = do
            curSw <- ask
            mm    <- get
            let isPortUndef pid ms = swName pid == curSw && isNothing ms
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
        | "Mac Address Table" `T.isInfixOf` ts = do
            liftIO $ putStrLn $ "save port " ++ show pid
            modify (M.insert pid (Just (parseShowMacAddrTable ts)))
            go Enabled
        | "#" `T.isSuffixOf` ts = do
            let SwPort pn = swPort pid
            liftIO $ do
              putStrLn $ "show port " ++ show pn
              TL.telnetSend con . B8.pack $ "show mac address-table interface FastEthernet 0/" ++ show pn ++ "\n"
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

queryArp :: T.Text -> IO MacIpMap
queryArp host   = Sh.shelly . Sh.silently $
    Sh.runFoldLines M.empty go "ssh" [host, "/ip", "arp", "print"]
  where
    go :: MacIpMap -> T.Text -> MacIpMap
    go zs t = case T.words t of
      (_ : _ : x : y : _) -> either (const zs) (\(w, y) -> uncurry (M.insertWith (++)) (w, y) zs) $ do
        ip <- parseIp x
        ma <- parseMacAddr y
        return (ma, [ip])
      _                   -> zs

run :: ReaderT (M.Map SwName AuthInfo) (ExceptT String IO) PortMacMap
run = do
    authinfo <- ask
    swports  <- macMap <$> liftIO (readIORef telnetRef)
    forM_ (M.keys swports) $ \(PortId {swName = sw}) ->
        case M.lookup sw authinfo of
          Just ai@AuthInfo{hostName = h} -> liftIO $ do
            print $ "Connect to " ++ show h
            atomicModifyIORef telnetRef $ \r ->
                ( r { switch = sw
                    , switchAuth = ai
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
    authinfo <- parseAuthInfo <$> T.readFile "authinfo.txt"
    print authinfo
    swports <- parseArgs <$> getArgs
    print swports
    atomicModifyIORef telnetRef (\r -> (r{macMap = swports}, ()))
    emm <- runExceptT . flip runReaderT authinfo $ run
    case emm of
      Right mm -> do
        print $ "Gathered ac map:"
        print mm
        arp1 <- queryArp "r1"
        print arp1
        print "Finally, ipss..."
        print (getIPs mm arp1)
      Left err -> print err

parseAuthInfo :: T.Text -> M.Map SwName AuthInfo
parseAuthInfo   = M.fromList . map go .  T.lines
  where
    go :: T.Text -> (SwName, AuthInfo)
    go t =  let [sn, hn, un, pw, enpw] = T.splitOn ", " t
            in  ( SwName sn
                , AuthInfo  { hostName = T.unpack hn
                            , userName = un
                            , password = pw
                            , enablePasword = enpw
                            }
                )

parseArgs :: [String] -> PortMacMap
parseArgs = foldr go M.empty
  where
    go :: String -> PortMacMap -> PortMacMap
    go xs z = let (sn, '/' : sp) = span (/= '/') xs
              in  M.insert
                    (PortId {swName = SwName (T.pack sn), swPort = SwPort (read sp)})
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

