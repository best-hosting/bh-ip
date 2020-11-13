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
import Control.Monad.Trans.RWS.CPS
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map as M
import System.IO.Unsafe
import System.Environment
import Control.Monad.Trans.Cont
import Control.Monad
import Data.Maybe

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
  deriving (Show)
type MacMap         = M.Map PortId (Maybe [MacAddr])
data TelnetRef      = TelnetRef
                        { switch :: SwName
                        , switchAuth :: AuthInfo
                        , telnetState :: TelnetState
                        , macMap :: MacMap
                        }
  deriving (Show)

data AuthInfo       = AuthInfo
                        { hostName :: HostName
                        , userName :: T.Text
                        , password :: T.Text
                        , enablePasword :: T.Text
                        }
  deriving (Show)

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
    ms' <- go s0 ai
    case ms' of
      Just s'
        | s' == Enabled -> atomicWriteIORef telnetRef r{telnetState = s'} >> k (con, ts)
        | otherwise     -> atomicWriteIORef telnetRef r{telnetState = s'}
      Nothing           -> k (con, ts)
  where
    -- | Return 'Nothing' if i don't understand state. And 'Just state'
    -- otherwise.
    go :: TelnetState -> AuthInfo -> IO (Maybe TelnetState)
    go Unauth ai = go AuthUsername ai
    go s@AuthUsername ai@AuthInfo{userName = user}
        | "Username" `T.isInfixOf` ts = do
            TL.telnetSend con . B8.pack $ T.unpack user ++ "\n"
            return (Just s)
        | "Password" `T.isInfixOf` ts = go Password ai
        | otherwise                 = return (Just s)
    go s@Password ai@AuthInfo{password = pw}
        | "Password" `T.isInfixOf` ts = do
            TL.telnetSend con . B8.pack $ T.unpack pw ++ "\n"
            return (Just s)
        | ">" `T.isSuffixOf` ts       = go Logged ai
        | otherwise                 = return (Just s)
    go s@Logged ai
        | ">" `T.isSuffixOf` ts       = do
            TL.telnetSend con . B8.pack $ "enable\n"
            return (Just s)
        | "Password" `T.isInfixOf` ts = go EnablePassword ai
        | otherwise                 = return (Just s)
    go s@EnablePassword AuthInfo{enablePasword = enpw}
        | "Password" `T.isInfixOf` ts = do
            TL.telnetSend con . B8.pack $ T.unpack enpw ++ "\n"
            return (Just s)
        | "#" `T.isSuffixOf` ts       = return (Just Enabled)
        | otherwise                 = return (Just s)
    go _ _ = return Nothing

getMacs :: TL.HasTelnetPtr t => (t, T.Text) -> ContT () IO ()
getMacs (con, ts) = liftIO $ do
    r0 <- readIORef telnetRef
    r' <- if "Invalid input detected" `T.isInfixOf` ts
            then go r0{telnetState = Exit}
            else go r0
    atomicWriteIORef telnetRef r'
  where
    go :: TelnetRef -> IO TelnetRef
    go r@TelnetRef {switch = curSw, telnetState = Enabled, macMap =  mm}
        | "#" `T.isSuffixOf` ts =
            let isPortUndef pid ms = swName pid == curSw && isNothing ms
            in  case (M.toList . M.filterWithKey isPortUndef $ mm) of
                  ((pid, _) : _) -> do
                    -- I need this, because response to previous 'show' command
                    -- may contain both command result ("Mac Address Table") and
                    -- cmd line prompt (with '#'). Thus, calling 'getMacs2' now
                    -- with 'ShowMacAddressTable p' state will result in parsing
                    -- previous port info as info for new (just selected) port
                    -- 'p'.
                    TL.telnetSend con . B8.pack $ "\n"
                    return r{telnetState = ShowMacAddressTable pid}
                  [] -> go r{telnetState = Exit}
        | otherwise = return r
    go r@TelnetRef {telnetState = ShowMacAddressTable pid, macMap = mm}
        | "Mac Address Table" `T.isInfixOf` ts = do
            putStrLn $ "save port " ++ show pid
            go r{telnetState = Enabled, macMap = M.insert pid (Just (parseMacs ts)) mm}
        | "#" `T.isSuffixOf` ts = do
            let SwPort pn = swPort pid
            putStrLn $ "show port " ++ show pn
            TL.telnetSend con . B8.pack $ "show mac address-table interface FastEthernet 0/" ++ show pn ++ "\n"
            return r
        | otherwise = return r
    go r@TelnetRef {telnetState = Exit}
        | "#" `T.isSuffixOf` ts   = do
            TL.telnetSend con . B8.pack $ "exit\n"
            return r
        | otherwise             = return r
    go _ = undefined

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

main :: IO ()
main    = do
    authinfo <- parseAuthInfo <$> T.readFile "authinfo.txt"
    print authinfo
    swports <- parseArgs <$> getArgs
    print swports
    atomicModifyIORef telnetRef (\r -> (r{macMap = swports}, ()))
    forM_ (M.keys swports) $ \(PortId {swName = sw}) ->
        case M.lookup sw authinfo of
          Just ai@AuthInfo{hostName = h} -> do
            print $ "Connect to " ++ show h
            atomicModifyIORef telnetRef $ \r ->
                ( r { switch = sw
                    , switchAuth = ai
                    , telnetState = Unauth
                    }
                , ()
                )
            connect h "23" (\(s, _) -> handle s)
          Nothing -> return ()
    TelnetRef {macMap = mm} <- readIORef telnetRef
    print $ "Gathered ac map:"
    print mm
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

parseArgs :: [String] -> MacMap
parseArgs = foldr go M.empty
  where
    go :: String -> MacMap -> MacMap
    go xs z = let (sn, '/' : sp) = span (/= '/') xs
              in  M.insert
                    (PortId {swName = SwName (T.pack sn), swPort = SwPort (read sp)})
                    Nothing
                    z

parseMacs :: T.Text -> [MacAddr]
parseMacs   = foldr go [] . T.lines
  where
    go :: T.Text -> [MacAddr] -> [MacAddr]
    go t zs = case (T.words t) of
        (_ : ma : _)
          | isNothing $ T.find (not . isMacChar) ma
                        -> MacAddr ma : zs
          | otherwise   -> zs
        _               -> zs
    isMacChar :: Char -> Bool
    isMacChar x = any ($ x) [isHexDigit, (== '.')]

