{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module BH.Switch
    ( SwName (..)
    , SwInfo (..)
    , parseSwInfo
    , SwPort (..)
    , PortId (..)
    , PortMacMap
    , MacPortMap
    , SwConfig
    , TelnetRef4 (..)
    , TelnetCmd
    , telnetRef4
    , CmdReader (..)
    , MacIpMap
    , PortMap
    , shiftW
    , saveResume
    , modifyResult
    , saveResult
    , finishCmd
    , run
    , runOn
    , runTill
    , sendTelnetCmd
    , sendAndParseTelnetCmd
    , sendTelnetExit
    , parseTelnetCmdOut
    )
  where

import qualified Data.Text as T

import           Control.Monad.Loops
import qualified Data.ByteString.Char8 as B8
import           Network.Simple.TCP
import qualified Network.Telnet.LibTelnet.Options as TL
import qualified Data.List as L
import Data.Char
import qualified Data.Text.Encoding as T
import Data.IORef
import qualified Data.Map as M
import System.IO.Unsafe
import Control.Monad.Trans.Cont
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.Foldable
import Data.Maybe

import qualified Network.Telnet.LibTelnet as TL

import BH.IP


newtype SwName      = SwName T.Text
 deriving (Eq, Ord, Show)

data SwInfo         = SwInfo
                        { swName   :: SwName
                        , hostName :: HostName
                        , userName :: T.Text
                        , password :: T.Text
                        , enablePassword :: T.Text
                        , defaultPortSpec :: T.Text
                        }
  deriving (Show)

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
                            , enablePassword = enpw
                            , defaultPortSpec = ds
                            }
                )

-- FIXME: Rename to PortNum .
data SwPort         = SwPort Int
  deriving (Eq, Ord, Show)

-- FIXME: Rename to SwPort .
data PortId         = PortId {portSw :: SwName, port :: SwPort}
  deriving (Eq, Ord, Show)

type TelnetCmd a b = T.Text -> ContT () (ReaderT (CmdReader a b) IO) ()
data TelnetRef4 a b  = TelnetRef4
                        { tFinal  :: Maybe b
                        , tResume :: Maybe (T.Text -> ReaderT (CmdReader a b) IO ())
                        , tInt :: Int
                        , tEcho :: T.Text
                        , tPrompt :: T.Text
                        }

defTelnetRef4 :: TelnetRef4 a b
defTelnetRef4 = TelnetRef4  { tResume = Nothing
                            , tFinal = Nothing
                            , tInt = 0
                            , tEcho = T.empty
                            , tPrompt = T.empty
                            }

telnetRef4 :: IORef (TelnetRef4 a b)
{-# NOINLINE telnetRef4 #-}
telnetRef4  = unsafePerformIO (newIORef defTelnetRef4)

-- FIXME: Query, result and program (telnet commands) are all bound together.
-- I.e. if i query by IP, i definitely need 'findPort' and result will be
-- switch port. Etc. That means, i may group this triple: query type, result
-- type and actual function to execute after login into single class instance.
--
-- That been said, the 'config save' operation is different: it does not have
-- a query. Though, if i consider all this not as queries/response, but just
-- like input/program/output, then..
data CmdReader a b = CmdReader { switchInfo4 :: SwInfo
                             , tCon :: TL.TelnetPtr
                             , telnetIn :: a
                             , telRef :: IORef (TelnetRef4 a b)
                             }


type PortMacMap     = M.Map PortId (Maybe [MacAddr])

type MacPortMap     = M.Map MacAddr (Maybe [PortId])

type MacIpMap       = M.Map MacAddr [IP]

type PortMap        = M.Map PortId [(MacAddr, [IP])]

type SwConfig       = M.Map SwName T.Text










-- | 'shiftT' versino suitable for chaining with '>>='. I just pass previous
-- monad result to shiftT's function.
shiftW :: Monad m => ((a -> m r, b) -> ContT r m r) -> b -> ContT r m a
shiftW f x = shiftT (\k -> f (k, x))

-- | Send telnet command and wait until it'll be echo-ed back.
sendTelnetCmd :: T.Text -> T.Text -> ContT () (ReaderT (CmdReader a b) IO) T.Text
sendTelnetCmd = sendAndParseTelnetCmd (flip const)

-- FIXME: Rename to just 'sendAndParse'
sendAndParseTelnetCmd :: (T.Text -> Maybe b -> Maybe b) -> T.Text -> T.Text -> ContT () (ReaderT (CmdReader a b) IO) T.Text
sendAndParseTelnetCmd f cmd t0 =
    shiftW (\(k, ts) -> do
        con  <- asks tCon
        when ("#" `T.isSuffixOf` ts || ">" `T.isSuffixOf` ts) $ do
          liftIO $ TL.telnetSend con . B8.pack $ T.unpack cmd <> "\n"
          saveResume k
      ) t0 >>=
    parseEcho cmd >>=
    parseTelnetCmdOut f >>=
    parsePrompt

-- | Parse cmd echo-ed back.
parseEcho :: T.Text -> T.Text -> ContT () (ReaderT (CmdReader a b) IO) T.Text
parseEcho cmd = shiftW $ \(k, ts) -> do
      tRef <- asks telRef
      r <- liftIO (readIORef tRef)
      let echoCmd = tEcho r <> ts
      liftIO $ print $ "Command echo-ed back: " <> echoCmd
      liftIO $ print $ "Original was: " <> cmd
      if cmd `T.isInfixOf` echoCmd
        then do
          liftIO $ atomicModifyIORef tRef (\x -> (x{tEcho = T.empty}, ()))
          liftIO $ print $ "Command echo complete"
          saveResume k
          lift (k ts)
        else liftIO $ atomicModifyIORef tRef (\x -> (x{tEcho = echoCmd}, ()))

parsePrompt :: T.Text -> ContT () (ReaderT (CmdReader a b) IO) T.Text
parsePrompt ts = do
      tRef <- asks telRef
      r <- liftIO (readIORef tRef)
      parseEcho (tPrompt r) ts

-- | Gather result and then proceed to next command immediately.
parseTelnetCmdOut :: (T.Text -> Maybe b -> Maybe b)
                      -> T.Text -> ContT () (ReaderT (CmdReader a b) IO) T.Text
parseTelnetCmdOut f = shiftW $ \(k, ts) -> do
    liftIO $ print "Go parsing"
    if "#" `T.isSuffixOf` ts
      then do
        modifyResult (f ts)
        saveResume k
        lift (k ts)
      else do
        modifyResult (f ts)
        liftIO $ print "Retry parsing.."

sendTelnetExit :: T.Text -> ContT () (ReaderT (CmdReader a b) IO) ()
sendTelnetExit = (\_ -> pure ()) <=< sendTelnetCmd "exit"

saveResume :: MonadIO m => (T.Text -> ReaderT (CmdReader a b) IO ())
              -> ContT () (ReaderT (CmdReader a b) m) ()
saveResume k = do
    tRef <- asks telRef
    liftIO $ atomicModifyIORef tRef (\r -> (r{tResume = Just k}, ()))

-- | Modify result. If there's not result yet, initialize it with empty value.
modifyResult :: MonadIO m => (Maybe b -> Maybe b) -> ContT () (ReaderT (CmdReader a b) m) ()
modifyResult f = do
    tRef <- asks telRef
    liftIO $ atomicModifyIORef tRef (\r -> (r{tFinal = f (tFinal r)}, ()))

saveResult :: MonadIO m => b -> ContT () (ReaderT (CmdReader a b) m) ()
saveResult x = do
    tRef <- asks telRef
    liftIO $ atomicModifyIORef tRef (\r -> (r{tFinal = Just x}, ()))

-- FIXME: Do i need this?
finishCmd :: MonadIO m => ContT () (ReaderT (CmdReader a b) m) ()
finishCmd = do
    tRef <- asks telRef
    liftIO $ atomicModifyIORef tRef (\r -> (r{tResume = Just (\_ -> pure ())}, ()))

runCmd :: T.Text
          -> (TelnetCmd a b)
          -> ContT () (ReaderT (CmdReader a b) IO) ()
runCmd ts cmd = do
    tRef <- asks telRef
    r0 <- liftIO (readIORef tRef)
    let mCont = tResume r0
        n0 = tInt r0
    liftIO $ print $ "f start: " ++ show n0
    liftIO $ atomicWriteIORef tRef r0{tInt = n0 + 1}
    shiftT $ \end -> do
      case mCont of
        Nothing -> liftIO (print "huy")  >> cmd ts
        Just c  -> liftIO (print "cont") >> lift (c ts)

-- FIXME: Rewrite login to sendTelnetCmd, etc.
loginCmd :: T.Text -> ContT () (ReaderT (CmdReader a b) IO) T.Text
loginCmd ts0 = shiftT $ \finish -> do
    con  <- asks tCon
    -- shiftT stops execution, if supplied continuation is _not_ called. I
    -- don't need any other "suspend mechanisms" apart from plain 'return'!
    pure ts0 >>=
      shiftW (\(k, ts) ->
          when ("Username" `T.isInfixOf` ts || "User Name" `T.isInfixOf` ts) $ do
            user <- asks (userName . switchInfo4)
            liftIO $ TL.telnetSend con . B8.pack $ T.unpack user ++ "\n"
            saveResume k
        ) >>=
      shiftW (\(k, ts) ->
          when ("Password" `T.isInfixOf` ts) $ do
            user <- asks (password . switchInfo4)
            liftIO $ TL.telnetSend con . B8.pack $ T.unpack user ++ "\n"
            saveResume k
        ) >>=
      shiftW (\(k, ts) -> do
          when (">" `T.isSuffixOf` ts) $ do
            tRef <- asks telRef
            r <- liftIO (readIORef tRef)
            -- There should be at least one non-empty line, since we've
            -- chacked this above.
            let prompt = T.takeWhile (/= '>') $ last (T.lines ts)
            liftIO $ atomicModifyIORef tRef (\x -> (x{tPrompt = prompt}, ()))
            liftIO $ TL.telnetSend con . B8.pack $ "enable\n"
            saveResume k
        ) >>=
      shiftW (\(k, ts) ->
          when ("Password" `T.isInfixOf` ts) $ do
            enpw <- asks (enablePassword . switchInfo4)
            liftIO $ TL.telnetSend con . B8.pack $ T.unpack enpw ++ "\n"
            saveResume k
        ) >>= \ts ->
      when ("#" `T.isSuffixOf` ts) (finishCmd >> lift (finish ts))

-- FIXME: Provide variants of run for running till result is found. Or running
-- on all switches.
runTill :: (Monoid b, Show b) => a -> TelnetCmd a b -> (b -> Bool) -> ReaderT (M.Map SwName SwInfo) (ExceptT String IO) (Maybe b)
runTill input telnetCmd p = do
    sws <- asks M.keys
    foldM go Nothing sws
  where
    --go :: SwName -> Maybe b -> ReaderT (M.Map SwName SwInfo) (ExceptT String IO) (Maybe b)
    go zs sn
      | fromMaybe False (p <$> zs) = liftIO (putStrLn ("Go ahead " ++ show sn)) >> pure zs
      | otherwise                  = run input telnetCmd sn

runOn :: a -> TelnetCmd a b -> [SwName] -> ReaderT (M.Map SwName SwInfo) (ExceptT String IO) (M.Map SwName b)
runOn input telnetCmd =
    foldM (\zs sn -> maybe zs (\x -> M.insert sn x zs) <$> run input telnetCmd sn) M.empty

-- | Run on one switch.
run :: a -> TelnetCmd a b -> SwName -> ReaderT (M.Map SwName SwInfo) (ExceptT String IO) (Maybe b)
run input telnetCmd sn = do
    mSwInfo <- asks (M.lookup sn)
    case mSwInfo of
      Just swInfo@SwInfo{hostName = h} -> liftIO $ do
        print $ "Connect to " ++ show h
        let cr = CmdReader {switchInfo4 = swInfo, telRef = telnetRef4, telnetIn = input}
        atomicWriteIORef telnetRef4 defTelnetRef4
        connect h "23" (\(s, _) -> handle cr s)
      Nothing -> fail $ "No auth info for switch: '" ++ show sn ++ "'"
    tFinal <$> liftIO (readIORef telnetRef4)
  where
    --handle :: CmdReader a -> Socket -> IO ()
    handle cr sock = do
        telnet <- TL.telnetInit telnetOpts [] (telnetH cr (telnetCmd <=< loginCmd) sock)
        whileJust_ (recv sock 4096) $ \bs -> do
            let bl = B8.length bs
            putStr $ "Socket (" ++ show bl ++ "): "
            B8.putStrLn bs
            TL.telnetRecv telnet bs

    telnetOpts :: [TL.OptionSpec]
    telnetOpts  = [ TL.OptionSpec TL.optEcho True True
                  --, TL.OptionSpec TL.optLineMode True True
                  ]

telnetH :: CmdReader a b -> (TelnetCmd a b) -> Socket -> TL.EventHandler
telnetH cr telnetCmd _ con (TL.Received b)
  = do
    putStr ("R(" ++ show (B8.length b) ++ "):'") *> B8.putStrLn b *> putStrLn "'"
    print (L.map ord (B8.unpack b))
    flip runReaderT cr{tCon = con} . evalContT $ (runCmd (T.decodeLatin1 b) telnetCmd)
telnetH _ _ s _ (TL.Send b)
  = do
    putStr ("S(" ++ show (B8.length b) ++ "):'") *> B8.putStrLn b *> putStrLn "'"
    print (L.map ord (B8.unpack b))
    send s b
telnetH _ _ _ _ (TL.Do o)
  = putStr $ "DO " ++ show o ++ "\n"
telnetH _ _ _ _ (TL.Dont o)
  = putStr $ "DON'T " ++ show o ++ "\n"
telnetH _ _ _ _ (TL.Will o)
  = putStr $ "Will " ++ show o ++ "\n"
telnetH _ _ _ _ (TL.Wont o)
  = putStr $ "WON'T " ++ show o ++ "\n"
telnetH _ _ _ _ (TL.Iac i)
  = putStr $ "IAC " ++ show i ++ "\n"
telnetH _ _ _ _ _ = pure ()

