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
    , TelnetCmd
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
    , parseTelnetCmdOut2
    , TelnetParser
    , ParserResult (..)
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

type TelnetCtx a b = ContT () (ReaderT (CmdReader a b) IO)
type TelnetCmd a b = T.Text -> TelnetCtx a b ()

data TelnetState a b    = TelnetState
                            { telnetResult  :: Maybe b
                            , telnetResume  :: Maybe (T.Text -> ReaderT (CmdReader a b) IO ())
                            , tInt :: Int
                            , telnetEcho    :: T.Text
                            , telnetPrompt  :: T.Text
                            }

defTelnetState :: TelnetState a b
defTelnetState  = TelnetState { telnetResume = Nothing
                              , telnetResult = Nothing
                              , tInt = 0
                              , telnetEcho   = T.empty
                              , telnetPrompt = T.empty
                              }

telnetStateRef :: IORef (TelnetState a b)
{-# NOINLINE telnetStateRef #-}
telnetStateRef  = unsafePerformIO (newIORef defTelnetState)

-- FIXME: Query, result and program (telnet commands) are all bound together.
-- I.e. if i query by IP, i definitely need 'findPort' and result will be
-- switch port. Etc. That means, i may group this triple: query type, result
-- type and actual function to execute after login into single class instance.
--
-- That been said, the 'config save' operation is different: it does not have
-- a query. Though, if i consider all this not as queries/response, but just
-- like input/program/output, then..
data CmdReader a b = CmdReader  { switchInfo4   :: SwInfo
                                , telnetConn    :: TL.TelnetPtr
                                , telnetIn      :: a
                                , telnetRef     :: IORef (TelnetState a b)
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
sendTelnetCmd :: T.Text -> T.Text -> TelnetCtx a b T.Text
sendTelnetCmd = sendAndParseTelnetCmd (flip Final)

-- FIXME: Rename to just 'sendAndParse'
sendAndParseTelnetCmd :: TelnetParser b -> T.Text -> T.Text -> TelnetCtx a b T.Text
sendAndParseTelnetCmd f cmd t0 = liftIO (print "Huy blye") >>
    shiftW (\(k, ts) -> do
        con  <- asks telnetConn
        when ("#" `T.isSuffixOf` ts || ">" `T.isSuffixOf` ts) $ do
          liftIO $ TL.telnetSend con . B8.pack $ T.unpack cmd <> "\n"
          saveResume k
      ) t0 >>=
    parseEcho cmd >>=
    parseTelnetCmdOut2 f >>=
    parsePrompt

-- | Parse cmd echo-ed back.
parseEcho :: T.Text -> T.Text -> TelnetCtx a b T.Text
parseEcho cmd = shiftW $ \(k, ts) -> do
      tRef <- asks telnetRef
      r <- liftIO (readIORef tRef)
      let echoCmd = telnetEcho r <> ts
      liftIO $ print $ "Original was: " <> cmd
      liftIO $ print $ "Command echo-ed back: " <> echoCmd
      if cmd `T.isInfixOf` echoCmd
        then do
          liftIO $ atomicModifyIORef tRef (\x -> (x{telnetEcho = T.empty}, ()))
          liftIO $ print $ "Command echo complete"
          saveResume k
          lift (k ts)
        else liftIO $ atomicModifyIORef tRef (\x -> (x{telnetEcho = echoCmd}, ()))

parsePrompt :: T.Text -> TelnetCtx a b T.Text
parsePrompt ts = do
    liftIO $ print "Parse prompt"
    tRef <- asks telnetRef
    r <- liftIO (readIORef tRef)
    parseEcho (telnetPrompt r) ts

-- | Gather result and then proceed to next command immediately.
parseTelnetCmdOut :: (T.Text -> Maybe b -> Maybe b) -> T.Text -> TelnetCtx a b T.Text
parseTelnetCmdOut f = undefined

{-    liftIO $ print "Go parsing"
    if "#" `T.isSuffixOf` ts
      then do
        modifyResult (f ts)
        saveResume k
        lift (k ts)
      else do
        modifyResult (f ts)
        liftIO $ print "Retry parsing.."-}

type TelnetParser b = T.Text -> Maybe b -> ParserResult b

data ParserResult b = Final   {parserResult :: Maybe b, unparsedText :: T.Text}
                    | Partial {parserResult :: Maybe b}
  deriving (Show)

isParserEnded :: ParserResult b -> Bool
isParserEnded (Final _ _)   = True
isParserEnded _             = False

parseTelnetCmdOut2 :: TelnetParser b -> T.Text -> TelnetCtx a b T.Text
parseTelnetCmdOut2 f = shiftW $ \(k, ts) -> do
    liftIO $ print "Go parsing 222:"
    liftIO $ print ts
    liftIO $ print "3333333"
    stRef <- asks telnetRef
    st    <- liftIO (readIORef stRef)
    let r = f ts (telnetResult st)
    liftIO $ atomicModifyIORef stRef (\x -> (x{telnetResult = parserResult r}, ()))
    --when (isParserEnded r) $ saveResume k >> lift (k ts)
    if "end\n" `T.isInfixOf` ts
      then do
        liftIO $ print "hunya:"
        liftIO $ mapM_ print (T.lines ts)
        error "Nihuya"
      else return ()
    if isParserEnded r
      then do
        let huy = unparsedText r
        liftIO $ print $ "Finish parsing with " <> T.unpack huy
        saveResume k
        lift $ k (unparsedText r)
      else liftIO $ print "Retry parsing.."

sendTelnetExit :: T.Text -> TelnetCtx a b ()
sendTelnetExit = (\_ -> pure ()) <=< sendTelnetCmd "exit"

saveResume :: MonadIO m => (T.Text -> ReaderT (CmdReader a b) IO ())
              -> ContT () (ReaderT (CmdReader a b) m) ()
saveResume k = do
    tRef <- asks telnetRef
    liftIO $ atomicModifyIORef tRef (\r -> (r{telnetResume = Just k}, ()))

-- | Modify result. If there's not result yet, initialize it with empty value.
modifyResult :: MonadIO m => (Maybe b -> Maybe b) -> ContT () (ReaderT (CmdReader a b) m) ()
modifyResult f = do
    tRef <- asks telnetRef
    liftIO $ atomicModifyIORef tRef (\r -> (r{telnetResult = f (telnetResult r)}, ()))

saveResult :: MonadIO m => b -> ContT () (ReaderT (CmdReader a b) m) ()
saveResult x = do
    tRef <- asks telnetRef
    liftIO $ atomicModifyIORef tRef (\r -> (r{telnetResult = Just x}, ()))

-- FIXME: Do i need this?
finishCmd :: MonadIO m => ContT () (ReaderT (CmdReader a b) m) ()
finishCmd = do
    tRef <- asks telnetRef
    liftIO $ atomicModifyIORef tRef (\r -> (r{telnetResume = Just (\_ -> pure ())}, ()))

runCmd :: T.Text -> (TelnetCmd a b) -> TelnetCtx a b ()
runCmd ts cmd = do
    tRef <- asks telnetRef
    r0 <- liftIO (readIORef tRef)
    let mCont = telnetResume r0
        n0 = tInt r0
    liftIO $ print $ "f start: " ++ show n0
    liftIO $ atomicWriteIORef tRef r0{tInt = n0 + 1}
    shiftT $ \end -> do
      case mCont of
        Nothing -> liftIO (print "huy")  >> cmd ts
        Just c  -> liftIO (print "cont") >> lift (c ts)

-- FIXME: Rewrite login to sendTelnetCmd, etc.
loginCmd :: T.Text -> TelnetCtx a b T.Text
loginCmd ts0 = shiftT $ \finish -> do
    con  <- asks telnetConn
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
            tRef <- asks telnetRef
            r <- liftIO (readIORef tRef)
            -- There should be at least one non-empty line, since we've
            -- chacked this above.
            let prompt = T.takeWhile (/= '>') $ last (T.lines ts)
            liftIO $ atomicModifyIORef tRef (\x -> (x{telnetPrompt = prompt}, ()))
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
        let cr = CmdReader {switchInfo4 = swInfo, telnetRef = telnetStateRef, telnetIn = input}
        atomicWriteIORef telnetStateRef defTelnetState
        connect h "23" (\(s, _) -> handle cr s)
      Nothing -> fail $ "No auth info for switch: '" ++ show sn ++ "'"
    telnetResult <$> liftIO (readIORef telnetStateRef)
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
    flip runReaderT cr{telnetConn = con} . evalContT $ (runCmd (T.decodeLatin1 b) telnetCmd)
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

