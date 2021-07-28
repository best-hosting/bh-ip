{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module BH.Telnet
    ( TelCmd (..)
    , cmd
    , cmdNoEcho
    , TelnetCmd
    , TelnetInfo (..)
    , shiftW
    , saveResume
    , sendCmd
    , parseCmd
    , sendAndParse
    , sendExit
    , run
    , runOn
    , runTill
    , telnetPromptP
    , setPrompt
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
import Control.Applicative
import Control.Monad.Except
import Data.Functor

import qualified Network.Telnet.LibTelnet as TL
import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Combinator as A

import BH.Common
import BH.Switch


type TelnetCmd a b c = (Show b, Monoid b) => T.Text -> ContT () (ReaderT (TelnetInfo a b) IO) c

data TelnetState a b    = TelnetState
                            { telnetResult :: b
                            , telnetResume  :: Maybe (T.Text -> ReaderT (TelnetInfo a b) IO ())
                            , tInt :: Int
                            , telnetEchoResult   :: Maybe (A.Result T.Text)
                            , telnetOutputResult :: Maybe (A.Result b)
                            , telnetPrompt  :: T.Text
                            }

telnetEchoResultL :: LensC (TelnetState a b) (Maybe (A.IResult T.Text T.Text))
telnetEchoResultL g z@TelnetState{telnetEchoResult = x} = (\x' -> z{telnetEchoResult = x'}) <$> g x

telnetOutputResultL :: LensC (TelnetState a b) (Maybe (A.IResult T.Text b))
telnetOutputResultL g z@TelnetState{telnetOutputResult = x} = (\x' -> z{telnetOutputResult = x'}) <$> g x

defTelnetState :: Monoid b => TelnetState a b
defTelnetState  = TelnetState { telnetResume = Nothing
                              , telnetResult = mempty
                              , tInt = 0
                              , telnetEchoResult = Nothing
                              , telnetOutputResult = Nothing
                              -- ^ Input already read for checking command echo.
                              , telnetPrompt = T.empty
                              }

-- FIXME: Do i really need it here? Or just hardcode in 'run' ?
telnetStateRef :: (Show b, Monoid b) => IORef (TelnetState a b)
{-# NOINLINE telnetStateRef #-}
telnetStateRef  = unsafePerformIO (newIORef defTelnetState)

data TelnetInfo a b = TelnetInfo { switchInfo   :: SwInfo
                                 , telnetConn   :: TL.TelnetPtr
                                 , telnetIn     :: a
                                 , telnetRef    :: IORef (TelnetState a b)
                                 }


data TelCmd    = TelCmd {cmdText :: T.Text, cmdEcho :: Bool}
  deriving (Show)

cmd :: T.Text -> TelCmd
cmd t = TelCmd {cmdText = t, cmdEcho = True}

cmdNoEcho :: T.Text -> TelCmd
cmdNoEcho t = TelCmd {cmdText = t, cmdEcho = False}




-- | 'shiftT' version suitable for chaining with '>>='. I just pass previous
-- monad result to shiftT's function.
shiftW :: Monad m => ((a -> m r, b) -> ContT r m r) -> b -> ContT r m a
shiftW f x = shiftT (\k -> f (k, x))

-- | Send telnet command and wait until it'll be echo-ed back.
sendCmd :: TelCmd -> TelnetCmd a b T.Text
sendCmd = sendAndParse (pure mempty)

sendAndParse :: A.Parser b -> TelCmd -> TelnetCmd a b T.Text
sendAndParse p cmd t0 = do
    stRef <- asks telnetRef
    st <- liftIO (readIORef stRef)
    -- FIXME: Previous parser should consume \r\n, so '<|>' will not be needed.
    sendParseWithPrompt (takeTillPromptP (telnetPrompt st)) p cmd t0

-- FIXME: This prompt parser will match prompt inside a string. That's
-- incorrect. Make it more strict. May be require newline at the beginning.
-- And, probably, at the end. Though the latter may cause problems with
-- partial parsers.
takeTillPromptP :: T.Text -> A.Parser T.Text
takeTillPromptP prompt =
    A.string prompt <|> A.takeTill A.isEndOfLine *> A.endOfLine *> takeTillPromptP prompt

sendParseWithPrompt :: A.Parser T.Text
                        -- ^ Cmd prompt parser.
                        -> A.Parser b -> TelCmd -> TelnetCmd a b T.Text
sendParseWithPrompt promptP p TelCmd{..} t0 = 
    liftIO (putStrLn "sendParseWithPrompt: parsing echo") >>
    parseEcho promptP t0 >>=
    shiftW (\(k, _) -> do
        con  <- asks telnetConn
        liftIO $ TL.telnetSend con . B8.pack $ T.unpack cmdText <> "\n"
        saveResume k
      ) >>=
    shiftW (\(k, ts) ->
        (if cmdEcho then parseEchoText cmdText ts else return ts)
        >>= flip saveAndCont k
      ) >>=
    parseCmd p

parseEchoText :: T.Text -- ^ Text, which i expect to be echoed back.
              -> TelnetCmd a b T.Text
parseEchoText echoTxt ts = do
    liftIO $ print $ "Parsing echo text: '" <> echoTxt <> "'"
    parseEcho (A.string echoTxt <* A.endOfLine) ts

-- | More generic 'parseEchoText', which accepts arbitrary parser. But result
-- is restricted to 'Text' and this function is intended just for a little
-- more sophisticated command echo and cmd prompt parsing.
parseEcho :: A.Parser T.Text -- ^ Parser for command output.
              -> TelnetCmd a b T.Text
parseEcho = parseOutputL telnetEchoResultL

parseCmd :: A.Parser b -> TelnetCmd a b T.Text
parseCmd p = shiftW $ \(k, ts) ->do
      liftIO $ putStrLn "Starting command output parsing.."
      unparsedTxt <- parseOutputL telnetOutputResultL p ts
      stRef <- asks telnetRef
      st    <- liftIO (readIORef stRef)
      case telnetOutputResult st of
        Just (A.Done _ res) -> do
          liftIO $ putStrLn "Cmd output result" >> print res
          liftIO $ atomicModifyIORef' stRef (\x -> (x{telnetResult = res <> telnetResult st}, ()))
        _ -> error "Huh blye?"
      st2    <- liftIO (readIORef stRef)
      liftIO $ putStrLn "Merged telnet output result" >> print (telnetResult st2)
      saveAndCont unparsedTxt k

parseOutputL :: LensC (TelnetState a b) (Maybe (A.IResult T.Text c))
                -> A.Parser c -- ^ Parser for command output.
                -> TelnetCmd a b T.Text
parseOutputL l p ts = do
    liftIO $ putStrLn "Starting output parsing.."
    liftIO $ putStrLn "Reset parser result.."
    stRef <- asks telnetRef
    liftIO $ atomicModifyIORef' stRef (\s -> (setL l Nothing s, ()))
    shiftT (saveAndCont ts) >>= go
  where
    --go :: TelnetCmd a b T.Text
    go = shiftW $ \(k, ts) -> do
      liftIO $ putStrLn "Parsing.."
      stRef <- asks telnetRef
      st    <- liftIO (readIORef stRef)
      let res = maybe (A.parse p) A.feed (getL l st) ts
      liftIO $ atomicModifyIORef' stRef (\s -> (setL l (Just res) s, ()))
      case res of
        A.Partial _ -> liftIO $ putStrLn "Partial result.."
        A.Fail i xs err -> error $ "Naebnulos vse: " <> T.unpack i <> concat xs <> err
        A.Done unparsedTxt _ -> do
          liftIO $ putStrLn "Finished output parsing"
          liftIO $ print $ "Unparsed text left: '" <> unparsedTxt <> "'"
          saveAndCont unparsedTxt k

sendExit :: TelnetCmd a b ()
sendExit = (\_ -> pure ()) <=< sendCmd (cmd "exit")

-- | Save resume continuation.
saveResume :: MonadIO m => (T.Text -> ReaderT (TelnetInfo a b) IO ())
              -> ContT () (ReaderT (TelnetInfo a b) m) ()
saveResume k = do
    tRef <- asks telnetRef
    liftIO $ atomicModifyIORef' tRef (\r -> (r{telnetResume = Just k}, ()))

-- | Save resume continuation and call it immediately.
saveAndCont :: T.Text -> (T.Text -> ReaderT (TelnetInfo a b) IO ()) -> ContT () (ReaderT (TelnetInfo a b) IO) ()
saveAndCont ts k = saveResume k >> lift (k ts)

runCmd :: (Show b, Monoid b) => T.Text -> TelnetCmd a b () -> ContT () (ReaderT (TelnetInfo a b) IO) ()
runCmd ts cmd = do
    tRef <- asks telnetRef
    r0 <- liftIO (readIORef tRef)
    let mCont = telnetResume r0
        n0 = tInt r0
    liftIO $ print $ "f start: " ++ show n0
    liftIO $ atomicWriteIORef tRef r0{tInt = n0 + 1}
    case mCont of
      Nothing -> liftIO (putStrLn "Start cmd.")  >> cmd ts
      Just c  -> liftIO (putStrLn "Continue cmd.") >> lift (c ts)

-- | FIXME: Rename to userNameP
userNamePromptP :: A.Parser T.Text
userNamePromptP =
    let nameP = A.string "Username:" <|> A.string "User name:"
    in  A.manyTill (A.takeTill A.isEndOfLine *> A.endOfLine) (A.lookAhead nameP)
        *> nameP

-- | FIXME: Rename to passwordP .
passwordPromptP :: A.Parser T.Text
passwordPromptP = (A.endOfLine <|> pure ()) *> A.string "Password:"

-- FIXME: I need a way to indicate, that there is no more input available
-- and this should terminate all partial parser to fail. Otherwise, the
-- session will just hang on partials.
-- FIXME: Really check prompt.
checkRootP :: Monoid b => A.Parser b
checkRootP = A.lookAhead (A.takeTill (== '#') *> A.string "#" $> mempty) <|> fail "Not a root"

loginCmd :: TelnetCmd a b T.Text
loginCmd ts0 = shiftT $ \finish -> do
    SwInfo  { swUser = user
            , swPassword = pw
            , swRootPassword = enPw
            } <- asks switchInfo
    -- shiftT stops execution, if supplied continuation is _not_ called. I
    -- don't need any other "suspend mechanisms" apart from plain 'return'!
    pure ts0 >>=
      sendParseWithPrompt userNamePromptP (pure mempty) (cmd user) >>=
      sendParseWithPrompt passwordPromptP (pure mempty) (cmdNoEcho pw) >>=
      setPrompt telnetPromptP >>=
      sendCmd (cmd "enable") >>=
      sendParseWithPrompt passwordPromptP checkRootP (cmdNoEcho enPw) >>=
      lift . finish

-- | Match to prompt and return it as a result, but leave it unconsumed, so
-- further parser may match to prompt too.
telnetPromptP :: A.Parser T.Text
telnetPromptP = go
  where
    notEndOfPrompt :: Char -> Bool
    notEndOfPrompt = (&&) <$> (not . A.isEndOfLine) <*> (`notElem` ['#', '>'])
    -- FIXME: Strictly speaking, i need '<* A.endOfInput' at the end. But then
    -- to obtain 'Done' result i need to explicitly finalize parsing by
    -- passing empty input. Without '<* A.endOfInput' i may match chars in the
    -- middle.. So, to really fix this, i need to either signal somehow the
    -- _last_ chunk of input or just restart entire cycle with empty input,
    -- when input end is reached.
    go :: A.Parser T.Text
    --go  = A.lookAhead (A.takeWhile1 notEndOfPrompt <* (A.char '#' <|> A.char '>') <* A.endOfInput)
    go  = A.lookAhead (A.takeWhile1 notEndOfPrompt <* (A.char '#' <|> A.char '>'))
          <|> A.takeTill A.isEndOfLine *> A.endOfLine *> go

setPrompt :: A.Parser T.Text -> TelnetCmd a b T.Text
setPrompt promptP ts = do
    liftIO $ putStrLn "Setting new prompt.."
    liftIO $ putStrLn "Reset old telnet prompt.."
    stRef <- asks telnetRef
    liftIO $ atomicModifyIORef' stRef (\x -> (x{telnetPrompt = T.empty}, ()))
    shiftT (saveAndCont ts) >>= go
  where
    go :: TelnetCmd a b T.Text
    go = shiftW $ \(k, ts) -> do
      liftIO $ putStrLn "Start parsing new prompt.."
      unparsedTxt <- parseEcho promptP ts
      stRef <- asks telnetRef
      st    <- liftIO (readIORef stRef)
      case telnetEchoResult st of
        Just (A.Done _ prompt) -> do
          liftIO $ print $ "Set prompt to: " <> prompt
          liftIO $ atomicModifyIORef' stRef (\x -> (x{telnetPrompt = prompt}, ()))
        _ -> error "Huh?"
      saveAndCont unparsedTxt k

runTill :: (MonadReader SwInfoMap m, MonadError String m, MonadIO m, Show b, Monoid b) => a -> TelnetCmd a b () -> (b -> Bool) -> m b
runTill input telnetCmd p = do
    sws <- asks M.keys
    foldM go mempty sws
  where
    --go :: (MonadReader SwInfoMap m, MonadError String m, MonadIO m) => b -> SwName -> m b
    go z sn
      | p z         = liftIO (putStrLn ("Go ahead " ++ show sn)) >> pure z
      | otherwise   = do
        r <- run input telnetCmd sn
        liftIO $ print $ "From runTill: " ++ show r
        return r

runOn :: (MonadReader SwInfoMap m, MonadError String m, MonadIO m, Show b, Monoid b) => a -> TelnetCmd a b () -> [SwName] -> m (M.Map SwName b)
runOn input telnetCmd =
    foldM (\zs sn -> (\x -> M.insert sn x zs) <$> run input telnetCmd sn) M.empty

-- | Run on one switch.
run :: (MonadReader SwInfoMap m, MonadError String m, MonadIO m, Show b, Monoid b) => a -> TelnetCmd a b () -> SwName -> m b
run = run' telnetStateRef

-- I need to pass default state as argument here to bind type variable 'b'
-- used in default state to resulting 'b'. Otherwise, default state used in
-- 'atomicWriteIORef' call won't typecheck.
run' :: (MonadReader SwInfoMap m, MonadError String m, MonadIO m, Show b, Monoid b) => IORef (TelnetState a b) -> a -> TelnetCmd a b () -> SwName -> m b
run' tRef input telnetCmd sn = do
    mSwInfo <- asks (M.lookup sn)
    case mSwInfo of
      Just swInfo@SwInfo{swHost = h} -> liftIO $ do
        print $ "Connect to " ++ show h
        let ti con = TelnetInfo
                        { switchInfo = swInfo
                        , telnetRef = tRef
                        , telnetIn = input
                        , telnetConn = con
                        }
        -- FIXME: Should i reset telnet result between runs?
        atomicWriteIORef tRef defTelnetState
        connect h "23" (\(s, _) -> handle ti s)
        st2    <- liftIO (readIORef tRef)
        liftIO $ putStrLn "Merging telnet OUTPUT result2" >> print (telnetResult st2)
      Nothing -> throwError $ "No auth info for switch: '" ++ show sn ++ "'"
    telnetResult <$> liftIO (readIORef tRef)
  where
    --handle :: (TL.TelnetPtr -> TelnetInfo a) -> Socket -> IO ()
    handle ti sock = do
        telnet <- TL.telnetInit telnetOpts [] (telnetH ti (telnetCmd <=< loginCmd) sock)
        whileJust_ (recv sock 4096) $ \bs -> do
            let bl = B8.length bs
            putStr $ "Socket (" ++ show bl ++ "): "
            B8.putStrLn bs
            TL.telnetRecv telnet bs

    telnetOpts :: [TL.OptionSpec]
    telnetOpts  = [ TL.OptionSpec TL.optEcho True True
                  --, TL.OptionSpec TL.optLineMode True True
                  ]

telnetH :: (Show b, Monoid b) => (TL.TelnetPtr -> TelnetInfo a b) -> TelnetCmd a b () -> Socket -> TL.EventHandler
telnetH ti telnetCmd _ con (TL.Received b)
  = do
    putStr ("R(" ++ show (B8.length b) ++ "):'") *> B8.putStrLn b *> putStrLn "'"
    print (L.map ord (B8.unpack b))
    flip runReaderT (ti con) . evalContT $ runCmd (T.decodeLatin1 b) telnetCmd
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


