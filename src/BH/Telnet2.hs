{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module BH.Telnet2
    ( TelCmd (..)
    , cmd
    , cmdNoEcho
    , putResult
    , modifyResult
    , TelnetRunM
    , TelnetInfo (..)
    , saveResume
    , saveAndCont
    , telnetParserResultL
    , telnetEchoResultL
    , parseResult
    , parseEcho
    , sendParseWithPrompt
    , sendAndParse
    , sendCmd
    , telnetPromptP
    , setPrompt
    , runCmd
    , run
    , runOn
    , runTill
    , loginCmd
    , sendExit
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
import Control.Monad.State
import Control.Applicative
import Control.Monad.Except
import Data.Functor

import qualified Network.Telnet.LibTelnet as TL
import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Combinator as A

import BH.Common
import BH.Switch


type TelnetRunM p a b = ContT () (ReaderT (TelnetInfo p a b) (StateT T.Text IO))
type TelnetM p a b = ReaderT (TelnetInfo p a b) (StateT T.Text IO)

data TelnetState p a b  = TelnetState
                            { telnetResult :: b
                            , telnetResume  :: Maybe (() -> ReaderT (TelnetInfo p a b) (StateT T.Text IO) ())
                            , tInt :: Int
                            , telnetEchoResult   :: Maybe (A.Result T.Text)
                              -- ^ Input already read for checking command echo.
                            , telnetParserResult :: p
                            , telnetPrompt  :: T.Text
                            }

telnetParserResultL :: LensC (TelnetState p a b) p
telnetParserResultL g z@TelnetState{telnetParserResult = x} = (\x' -> z{telnetParserResult = x'}) <$> g x

telnetEchoResultL :: LensC (TelnetState p a b) (Maybe (A.Result T.Text))
telnetEchoResultL g z@TelnetState{telnetEchoResult = x} = (\x' -> z{telnetEchoResult = x'}) <$> g x

defTelnetState :: (Monoid b, Monoid p) => TelnetState p a b
defTelnetState  = TelnetState { telnetResume = Nothing
                              , telnetResult = mempty
                              , tInt = 0
                              , telnetEchoResult = Nothing
                              , telnetParserResult = mempty
                              , telnetPrompt = T.empty
                              }

-- FIXME: Do i really need it here? Or just hardcode in 'run' ?
telnetStateRef :: (Monoid p, Monoid b) => IORef (TelnetState p a b)
{-# NOINLINE telnetStateRef #-}
telnetStateRef  = unsafePerformIO (newIORef defTelnetState)

data TelnetInfo p a b = TelnetInfo { switchData   :: SwData
                                 , telnetConn   :: TL.TelnetPtr
                                 , telnetIn     :: a
                                 , telnetRef    :: IORef (TelnetState p a b)
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

-- | Save resume continuation.
saveResume :: (() -> TelnetM p a b ()) -> TelnetRunM p a b ()
saveResume k = do
    tRef <- asks telnetRef
    liftIO $ atomicModifyIORef' tRef (\r -> (r{telnetResume = Just k}, ()))

-- FIXME: Rename to 'saveResume'. I don't need 'saveResume' at all.
-- | Save resume continuation and call it immediately.
saveAndCont :: TelnetRunM p a b ()
saveAndCont = shiftT (\k -> saveResume k >> lift (k ()))

parseOutput :: Show d => LensC (TelnetState p a b) (Maybe (A.Result d))
                -> A.Parser d
                -> TelnetRunM p a b d
parseOutput l p = do
    liftIO $ putStrLn "Starting output parsing.."
    liftIO $ putStrLn "Reset parser result.."
    stRef <- asks telnetRef
    liftIO $ atomicModifyIORef' stRef (\s -> (setL l Nothing s, ()))
    saveAndCont >> go
  where
    --go :: TelnetRunM p a b d
    go = shiftT $ \k -> do
      liftIO $ putStrLn "Parsing.."
      stRef <- asks telnetRef
      st    <- liftIO (readIORef stRef)
      ts <- get
      let res = maybe (A.parse p) A.feed (getL l st) ts
      liftIO $ atomicModifyIORef' stRef (\s -> (setL l (Just res) s, ()))
      case res of
        A.Partial _ -> liftIO $ putStrLn "Partial result.."
        -- FIXME: Here i should try to parse "invalid command". [parsing]
        A.Fail i xs err -> error $ "Naebnulos vse: " <> show res
        A.Done unparsedTxt r -> do
          liftIO $ putStrLn "Finished output parsing"
          liftIO $ print $ "Unparsed text left: '" <> unparsedTxt <> "'"
          put unparsedTxt
          saveAndCont
          lift (k r)

-- | More generic 'parseEchoText', which accepts arbitrary parser. But result
-- is restricted to 'Text' and this function is intended just for a little
-- more sophisticated command echo and cmd prompt parsing.
parseEcho :: A.Parser T.Text -- ^ Parser for command output.
              -> TelnetRunM p a b T.Text
parseEcho = parseOutput telnetEchoResultL

parseResult ::
  Show d =>
  LensC p (Maybe (A.Result d)) -> A.Parser d -> TelnetRunM p a b d
parseResult l = parseOutput (telnetParserResultL . l)

-- FIXME: Some of 'Show' constraints are probably not required, but i'll leave
-- them as is for simplifying debugging until this'll become a problem.
sendParseWithPrompt :: Show d => A.Parser T.Text
                        -- ^ Cmd prompt parser.
                        -> LensC p (Maybe (A.Result d)) -> A.Parser d
                        -> TelCmd -> TelnetRunM p a b d
sendParseWithPrompt promptP l p telCmd@TelCmd{..} = do
  liftIO (putStrLn "sendParseWithPrompt: parsing echo")
  void $ parseEcho promptP
  telnetCmdSend
  parseEchoCmd
  parseResult l p
 where
  telnetCmdSend :: TelnetRunM p a b ()
  telnetCmdSend = shiftT $ \k -> do
    con  <- asks telnetConn
    liftIO $ TL.telnetSend con . B8.pack $ T.unpack cmdText <> "\n"
    saveResume k
  parseEchoCmd :: TelnetRunM p a b ()
  parseEchoCmd = do
    when cmdEcho $ do
      liftIO $ print $ "Parsing echo text: '" <> cmdText <> "'"
      void $ parseEcho (A.string cmdText <* A.endOfLine)
    saveAndCont

-- FIXME: This prompt parser will match prompt inside a string. That's
-- incorrect. Make it more strict. May be require newline at the beginning.
-- And, probably, at the end. Though the latter may cause problems with
-- partial parsers. [parsing]
takeTillPromptP :: T.Text -> A.Parser T.Text
takeTillPromptP prompt =
    A.string prompt <|> A.takeTill A.isEndOfLine *> A.endOfLine *> takeTillPromptP prompt

-- FIXME: The reason, that i can't parse to anything, except final result is
-- because final result is _hardcoded_ in type-sig. Though, 'Text' is always
-- 'sendAndParse' return. But it should be the opposite: result should be
-- returned, but 'Text' should be passed over internally. Then i may supply
-- parser to any type.
-- With current situation all 'sendX' calls assume, that they directly parse
-- into final result. But there may be situation, where i just want _to get_
-- parse result without amending it to final result in any way. And this is
-- not possible now. [telnet_runtime][difficult]
sendAndParse ::
  Show d
  => LensC p (Maybe (A.Result d))
  -> A.Parser d
  -> TelCmd -> TelnetRunM p a b d
sendAndParse l p comm = do
    stRef <- asks telnetRef
    st <- liftIO (readIORef stRef)
    -- FIXME: Previous parser should consume \r\n, so '<|>' will not be needed. [parsing]
    sendParseWithPrompt (takeTillPromptP (telnetPrompt st)) l p comm

putResult :: b -> TelnetRunM p a b ()
putResult res = do
    stRef <- asks telnetRef
    liftIO $ atomicModifyIORef' stRef (\x -> (x{telnetResult = res}, ()))

modifyResult :: (b -> b) -> TelnetRunM p a b ()
modifyResult f = do
    stRef <- asks telnetRef
    st <- liftIO (readIORef stRef)
    liftIO $ atomicModifyIORef' stRef (\x -> (x{telnetResult = f (telnetResult st)}, ()))

sendCmd :: TelCmd -> TelnetRunM p a b ()
sendCmd = sendAndParse nothingL (pure ())

-- | Send telnet command and wait until it'll be echo-ed back.
sendExit :: TelnetRunM p a b ()
sendExit = sendCmd (cmd "exit")

runCmd :: TelnetRunM p a b () -> TelnetRunM p a b ()
runCmd comm = do
    tRef <- asks telnetRef
    r0 <- liftIO (readIORef tRef)
    let mCont = telnetResume r0
        n0 = tInt r0
    liftIO $ print $ "Runner: " ++ show n0
    liftIO $ atomicWriteIORef tRef r0{tInt = n0 + 1}
    case mCont of
      Nothing -> liftIO (putStrLn "Start cmd.")  >> comm
      Just c  -> liftIO (putStrLn "Continue cmd.") >> lift (c ())

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
-- session will just hang on partials. [parsing] [telnet_runtime]
-- FIXME: Really check prompt. [parsing]
checkRootP :: A.Parser ()
checkRootP = A.lookAhead (A.takeTill (== '#') *> A.string "#" $> mempty) <|> fail "Not a root"

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
    -- when input end is reached. [parsing] [telnet_runtime]
    go :: A.Parser T.Text
    --go  = A.lookAhead (A.takeWhile1 notEndOfPrompt <* (A.char '#' <|> A.char '>') <* A.endOfInput)
    go  = A.lookAhead (A.takeWhile1 notEndOfPrompt <* (A.char '#' <|> A.char '>'))
          <|> A.takeTill A.isEndOfLine *> A.endOfLine *> go

setPrompt :: A.Parser T.Text -> TelnetRunM p a b ()
setPrompt promptP = do
    liftIO $ putStrLn "Setting new prompt.."
    liftIO $ putStrLn "Reset old telnet prompt.."
    stRef <- asks telnetRef
    liftIO $ atomicModifyIORef' stRef (\x -> (x{telnetPrompt = T.empty}, ()))
    saveAndCont >> go
  where
    go :: TelnetRunM p a b ()
    go = shiftT $ \k -> do
      liftIO $ putStrLn "Start parsing new prompt.."
      prompt <- parseEcho promptP
      stRef <- asks telnetRef
      liftIO $ print $ "Set prompt to: " <> prompt
      liftIO $ atomicModifyIORef' stRef (\x -> (x{telnetPrompt = prompt}, ()))
      saveAndCont
      lift (k ())

loginCmd :: TelnetRunM p a b ()
loginCmd = shiftT $ \k -> do
    SwData {..} <- asks switchData
    -- shiftT stops execution, if supplied continuation is _not_ called. I
    -- don't need any other "suspend mechanisms" apart from plain 'return'!
    sendParseWithPrompt userNamePromptP nothingL (pure ()) (cmd swUser)
    sendParseWithPrompt passwordPromptP nothingL (pure ()) (cmdNoEcho swPassword)
    setPrompt telnetPromptP
    sendCmd (cmd "enable")
    sendParseWithPrompt passwordPromptP nothingL checkRootP (cmdNoEcho swRootPassword)
    saveAndCont
    lift (k ())

telnetH :: (TL.TelnetPtr -> TelnetInfo p a b) -> TelnetRunM p a b () -> Socket -> TL.EventHandler
telnetH ti telnetCmd _ con (TL.Received b)
  = do
    putStr ("R(" ++ show (B8.length b) ++ "):'") *> B8.putStrLn b *> putStrLn "'"
    print (L.map ord (B8.unpack b))
    flip evalStateT (T.decodeLatin1 b) . flip runReaderT (ti con) . evalContT
      $ runCmd telnetCmd
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

-- I need to pass default state as argument here to bind type variable 'b'
-- used in default state to resulting 'b'. Otherwise, default state used in
-- 'atomicWriteIORef' call won't typecheck.
run' :: (MonadReader SwInfo m, MonadError String m, MonadIO m, Monoid p, Monoid b, Show b)
  => IORef (TelnetState p a b) -> a -> TelnetRunM p a b () -> SwName -> m b
run' tRef input telnetCmd sn = do
    mSwInfo <- asks (M.lookup sn)
    case mSwInfo of
      Just swData@SwData{swHost = h} -> liftIO $ do
        print $ "Connect to " ++ show h
        let ti con = TelnetInfo
                        { switchData = swData
                        , telnetRef = tRef
                        , telnetIn = input
                        , telnetConn = con
                        }
        atomicWriteIORef tRef defTelnetState
        connect h "23" (\(s, _) -> handle ti s)
        st2    <- liftIO (readIORef tRef)
        liftIO $ putStrLn "Merging telnet OUTPUT result2" >> print (telnetResult st2)
      Nothing -> throwError $ "No auth info for switch: '" ++ show sn ++ "'"
    telnetResult <$> liftIO (readIORef tRef)
  where
    --handle :: (TL.TelnetPtr -> TelnetInfo a) -> Socket -> IO ()
    handle ti sock = do
        telnet <- TL.telnetInit telnetOpts [] (telnetH ti (loginCmd >> telnetCmd) sock)
        whileJust_ (recv sock 4096) $ \bs -> do
            let bl = B8.length bs
            putStr $ "Socket (" ++ show bl ++ "): "
            B8.putStrLn bs
            TL.telnetRecv telnet bs
    telnetOpts :: [TL.OptionSpec]
    telnetOpts  = [ TL.OptionSpec TL.optEcho True True
                  --, TL.OptionSpec TL.optLineMode True True
                  ]

-- | Run on one switch.
run :: (MonadReader SwInfo m, MonadError String m, MonadIO m, Monoid p, Monoid b, Show b)
  => a -> TelnetRunM p a b () -> SwName -> m b
run = run' telnetStateRef

-- | Run till predicate returns 'Just' with some input.
runTill ::
  (MonadReader SwInfo m, MonadError String m, MonadIO m, Monoid p, Monoid b, Show b)
  => (b -> Maybe a) -> TelnetRunM p a b () -> m b
runTill p telnetCmd = asks M.keys >>= foldM go mempty
  where
    --go ::
    --  (MonadReader SwInfo m, MonadError String m, MonadIO m, Show b) =>
    --  b -> SwName -> m b
    go z sn =
      case p z of
        Just input -> do
          r <- run input telnetCmd sn
          liftIO $ print $ "From runTill: " ++ show r
          return (r <> z)
        Nothing -> liftIO (putStrLn ("Go ahead " ++ show sn)) >> pure z

-- | Run on specified switches with input depending on switch.
runOn ::
  (MonadReader SwInfo m, MonadError String m, MonadIO m, Monoid p, Monoid b, Show b)
  => (SwName -> a) -> [SwName] -> TelnetRunM p a b () -> m b
runOn getInput switches telnetCmd = foldM go mempty switches
 where
  --go ::
  --  (MonadReader SwInfo m, MonadError String m, MonadIO m, Show b) =>
  --  b -> SwName -> m b
  go z sn = (<> z) <$> run (getInput sn) telnetCmd sn

