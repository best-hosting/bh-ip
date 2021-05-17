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
    , PortNum (..)
    , SwPort (..)
    , PortMacMap
    , MacPortMap
    , SwConfig
    , TelnetCmd
    , TelnetInfo (..)
    , MacIpMap
    , PortMap
    , shiftW
    , saveResume
    , run
    , runOn
    , runTill
    , sendCmd
    , sendAndParse
    , sendExit
    , parseTelnetCmdOut
    , TelnetParser
    , ParserResult (..)
    , unparsedText
    , TelCmd (..)
    , defCmd
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

data PortNum         = PortNum Int
  deriving (Eq, Ord, Show)

data SwPort         = SwPort {portSw :: SwName, portSpec :: T.Text, port :: PortNum}
  deriving (Eq, Ord, Show)

type TelnetCmd a b c = T.Text -> ContT () (ReaderT (TelnetInfo a b) IO) c

data TelnetState a b    = TelnetState
                            { telnetResult  :: Maybe b
                            , telnetResume  :: Maybe (T.Text -> ReaderT (TelnetInfo a b) IO ())
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

data TelnetInfo a b = TelnetInfo  { switchInfo    :: SwInfo
                                , telnetConn    :: TL.TelnetPtr
                                , telnetIn      :: a
                                , telnetRef     :: IORef (TelnetState a b)
                                }


type PortMacMap     = M.Map SwPort (Maybe [MacAddr])

type MacPortMap     = M.Map MacAddr (Maybe [SwPort])

type MacIpMap       = M.Map MacAddr [IP]

type PortMap        = M.Map SwPort [(MacAddr, [IP])]

type SwConfig       = M.Map SwName T.Text

data TelCmd    = TelCmd {cmdText :: T.Text, cmdEcho :: Bool}
  deriving (Show)

defCmd :: T.Text -> TelCmd
defCmd t = TelCmd {cmdText = t, cmdEcho = True}





-- | 'shiftT' versino suitable for chaining with '>>='. I just pass previous
-- monad result to shiftT's function.
shiftW :: Monad m => ((a -> m r, b) -> ContT r m r) -> b -> ContT r m a
shiftW f x = shiftT (\k -> f (k, x))

-- | Send telnet command and wait until it'll be echo-ed back.
sendCmd :: TelCmd -> TelnetCmd a b T.Text
sendCmd = sendAndParse (flip Final)

sendAndParse :: TelnetParser b -> TelCmd -> TelnetCmd a b T.Text
sendAndParse f cmd t0 = do
    stRef <- asks telnetRef
    st <- liftIO (readIORef stRef)
    sendParseWithPrompt (echoPromptP (telnetPrompt st)) f cmd t0

sendParseWithPrompt :: PromptParser () -> TelnetParser b -> TelCmd -> TelnetCmd a b T.Text
sendParseWithPrompt pp f (TelCmd {cmdText = cmd, cmdEcho = ce}) t0 = 
    liftIO (print "sendParseWithPrompt: ") >>
    parseEcho pp t0 >>=
    shiftW (\(k, ts) -> do
        con  <- asks telnetConn
        liftIO $ TL.telnetSend con . B8.pack $ T.unpack cmd <> "\n"
        saveResume k
      ) >>=
    (\ts ->
        if ce
          then parseEcho (echoPromptP cmd) ts
          else return ts
    ) >>=
    parseTelnetCmdOut f

echoPromptP :: T.Text -> PromptParser ()
echoPromptP txt ts
  | txt `T.isInfixOf` ts = Final    { parserResult = Just ()
                                    , unparsedText_ = snd $ T.breakOnEnd txt ts
                                    }
  | otherwise            = Partial  { parserResult = Just () }

parseEcho :: (T.Text -> ParserResult ()) -> TelnetCmd a b T.Text
parseEcho pp = shiftW $ \(k, ts) -> do
      stRef <- asks telnetRef
      st <- liftIO (readIORef stRef)
      let echoCmd = telnetEcho st <> ts
          r = pp echoCmd
      liftIO $ print $ "Read back: " <> echoCmd
      if isParserEnded r
        then do
          liftIO $ atomicModifyIORef stRef (\x -> (x{telnetEcho = T.empty}, ()))
          saveResume k
          let rem = fromMaybe T.empty (unparsedText r)
          liftIO $ print $ "Finished reading back with: '" <> rem <> "'"
          lift (k rem)
        else liftIO $ atomicModifyIORef stRef (\x -> (x{telnetEcho = echoCmd}, ()))

type TelnetParser b = T.Text -> Maybe b -> ParserResult b
type PromptParser b = T.Text -> ParserResult b

-- I need 'Maybe' in 'parserResult' here to unbind parser state (finished or
-- not) from actual result state (obtained or not). With 'Maybe' these are two
-- independent properties, but without Maybe 'Final' result will indirectly
-- assume, that some result is obtained. And this may not be the case, when
-- command has no output and i just want to leave current result value (if
-- any) as is and finish parsing (passing all text further as 'unparsedText'),
-- but if 'Final' requires from me to provide some result, i'm stuck.
data ParserResult b = Final   {parserResult :: Maybe b, unparsedText_ :: T.Text}
                    | Partial {parserResult :: Maybe b}
  deriving (Show)

isParserEnded :: ParserResult b -> Bool
isParserEnded (Final _ _)   = True
isParserEnded _             = False

unparsedText :: ParserResult b -> Maybe T.Text
unparsedText Final {unparsedText_ = t} = Just t
unparsedText _                         = Nothing

parseTelnetCmdOut :: TelnetParser b -> TelnetCmd a b T.Text
parseTelnetCmdOut f = shiftW $ \(k, ts) -> do
    liftIO $ print $ "Start parsing: " <> ts
    stRef <- asks telnetRef
    st    <- liftIO (readIORef stRef)
    let r = f ts (telnetResult st)
    liftIO $ atomicModifyIORef stRef (\x -> (x{telnetResult = parserResult r}, ()))
    if isParserEnded r
      then do
        let rem = fromMaybe T.empty (unparsedText r)
        liftIO $ print $ "Finished parsing with '" <> rem <> "'"
        saveResume k
        lift $ k rem
      else liftIO $ print "Retry parsing.."

sendExit :: TelnetCmd a b ()
sendExit = (\_ -> pure ()) <=< sendCmd (defCmd "exit")

saveResume :: MonadIO m => (T.Text -> ReaderT (TelnetInfo a b) IO ())
              -> ContT () (ReaderT (TelnetInfo a b) m) ()
saveResume k = do
    tRef <- asks telnetRef
    liftIO $ atomicModifyIORef tRef (\r -> (r{telnetResume = Just k}, ()))

runCmd :: T.Text -> (TelnetCmd a b ()) -> ContT () (ReaderT (TelnetInfo a b) IO) ()
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

userNamePromptP :: PromptParser ()
userNamePromptP ts
  | "Username:" `T.isInfixOf` ts || "User Name:" `T.isInfixOf` ts
                        = Final   { parserResult = Just ()
                                  , unparsedText_ = rem
                                  }
  | otherwise           = Partial { parserResult = Just () }
  where
    rem | "Username:"  `T.isInfixOf` ts = snd $ T.breakOnEnd "Username:" ts
        | "User Name:" `T.isInfixOf` ts = snd $ T.breakOnEnd "User Name:" ts

passwordPromptP :: PromptParser ()
passwordPromptP ts
  | "Password:" `T.isInfixOf` ts = Final    { parserResult = Just ()
                                            , unparsedText_ = snd $ T.breakOnEnd "Password:" ts
                                            }
  | otherwise                    = Partial  { parserResult = Just () }

checkRootP :: TelnetParser b
checkRootP ts m
  | "#" `T.isSuffixOf` ts = Final   { parserResult = m
                                    , unparsedText_ = ts
                                    }
  | otherwise             = Partial { parserResult = m }

loginCmd :: TelnetCmd a b T.Text
loginCmd ts0 = shiftT $ \finish -> do
    con  <- asks telnetConn
    SwInfo  { userName = user
            , password = pw
            , enablePassword = enPw
            } <- asks switchInfo
    -- shiftT stops execution, if supplied continuation is _not_ called. I
    -- don't need any other "suspend mechanisms" apart from plain 'return'!
    pure ts0 >>=
      sendParseWithPrompt userNamePromptP (flip Final) (defCmd user) >>=
      sendParseWithPrompt passwordPromptP (flip Final) (TelCmd {cmdText = pw, cmdEcho = False}) >>=
      setPrompt telnetPromptP >>=
      sendCmd (defCmd "enable") >>=
      sendParseWithPrompt passwordPromptP checkRootP (TelCmd {cmdText = enPw, cmdEcho = False}) >>=
      lift . finish

telnetPromptP :: PromptParser T.Text
telnetPromptP ts
  | "#" `T.isSuffixOf` ts || ">" `T.isSuffixOf` ts
                = Final   { parserResult  = Just prompt
                          -- Do not consume any input at all. Prompt itself
                          -- may be needed for following 'sendX' function,
                          -- which will verify it. Text before prompt, well..
                          -- in rare cases, where 'parseTelnetCmdOut' will
                          -- follow 'setPrompt', it should also be preserved.
                          , unparsedText_ = ts
                          }
  | otherwise   = Partial { parserResult = Just T.empty }
  where
    prompt = T.init . last . T.lines $ ts

setPrompt :: PromptParser T.Text -> TelnetCmd a b T.Text
setPrompt pp t0 = do
    liftIO (print "setPrompt start.")
    stRef <- asks telnetRef
    liftIO $ atomicModifyIORef stRef (\x -> (x{telnetPrompt = T.empty}, ()))
    shiftW (\(k, ts) -> saveResume k >> lift (k ts)) t0 >>=
      shiftW (\(k, ts) -> do
        st <- liftIO (readIORef stRef)
        let promptTxt = telnetPrompt st <> ts
            r = pp promptTxt
        liftIO $ print $ "Current prompt: " <> promptTxt
        if isParserEnded r
          then do
            let rem = fromMaybe T.empty (unparsedText r)
                res = fromMaybe T.empty (parserResult r)
            liftIO $ atomicModifyIORef stRef (\x -> (x{telnetPrompt = res}, ()))
            saveResume k
            liftIO $ print $ "Set prompt to: '" <> res <> "'"
            liftIO $ print $ "Finished prompt parsing with: '" <> rem <> "'"
            lift (k rem)
          else liftIO $ atomicModifyIORef stRef (\x -> (x{telnetPrompt = promptTxt}, ()))
      )

runTill :: (Monoid b, Show b) => a -> TelnetCmd a b () -> (b -> Bool) -> ReaderT (M.Map SwName SwInfo) (ExceptT String IO) (Maybe b)
runTill input telnetCmd p = do
    sws <- asks M.keys
    foldM go Nothing sws
  where
    --go :: SwName -> Maybe b -> ReaderT (M.Map SwName SwInfo) (ExceptT String IO) (Maybe b)
    go zs sn
      | fromMaybe False (p <$> zs) = liftIO (putStrLn ("Go ahead " ++ show sn)) >> pure zs
      | otherwise                  = run input telnetCmd sn

runOn :: a -> TelnetCmd a b () -> [SwName] -> ReaderT (M.Map SwName SwInfo) (ExceptT String IO) (M.Map SwName b)
runOn input telnetCmd =
    foldM (\zs sn -> maybe zs (\x -> M.insert sn x zs) <$> run input telnetCmd sn) M.empty

-- | Run on one switch.
run :: a -> TelnetCmd a b () -> SwName -> ReaderT (M.Map SwName SwInfo) (ExceptT String IO) (Maybe b)
run input telnetCmd sn = do
    mSwInfo <- asks (M.lookup sn)
    case mSwInfo of
      Just swInfo@SwInfo{hostName = h} -> liftIO $ do
        print $ "Connect to " ++ show h
        let cr = TelnetInfo {switchInfo = swInfo, telnetRef = telnetStateRef, telnetIn = input}
        atomicWriteIORef telnetStateRef defTelnetState
        connect h "23" (\(s, _) -> handle cr s)
      Nothing -> fail $ "No auth info for switch: '" ++ show sn ++ "'"
    telnetResult <$> liftIO (readIORef telnetStateRef)
  where
    --handle :: TelnetInfo a -> Socket -> IO ()
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

telnetH :: TelnetInfo a b -> (TelnetCmd a b ()) -> Socket -> TL.EventHandler
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

