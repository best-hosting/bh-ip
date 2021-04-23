{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

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


data FindPort = HuyF
  deriving (Show, Eq)

instance TelnetOpClass FindPort where
    data TelnetOpRes FindPort = MacPortMap

data FRef = FRef {fInt :: Int, fCont :: Maybe ResumeFRef}

fRef :: IORef FRef
fRef  = unsafePerformIO . newIORef $ FRef {fInt = 0, fCont = Nothing}

type ResumeFRef = ReaderT (IORef FRef) IO ()

{-f :: ContT () (ReaderT (IORef FRef) IO) ()
f = shiftT $ \end -> do
    tRef <- ask
    r0 <- liftIO (readIORef tRef)
    let mCont = fCont r0
        n0 = fInt r0
    liftIO $ print $ "f start: " ++ show n0
    liftIO $ atomicWriteIORef tRef r0{fInt = n0 + 1}
    case mCont of
      Nothing -> liftIO (print "huy")
      Just c  -> lift (c >> end ())

    mac <- shiftT $ \k1 -> do
        liftIO $ putStrLn "-->> show mac address"
        r1 <- liftIO (readIORef tRef)
        let mk1 = Just (k1 "123")
            n1 = fInt r1
        liftIO $ print n1
        liftIO $ atomicWriteIORef tRef r1{fInt = n1 + 1}
        liftIO $ atomicWriteIORef tRef r1{fCont = mk1}
        lift (end ())

    shiftT $ \k2 -> do
        liftIO $ putStrLn ("-->> Read mac table " ++ show mac)
        r2 <- liftIO (readIORef tRef)
        let mk2 = Just (k2 ())
            n2 = fInt r2
        liftIO $ print n2
        liftIO $ atomicWriteIORef tRef r2{fInt = n2 + 1}
        liftIO $ atomicWriteIORef tRef r2{fInt = 2, fCont = mk2}
        lift (end ())

    r3 <- liftIO (readIORef tRef)
    let n3 = fInt r3
    liftIO $ print n3
    liftIO $ atomicWriteIORef tRef r3{fInt = n3 + 1}
    liftIO $ atomicWriteIORef tRef r3{fCont = Nothing}
    liftIO $ putStrLn "End"-}

f0 :: ContT () (ReaderT (IORef FRef) IO) ()
f0 = do
    tRef <- ask
    r0 <- liftIO (readIORef tRef)
    let mCont = fCont r0
        n0 = fInt r0
    liftIO $ print $ "f start: " ++ show n0
    liftIO $ atomicWriteIORef tRef r0{fInt = n0 + 1}
    case mCont of
      Nothing -> liftIO (print "huy") >> f'
      Just c  -> lift c

f' :: ContT () (ReaderT (IORef FRef) IO) ()
f' = shiftT $ \end -> do
    tRef <- ask
    mac <- shiftT $ \k1 -> do
        liftIO $ putStrLn "-->> show mac address"
        r1 <- liftIO (readIORef tRef)
        let mk1 = Just (k1 "123")
            n1 = fInt r1
        liftIO $ print n1
        liftIO $ atomicWriteIORef tRef r1{fInt = n1 + 1, fCont = mk1}
        lift (end ())

    shiftT $ \k2 -> do
        liftIO $ putStrLn ("-->> Read mac table " ++ show mac)
        r2 <- liftIO (readIORef tRef)
        let mk2 = Just (k2 ())
            n2 = fInt r2
        liftIO $ print n2
        if n2 < 4
          then liftIO $ atomicWriteIORef tRef r2{fInt = n2 + 1}
          else liftIO $ atomicWriteIORef tRef r2{fInt = n2 + 1, fCont = mk2}
        lift (end ())

    r3 <- liftIO (readIORef tRef)
    let n3 = fInt r3
    liftIO $ print n3
    liftIO $ atomicWriteIORef tRef r3{fInt = n3 + 1, fCont = Nothing}
    liftIO $ putStrLn "-->> End"

runF'2 :: IO ()
runF'2 = do
    r_init <- liftIO (readIORef fRef)
    liftIO $ atomicWriteIORef fRef r_init{fInt = 0, fCont = Nothing}
    let f = f0
    flip runReaderT fRef . evalContT $ f
    print "A"
    flip runReaderT fRef . evalContT $ f
    print "B"
    flip runReaderT fRef . evalContT $ f
    print "C"
    flip runReaderT fRef . evalContT $ f
    print "D"
    flip runReaderT fRef . evalContT $ f
    print "E"

runF' :: IO ()
runF' = do
    r_init <- liftIO (readIORef fRef)
    liftIO $ atomicWriteIORef fRef r_init{fInt = 0, fCont = Nothing}
    let f = f0
    flip runReaderT fRef . evalContT $ f

    rF1 <- liftIO (readIORef fRef)
    let Just kF1 = fCont rF1
    liftIO $ putStrLn "runF calling to read.."
    flip runReaderT fRef $ kF1

    rF2 <- liftIO (readIORef fRef)
    let Just kF2 = fCont rF2
    liftIO $ putStrLn "runF calling to finish.."
    flip runReaderT fRef $ kF2
    return ()

runF :: IO ()
runF = do
    r_init <- liftIO (readIORef fRef)
    liftIO $ atomicWriteIORef fRef r_init{fInt = 0, fCont = Nothing}
    let f = f0
    flip runReaderT fRef . evalContT $ do
        resetT f
        rF1 <- liftIO (readIORef fRef)
        let Just kF1 = fCont rF1
        liftIO $ putStrLn "runF calling to read.."
        lift kF1
        rF2 <- liftIO (readIORef fRef)
        let Just kF2 = fCont rF2
        liftIO $ putStrLn "runF calling to finish.."
        lift kF2
        return ()

{-    forM (M.toList . M.filter isNothing $ mm0) $ \(mac, _) -> do
        mac <- shiftT $ \k -> do
            putStrLn $ "show mac address table "
            TL.telnetSend con . B8.pack $ "show mac address-table address " ++ show mac
            --liftIO $ atomicWriteIORef tRef r0{telnetCont = Just (k mac)}
            --end ()
        return ()
    return ()-}

    {-
        shiftT $ \k -> do
            when ("Mac Address Table" `T.isInfixOf` ts) $ do
              let sws = parseShowMacAddrTable ts
              liftIO $ atomicWriteIORef tRef r0{telnetRes = M.insert mac sws mm}
            when ("#" `T.isSuffixOf` ts) $ do
              liftIO $ atomicWriteIORef tRef r0{telnetCont = Nothing}
              liftIO $ TL.telnetSend con . B8.pack $ "exit\n"-}


parseShowMacAddrTable :: T.Text -> [SwPort]
parseShowMacAddrTable = foldr go [] . T.lines
  where
    go :: T.Text -> [SwPort] -> [SwPort]
    go t zs = case (T.words t) of
        (_ : _ : _ : p : _) -> either (const zs) (: zs) (parsePort p)
        _               -> zs

parsePort :: T.Text -> Either String SwPort
parsePort = Right . SwPort . read . drop 1 . dropWhile (/= '/') . T.unpack

telnetH :: IORef (TelnetRef3 a) -> (T.Text -> TelnetCtx3 a ()) -> Socket -> TL.EventHandler
telnetH tRef telnetCmd _ t (TL.Received b)
  = do
    putStr "R: "
    B8.putStrLn b
    putStr ("S(" ++ show (B8.length b) ++ "):'") *> B8.putStrLn b *> putStrLn "'"
    flip runReaderT tRef . evalContT $ (resetT $ telnetCmd (T.decodeLatin1 b))
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

telnetH4 :: CmdReader a -> (TelnetCmd a) -> Socket -> TL.EventHandler
telnetH4 cr telnetCmd _ con (TL.Received b)
  = do
    putStr ("R(" ++ show (B8.length b) ++ "):'") *> B8.putStrLn b *> putStrLn "'"
    print (L.map ord (B8.unpack b))
    flip runReaderT cr . evalContT $ (runCmd (con, T.decodeLatin1 b) telnetCmd)
telnetH4 _ _ s _ (TL.Send b)
  = do
    putStr ("S(" ++ show (B8.length b) ++ "):'") *> B8.putStrLn b *> putStrLn "'"
    print (L.map ord (B8.unpack b))
    send s b
telnetH4 _ _ _ _ (TL.Do o)
  = putStr $ "DO " ++ show o ++ "\n"
telnetH4 _ _ _ _ (TL.Dont o)
  = putStr $ "DON'T " ++ show o ++ "\n"
telnetH4 _ _ _ _ (TL.Will o)
  = putStr $ "Will " ++ show o ++ "\n"
telnetH4 _ _ _ _ (TL.Wont o)
  = putStr $ "WON'T " ++ show o ++ "\n"
telnetH4 _ _ _ _ (TL.Iac i)
  = putStr $ "IAC " ++ show i ++ "\n"
telnetH4 _ _ _ _ _ = pure ()

loginCmd :: (TL.TelnetPtr, TelnetEnd Int, T.Text) -> ContT () (ReaderT (CmdReader Int) IO) (TL.TelnetPtr, TelnetEnd Int, T.Text)
loginCmd (con, suspend1, ts1) = shiftT $ \finish -> do
    tRef <- asks telRef
    (_, suspend2, ts2) <- shiftT $ \k1 -> do
      if "Username" `T.isInfixOf` ts1 || "User Name" `T.isInfixOf` ts1
        then do
          user <- asks (userName . switchInfo4)
          liftIO $ TL.telnetSend con . B8.pack $ T.unpack user ++ "\n"
          r1 <- liftIO (readIORef tRef)
          let n1 = tInt r1
          liftIO $ print n1
          liftIO $ atomicWriteIORef tRef r1{tInt = n1 + 1, tResume = Just k1}
          lift (suspend1 ())
        else lift (suspend1 ())
    (_, suspend3, ts3) <- shiftT $ \k2 -> do
        case ts2 of
          _
            | "Password" `T.isInfixOf` ts2 -> do
                user <- asks (password . switchInfo4)
                liftIO $ TL.telnetSend con . B8.pack $ T.unpack user ++ "\n"
                r1 <- liftIO (readIORef tRef)
                let n1 = tInt r1
                liftIO $ print n1
                liftIO $ atomicWriteIORef tRef r1{tInt = n1 + 1, tResume = Just k2}
                lift (suspend2 ())
            -- FIXME: May be drop unexpected state?
            | otherwise                 -> lift (suspend2 ())
    (_, suspend4, ts4) <- shiftT $ \k3 -> do
      if ">" `T.isSuffixOf` ts3
        then do
          liftIO $ TL.telnetSend con . B8.pack $ "enable\n"
          r1 <- liftIO (readIORef tRef)
          let n1 = tInt r1
          liftIO $ print n1
          liftIO $ atomicWriteIORef tRef r1{tInt = n1 + 1, tResume = Just k3}
          lift (suspend3 ())
        else liftIO (print "AAA") >> lift (suspend3 ())
    (con5, suspend5, ts5) <- shiftT $ \k4 -> do
      when ("Password" `T.isInfixOf` ts4) $ do
        enpw <- asks (enablePassword . switchInfo4)
        liftIO $ TL.telnetSend con . B8.pack $ T.unpack enpw ++ "\n"
        r1 <- liftIO (readIORef tRef)
        let n1 = tInt r1
        liftIO $ print n1
        liftIO $ atomicWriteIORef tRef r1{tInt = n1 + 1, tResume = Just k4}
      lift (suspend4 ())
    if "#" `T.isSuffixOf` ts5
      then do
        --liftIO $ TL.telnetSend con . B8.pack $ "exit\n"
        r1 <- liftIO (readIORef tRef)
        let n1 = tInt r1
        liftIO $ print n1
        liftIO $ atomicWriteIORef tRef r1{tInt = n1 + 1, tResume = Nothing}
        lift (finish (con5, suspend5, ts5))
      else lift (suspend5 ())

{-execCmd :: ((TL.TelnetPtr, TelnetEnd a, T.Text) -> ReaderT (CmdReader a) IO ())
            -> ContT () (ReaderT (CmdReader a) IO) (TL.TelnetPtr, TelnetEnd a, T.Text)
execCmd (con, suspend, ts) = shiftT $ \k -> do
      ts <- get
      if "#" `T.isSuffixOf` ts
        then do
          m <- asks findMac
          liftIO $ TL.telnetSend con . B8.pack $ "show mac address-table address " ++ show m ++ "\n"
          suspendAndFinish
            (susp, resume) <- get
            liftIO $ atomicWriteIORef tRef r1{tInt = n1 + 1, tResume = Just resume}
            susp ()
          r1 <- liftIO (readIORef tRef)
          let n1 = tInt r1
          liftIO $ print n1
          liftIO $ atomicWriteIORef tRef r1{tInt = n1 + 1, tResume = Just k1}
          lift (suspend1 ())
        else lift (suspend1 ())-}

findPort :: TelnetCmd Int
findPort (con, suspend1, ts1) = shiftT $ \finish -> do
    tRef <- asks telRef
    (_, suspend2, ts2) <- shiftT $ \k1 -> do
      if "#" `T.isSuffixOf` ts1
        then do
          m <- asks findMac
          liftIO $ TL.telnetSend con . B8.pack $ "show mac address-table address " ++ show m ++ "\n"
          liftIO $ putStrLn $ "show port "
          r1 <- liftIO (readIORef tRef)
          let n1 = tInt r1
          liftIO $ print n1
          liftIO $ atomicWriteIORef tRef r1{tInt = n1 + 1, tResume = Just k1}
          lift (suspend1 ())
        else lift (suspend1 ())
    (_, suspend3, ts3) <- shiftT $ \k2 -> do
      when ("Mac Address Table" `T.isInfixOf` ts2 || "Mac Address" `T.isInfixOf` ts2) $ do
        let swp = parseShowMacAddrTable ts2
        liftIO $ putStrLn $ "parse port "
      if "#" `T.isSuffixOf` ts2
        then do
          liftIO $ TL.telnetSend con . B8.pack $ "exit\n"
          r1 <- liftIO (readIORef tRef)
          let n1 = tInt r1
          liftIO $ print n1
          liftIO $ atomicWriteIORef tRef r1{tInt = n1 + 1, tResume = Just k2}
          lift (suspend2 ())
        else lift (suspend2 ())
    return ()

runCmd :: (TL.TelnetPtr, T.Text)
          -> (TelnetCmd a)
          -> ContT () (ReaderT (CmdReader a ) IO) ()
runCmd (con, ts) cmd = do
    tRef <- asks telRef
    r0 <- liftIO (readIORef tRef)
    let mCont = tResume r0
        mFinal = tFinal r0
        n0 = tInt r0
    liftIO $ print $ "f start: " ++ show n0
    liftIO $ atomicWriteIORef tRef r0{tInt = n0 + 1}
    shiftT $ \end -> do
      if isNothing mFinal
        then
          case mCont of
            Nothing -> liftIO (print "huy")  >> cmd (con, end, ts)
            Just c  -> liftIO (print "cont") >> lift (c (con, end, ts))
        else liftIO (print "A nehuya") >> return ()

run4 :: (TelnetCmd a) -> ReaderT (M.Map SwName SwInfo) (ExceptT String IO) (Maybe a)
run4 telnetCmd = do
    sws <- asks M.keys
    forM_ [head sws] $ \sw -> do
        mSwInfo <- asks (M.lookup sw)
        case mSwInfo of
          Just swInfo@SwInfo{hostName = h} -> liftIO $ do
            print $ "Connect to " ++ show h
            let cr = CmdReader {switchInfo4 = swInfo, telRef = telnetRef4, findMac = MacAddr "003048830f9c"}
            connect h "23" (\(s, _) -> handle cr s)
          Nothing -> fail $ "No auth info for switch: '" ++ show sw ++ "'"
    tFinal <$> liftIO (readIORef telnetRef4)
  where
    --handle :: CmdReader a -> Socket -> IO ()
    handle cr sock = do
        telnet <- TL.telnetInit telnetOpts [] (telnetH4 cr telnetCmd sock)
        whileJust_ (recv sock 4096) $ \bs -> do
            let bl = B8.length bs
            putStr $ "Socket (" ++ show bl ++ "): "
            B8.putStrLn bs
            TL.telnetRecv telnet bs

    telnetOpts :: [TL.OptionSpec]
    telnetOpts  = [ TL.OptionSpec TL.optEcho True True
                  --, TL.OptionSpec TL.optLineMode True True
                  ]

run :: TelnetOpClass a => IORef (TelnetRef3 a) -> (T.Text -> TelnetCtx3 a ()) -> ReaderT (M.Map SwName SwInfo) (ExceptT String IO) (TelnetOpRes a)
run tRef telnetCmd = do
    sws <- asks M.keys
    forM_ sws $ \sw -> do
        mSwInfo <- asks (M.lookup sw)
        case mSwInfo of
          Just swInfo@SwInfo{hostName = h} -> liftIO $ do
            print $ "Connect to " ++ show h
            atomicModifyIORef tRef $ \r ->
                ( r { switchInfo3 = swInfo
                    , telnetState3 = Unauth
                    }
                , ()
                )
            connect h "23" (\(s, _) -> handle s)
          Nothing -> fail $ "No auth info for switch: '" ++ show sw ++ "'"
    telnetRes <$> liftIO (readIORef telnetRef3)
  where
    handle :: Socket -> IO ()
    handle sock = do
        telnet <- TL.telnetInit telnetOpts [] (telnetH tRef telnetCmd sock)
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
    sip <- head . map (parseIP . T.pack) <$> getArgs
    print sip
    res <- runExceptT $ do
      mm <- flip runReaderT swInfo $ run4 (findPort <=< loginCmd)
      liftIO $ print $ "Found port:"
    case res of
      Right _ -> return ()
      Left err -> print err
