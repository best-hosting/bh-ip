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

data FRef = FRef {fInt :: Int, fCont :: Maybe (ReaderT (IORef FRef) IO ())}

fRef :: IORef FRef
fRef  = unsafePerformIO . newIORef $ FRef {fInt = 0, fCont = Nothing}

f :: ContT () (ReaderT (IORef FRef) IO) ()
f = shiftT $ \end -> do
    tRef <- ask
    r0 <- liftIO (readIORef tRef)
    let mCont = fCont r0
    liftIO $ print (fInt r0)
    maybe (liftIO (print "huy") >> return ()) lift mCont

    mac <- shiftT $ \k1 -> do
        liftIO $ putStrLn "show mac address"
        liftIO $ atomicWriteIORef tRef r0{fInt = 1, fCont = Just (k1 "123")}
        lift (end ())

    shiftT $ \k2 -> do
        liftIO $ putStrLn ("Read mac table " ++ show mac)
        liftIO $ atomicWriteIORef tRef r0{fInt = 2, fCont = Just (k2 ())}
        lift (end ())

    undefined
    liftIO $ atomicWriteIORef tRef r0{fInt = 3, fCont = Nothing}
    liftIO $ putStrLn "End"

runF :: IO ()
runF = do
    runReaderT (evalContT f) fRef
    runReaderT (evalContT f) fRef
    runReaderT (evalContT f) fRef

findPort :: TL.HasTelnetPtr t => (t, T.Text) -> TelnetCtx3 FindPort ()
findPort (con, ts) = shiftT $ \end -> do
    tRef <- ask
    r0 <- liftIO (readIORef tRef)
    let mCont = telnetCont r0
        mm0 = telnetRes r0
    maybe (return ()) lift mCont

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
    return ()
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

main :: IO ()
main = undefined

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

{-main :: IO ()
main    = do
    swInfo <- parseSwInfo <$> T.readFile "authinfo.txt"
    print swInfo
    sip <- head . map (parseIP . T.pack) <$> getArgs
    print sip
    res <- runExceptT $ do
      mm <- flip runReaderT swInfo $ run
      liftIO $ print $ "Found port:"
    case res of
      Right _ -> return ()
      Left err -> print err-}
