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
    data TelnetOpRes FindPort = PortId

{-saveSwitch :: TL.HasTelnetPtr t => (t, T.Text) -> TelnetCtx TelnetRef2 TelnetWrite ()
saveSwitch (con, ts) = do
    tRef <- ask
    liftIO $ do
      r0@TelnetRef2{switchInfo2 = swInfo, telnetState2 = s0, swConfs = mm0} <- readIORef tRef
      (s', mm') <- if "Invalid input detected" `T.isInfixOf` ts
              then flip runStateT mm0 . flip runReaderT swInfo $ go Exit
              else flip runStateT mm0 . flip runReaderT swInfo $ go s0
      atomicWriteIORef tRef r0{telnetState2 = s', swConfs = mm'}
  where
    go :: TelnetState TelnetWrite -> ReaderT SwInfo (StateT SwConfig IO) (TelnetState TelnetWrite)
    go s@Enabled
        | "#" `T.isSuffixOf` ts = do
            liftIO $ TL.telnetSend con . B8.pack $ "terminal length 0\n"
            return (Command TermLength)
        | otherwise = return s
    go s@(Command TermLength)
        | "#" `T.isSuffixOf` ts = do
            liftIO $ TL.telnetSend con . B8.pack $ "show running\n"
            return (Command ShowRun)
        | otherwise = return s
    go s@(Command ShowRun)
        | "#" `T.isSuffixOf` ts = do
            curSw <- asks swName
            liftIO $ putStrLn $ "save continues"
            modify (M.insertWith (\x y -> y <> x) curSw ts)
            go (Command WriteRun)
        | otherwise = do
            curSw <- asks swName
            liftIO $ putStrLn $ "save continues"
            modify (M.insertWith (\x y -> y <> x) curSw ts)
            return s
    go s@(Command WriteRun)
        | "#" `T.isSuffixOf` ts = do
            liftIO $ TL.telnetSend con . B8.pack $ "write\n"
            return Exit
        | otherwise = return s
    go s@Exit
        | "#" `T.isSuffixOf` ts   = do
            liftIO $ TL.telnetSend con . B8.pack $ "exit\n"
            return s
        | otherwise             = return s
    go _ = error "Huy"
-}

telnetH :: IORef (TelnetRef3 a) -> (T.Text -> TelnetCtx3 a ()) -> Socket -> TL.EventHandler
telnetH tRef telnetCmd _ t (TL.Received b)
  = do
    putStr "R: "
    B8.putStrLn b
    return ()
    flip runReaderT tRef . evalContT $ (telnetCmd (T.decodeLatin1 b))
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
