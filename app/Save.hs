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
import Control.Concurrent
import qualified Data.Yaml as Y
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding as A
import System.Directory
import Text.HTML.TagSoup

import BH.Switch


data TelnetWrite = WriteRun | ShowRun | TermLength
  deriving (Show, Eq)

telnetRef2 :: IORef (TelnetRef2 TelnetWrite)
{-# NOINLINE telnetRef2 #-}
telnetRef2  = unsafePerformIO . newIORef
                $ TelnetRef2 { swConfs = M.empty
                             , telnetState2 = Unauth
                             , switchInfo2 = undefined
                             }

saveSwitch :: TL.HasTelnetPtr t => (t, T.Text) -> TelnetCtx TelnetRef2 TelnetWrite ()
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

saveSwitch2 :: TelnetCmd () (M.Map SwName T.Text)
saveSwitch2 ts0 = do
    con  <- asks tCon
    pure ts0 >>=
      shiftW (\(k, ts) ->
          when ("#" `T.isSuffixOf` ts) $ do
            liftIO $ TL.telnetSend con . B8.pack $ "terminal length 0\n"
            saveResume k
        ) >>=
      shiftW (\(k, ts) ->
          when ("#" `T.isSuffixOf` ts) $ do
            liftIO $ TL.telnetSend con . B8.pack $ "show running\n"
            saveResume k
        ) >>=
      shiftW (\(k, ts) ->
          if "#" `T.isSuffixOf` ts
            then do
              curSw <- asks (swName . switchInfo4)
              liftIO $ putStrLn $ "save finishes"
              modifyResult (M.insertWith (\x y -> y <> x) curSw ts)
              liftIO $ TL.telnetSend con . B8.pack $ "exit\n"
              finishCmd
            else do
              curSw <- asks (swName . switchInfo4)
              liftIO $ putStrLn $ "save continues"
              modifyResult (M.insertWith (\x y -> y <> x) curSw ts)
              -- Iterate over this block.
              return ()
        )

telnetH :: Socket -> TL.EventHandler
telnetH _ t (TL.Received b)
  = do
    putStr "R: "
    B8.putStrLn b
    flip runReaderT telnetRef2 . evalContT $ (telnetLogin t (T.decodeLatin1 b) >>= saveSwitch)
telnetH s _ (TL.Send b)
  = do
    putStr ("S(" ++ show (B8.length b) ++ "):'") *> B8.putStrLn b *> putStrLn "'"
    print (L.map ord (B8.unpack b))
    send s b
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

run :: ReaderT (M.Map SwName SwInfo) (ExceptT String IO) SwConfig
run = do
    swcfs <- swConfs <$> liftIO (readIORef telnetRef2)
    forM_ (M.keys swcfs) $ \sw -> do
        mSwInfo <- asks (M.lookup sw)
        case mSwInfo of
          Just swInfo@SwInfo{hostName = h} -> liftIO $ do
            print $ "Connect to " ++ show h
            atomicModifyIORef telnetRef2 $ \r ->
                ( r { switchInfo2 = swInfo
                    , telnetState2 = Unauth
                    }
                , ()
                )
            connect h "23" (\(s, _) -> handle s)
          Nothing -> fail $ "No auth info for switch: '" ++ show sw ++ "'"
    swConfs <$> liftIO (readIORef telnetRef2)
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
    swInfo <- parseSwInfo <$> T.readFile "authinfo.txt"
    print swInfo
    swcfs <- parseArgs <$> getArgs
    print swcfs
    atomicModifyIORef telnetRef2 (\r -> (r{swConfs = swcfs}, ()))
    res <- runExceptT $ do
      Just mm <- flip runReaderT swInfo $ BH.Switch.run () saveSwitch2
      --mm <- flip runReaderT swInfo $ Main.run
      liftIO $ print $ "Gathered config:"
      liftIO . forM (M.toList mm) $ \(s, cf) -> do
        print s
        mapM print (T.lines cf)
    case res of
      Right _ -> return ()
      Left err -> print err

parseArgs :: [String] -> SwConfig
parseArgs = foldr go M.empty
  where
    go :: String -> SwConfig -> SwConfig
    go xs z = let (sn, '/' : sp) = span (/= '/') xs
              in  M.insert
                    (SwName (T.pack sn))
                    mempty
                    z

