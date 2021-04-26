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


parseShowMacAddrTable :: T.Text -> [SwPort]
parseShowMacAddrTable = foldr go [] . T.lines
  where
    go :: T.Text -> [SwPort] -> [SwPort]
    go t zs = case (T.words t) of
        (_ : _ : _ : p : _) -> either (const zs) (: zs) (parsePort p)
        _               -> zs

parsePort :: T.Text -> Either String SwPort
parsePort t = case reads . drop 1 . dropWhile (/= '/') . T.unpack $ t of
  (n, _) : _ -> Right (SwPort n)
  _          -> Left "Huy"

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

-- | 'shiftT' versino suitable for chaining with '>>='. I just pass previous
-- monad result to shiftT's function.
shiftW :: Monad m => ((a -> m r, b) -> ContT r m r) -> b -> ContT r m a
shiftW f x = shiftT (\k -> f (k, x))

loginCmd :: T.Text -> ContT () (ReaderT (CmdReader a b) IO) T.Text
loginCmd ts0 = shiftT $ \finish -> do
    tRef <- asks telRef
    con  <- asks tCon
    -- shiftT stops execution, if supplied continuation is _not_ called. I
    -- don't need any other "suspend mechanisms" apart from plain 'return'!
    pure ts0 >>=
      shiftW (\(k, ts) -> do
          when ("Username" `T.isInfixOf` ts || "User Name" `T.isInfixOf` ts) $ do
            user <- asks (userName . switchInfo4)
            liftIO $ TL.telnetSend con . B8.pack $ T.unpack user ++ "\n"
            r1 <- liftIO (readIORef tRef)
            let n1 = tInt r1
            liftIO $ print n1
            liftIO $ atomicWriteIORef tRef r1{tInt = n1 + 1, tResume = Just k}
        ) >>=
      shiftW (\(k, ts) -> do
          when ("Password" `T.isInfixOf` ts) $ do
            user <- asks (password . switchInfo4)
            liftIO $ TL.telnetSend con . B8.pack $ T.unpack user ++ "\n"
            r1 <- liftIO (readIORef tRef)
            let n1 = tInt r1
            liftIO $ print n1
            liftIO $ atomicWriteIORef tRef r1{tInt = n1 + 1, tResume = Just k}
        ) >>=
      shiftW (\(k, ts) -> do
          when (">" `T.isSuffixOf` ts) $ do
              liftIO $ TL.telnetSend con . B8.pack $ "enable\n"
              r1 <- liftIO (readIORef tRef)
              let n1 = tInt r1
              liftIO $ print n1
              liftIO $ atomicWriteIORef tRef r1{tInt = n1 + 1, tResume = Just k}
        ) >>=
      shiftW (\(k, ts) -> do
          when ("Password" `T.isInfixOf` ts) $ do
            enpw <- asks (enablePassword . switchInfo4)
            liftIO $ TL.telnetSend con . B8.pack $ T.unpack enpw ++ "\n"
            r1 <- liftIO (readIORef tRef)
            let n1 = tInt r1
            liftIO $ print n1
            liftIO $ atomicWriteIORef tRef r1{tInt = n1 + 1, tResume = Just k}
        ) >>= \ts ->
      when ("#" `T.isSuffixOf` ts) $ do
          r1 <- liftIO (readIORef tRef)
          let n1 = tInt r1
          liftIO $ print n1
          liftIO $ atomicWriteIORef tRef r1{tInt = n1 + 1, tResume = Nothing}
          lift (finish ts)

findPort :: TelnetCmd MacAddr [SwPort]
findPort ts0 = do
    tRef <- asks telRef
    con  <- asks tCon
    pure ts0 >>=
      shiftW (\(k, ts) ->
        when ("#" `T.isSuffixOf` ts) $ do
            m <- asks telnetIn
            liftIO $ TL.telnetSend con . B8.pack $ "show mac address-table address " ++ show m ++ "\n"
            liftIO $ putStrLn $ "show port "
            r1 <- liftIO (readIORef tRef)
            let n1 = tInt r1
            liftIO $ print n1
            liftIO $ atomicWriteIORef tRef r1{tInt = n1 + 1, tResume = Just k}
        ) >>=
      shiftW (\(k, ts) -> do
        swp <-
          if "Mac Address Table" `T.isInfixOf` ts || "Mac Address" `T.isInfixOf` ts then do
            liftIO $ putStrLn $ "parse port "
            return (parseShowMacAddrTable ts)
          else return []
        when ("#" `T.isSuffixOf` ts) $ do
            liftIO $ TL.telnetSend con . B8.pack $ "exit\n"
            r1 <- liftIO (readIORef tRef)
            let n1 = tInt r1
            liftIO $ print n1
            liftIO $ atomicWriteIORef tRef r1{tInt = n1 + 1, tResume = Just (\_ -> k ()), tFinal = Just swp}
        )

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

--run :: MacAddr -> (TelnetCmd a) -> ReaderT (M.Map SwName SwInfo) (ExceptT String IO) (Maybe a)
run :: a -> (TelnetCmd a b) -> ReaderT (M.Map SwName SwInfo) (ExceptT String IO) (Maybe b)
run mac telnetCmd = do
    sws <- asks M.keys
    forM_ [head sws] $ \sw -> do
        mSwInfo <- asks (M.lookup sw)
        case mSwInfo of
          Just swInfo@SwInfo{hostName = h} -> liftIO $ do
            print $ "Connect to " ++ show h
            let cr = CmdReader {switchInfo4 = swInfo, telRef = telnetRef4, telnetIn = mac}
            connect h "23" (\(s, _) -> handle cr s)
          Nothing -> fail $ "No auth info for switch: '" ++ show sw ++ "'"
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

main :: IO ()
main    = do
    swInfo <- parseSwInfo <$> T.readFile "authinfo.txt"
    print swInfo
    Right mac <- head . map (parseMacAddr . T.pack) <$> getArgs
    print mac
    res <- runExceptT $ do
      mm <- flip runReaderT swInfo $ run mac findPort
      liftIO $ print $ "Found port:" ++ show mm
    case res of
      Right _ -> return ()
      Left err -> print err
