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


saveSwitch :: TelnetCmd () T.Text
saveSwitch ts0 = do
    con  <- asks tCon
    pure ts0 >>=
      shiftW (\(k, ts) ->
          when ("#" `T.isSuffixOf` ts) $ do
            liftIO $ TL.telnetSend con . B8.pack $ "write\n"
            saveResume k
        ) >>=
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
              --modifyResult (M.insertWith (\x y -> y <> x) curSw ts)
              modifyResult (<> ts)
              liftIO $ TL.telnetSend con . B8.pack $ "exit\n"
              finishCmd
            else do
              curSw <- asks (swName . switchInfo4)
              liftIO $ putStrLn $ "save continues"
              --modifyResult (M.insertWith (\x y -> y <> x) curSw ts)
              modifyResult (<> ts)
              -- Iterate over this block.
              return ()
        )

main :: IO ()
main    = do
    swInfo <- parseSwInfo <$> T.readFile "authinfo.txt"
    print swInfo
    sns <- parseArgs <$> getArgs
    print sns
    res <- runExceptT . flip runReaderT swInfo $
      if null sns
        then runAll () saveSwitch
        else let sn = head sns
             in  ($ M.empty) . maybe id (M.insert sn) <$> run (head sns) () saveSwitch
    case res of
      Right m -> do
        forM_ (M.toList m) $ \(SwName s, cf) -> do
          print $ "Writing config for " ++ T.unpack s
          writeFile (T.unpack s ++ ".cf") (T.unpack cf)
      Left err -> print err

parseArgs :: [String] -> [SwName]
parseArgs = map (SwName . T.pack)
