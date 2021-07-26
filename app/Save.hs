{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import System.Environment
import Control.Monad.Reader
import Control.Monad.Trans.Except

import BH.Switch
import BH.Telnet


saveSwitch :: TelnetCmd () T.Text ()
saveSwitch t0 =
    sendCmd (defCmd "write") t0 >>=
    sendCmd (defCmd "terminal length 0") >>=
    sendAndParse parseCiscoConfig (defCmd "show running") >>=
    sendExit

main :: IO ()
main    = do
    swi <- runExceptT $ readSwInfo "authinfo.yaml"
    swInfo <- case swi of
      Right s -> return s
      Left e -> error "huy"
    print swInfo
    sns <- parseArgs <$> getArgs
    print sns
    res <- runExceptT . flip runReaderT swInfo $
      if null sns
        then runOn () saveSwitch (M.keys swInfo)
        else runOn () saveSwitch sns
    case res of
      Right m -> do
        forM_ (M.toList m) $ \(SwName s, cf) -> do
          print $ "Writing config for " ++ T.unpack s
          writeFile (T.unpack s ++ ".cf") (T.unpack cf)
      Left err -> print err

parseArgs :: [String] -> [SwName]
parseArgs = map (SwName . T.pack)
