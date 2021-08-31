{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Data.Text as T
import qualified Data.Map as M
import System.Environment
import Control.Monad.Reader
import Control.Monad.Trans.Except

import BH.Switch.Cisco
import BH.Telnet


saveSwitch :: TelnetRunM TelnetParserResult () (M.Map SwName T.Text) ()
saveSwitch = do
    curSn <- asks (swName . switchData)
    sendCmd (cmd "write")
    sendCmd (cmd "terminal length 0")
    cf <- sendAndParse pResTextL switchConfigP (cmd "show running")
    modifyResult (M.singleton curSn cf <>)
    sendCmd (cmd "exit")

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
        then runOn (const ()) (M.keys swInfo) saveSwitch
        else runOn (const ()) sns saveSwitch
    case res of
      Right m -> do
        forM_ (M.toList m) $ \(SwName s, cf) -> do
          print $ "Writing config for " ++ T.unpack s
          writeFile (T.unpack s ++ ".cf") (T.unpack cf)
      Left err -> print err

parseArgs :: [String] -> [SwName]
parseArgs = map (SwName . T.pack)
