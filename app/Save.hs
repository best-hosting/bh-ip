{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import System.Environment
import Control.Monad.Reader
import Control.Monad.Trans.Except
import qualified Data.Attoparsec.Text as A

import BH.Common
import BH.Switch.Cisco
import BH.Telnet
import qualified BH.Telnet2 as T2


saveSwitch2 :: T2.TelnetRunM TelnetParserResult () (M.Map SwName T.Text) ()
saveSwitch2 = do
    curSn <- asks (swName . T2.switchInfo)
    T2.sendCmd (T2.cmd "write")
    T2.sendCmd (T2.cmd "terminal length 0")
    cf <- T2.sendAndParse pResTextL parseCiscoConfig (T2.cmd "show running")
    T2.modifyResult (M.singleton curSn cf <>)
    T2.sendCmd (T2.cmd "exit")

saveSwitch :: (Show b, Monoid b) => A.Parser b -> TelnetCmd () b ()
saveSwitch p ts =
    sendCmd (cmd "write") ts >>=
    sendCmd (cmd "terminal length 0") >>=
    sendAndParse p (cmd "show running") >>=
    sendExit

getParser :: TelnetCtx () (M.Map SwName T.Text) (A.Parser (M.Map SwName T.Text))
getParser = do
    curSn <- asks (swName . switchInfo)
    return (M.singleton curSn <$> parseCiscoConfig)

saveSw :: TelnetCmd () (M.Map SwName T.Text) ()
saveSw ts = getParser >>= flip saveSwitch ts

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
        --then runOn (const ()) saveSw (M.keys swInfo)
        then T2.runOn (const ()) saveSwitch2 (M.keys swInfo)
        --else runOn (const ()) saveSw sns
        else T2.runOn (const ()) saveSwitch2 sns
    case res of
      Right m -> do
        forM_ (M.toList m) $ \(SwName s, cf) -> do
          print $ "Writing config for " ++ T.unpack s
          writeFile (T.unpack s ++ ".cf") (T.unpack cf)
      Left err -> print err

parseArgs :: [String] -> [SwName]
parseArgs = map (SwName . T.pack)
