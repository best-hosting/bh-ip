{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import System.Environment
import Control.Monad.Reader
import Control.Monad.Trans.Except
import qualified Data.Attoparsec.Text as A

import BH.Switch.Cisco
import BH.Telnet


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
        then runOn (const ()) saveSw (M.keys swInfo)
        else runOn (const ()) saveSw sns
    case res of
      Right m -> do
        forM_ (M.toList m) $ \(SwName s, cf) -> do
          print $ "Writing config for " ++ T.unpack s
          writeFile (T.unpack s ++ ".cf") (T.unpack cf)
      Left err -> print err

parseArgs :: [String] -> [SwName]
parseArgs = map (SwName . T.pack)
