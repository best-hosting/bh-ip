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


newtype TStr = TStr {tStr :: Maybe (A.Result (M.Map SwName T.Text))}

instance Semigroup TStr where
  x@(TStr (Just _)) <> TStr _ = x
  TStr Nothing <> y = y

instance Monoid TStr where
  mempty = TStr Nothing
  mappend = (<>)

tStrL :: LensC TStr (Maybe (A.Result (M.Map SwName T.Text)))
tStrL g TStr{..} = TStr <$> g tStr

saveSwitch2 :: A.Parser (M.Map SwName T.Text) -> T2.TelnetRunM TStr () (M.Map SwName T.Text) ()
saveSwitch2 p = do
    T2.sendCmd (T2.cmd "write")
    T2.sendCmd (T2.cmd "terminal length 0")
    T2.sendAndParse tStrL p (T2.cmd "show running") >>= T2.putResult
    T2.sendCmd (T2.cmd "exit")

saveSwitch :: (Show b, Monoid b) => A.Parser b -> TelnetCmd () b ()
saveSwitch p ts =
    sendCmd (cmd "write") ts >>=
    sendCmd (cmd "terminal length 0") >>=
    sendAndParse p (cmd "show running") >>=
    sendExit

getParser2 :: T2.TelnetRunM TStr () (M.Map SwName T.Text) (A.Parser (M.Map SwName T.Text))
getParser2 = do
    curSn <- asks (swName . T2.switchInfo)
    return (M.singleton curSn <$> parseCiscoConfig)

getParser :: TelnetCtx () (M.Map SwName T.Text) (A.Parser (M.Map SwName T.Text))
getParser = do
    curSn <- asks (swName . switchInfo)
    return (M.singleton curSn <$> parseCiscoConfig)

saveSw :: TelnetCmd () (M.Map SwName T.Text) ()
saveSw ts = getParser >>= flip saveSwitch ts

saveSw2 :: T2.TelnetRunM TStr () (M.Map SwName T.Text) ()
saveSw2 = getParser2 >>= saveSwitch2

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
        then T2.runOn (const ()) saveSw2 (M.keys swInfo)
        --else runOn (const ()) saveSw sns
        else T2.runOn (const ()) saveSw2 sns
    case res of
      Right m -> do
        forM_ (M.toList m) $ \(SwName s, cf) -> do
          print $ "Writing config for " ++ T.unpack s
          writeFile (T.unpack s ++ ".cf") (T.unpack cf)
      Left err -> print err

parseArgs :: [String] -> [SwName]
parseArgs = map (SwName . T.pack)
