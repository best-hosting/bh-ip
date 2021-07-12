{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.IO.Class
import qualified Data.Map as M
import System.Environment
import Control.Monad.Reader
import Control.Monad.Trans.Except
import qualified Data.Attoparsec.Text as A
import Control.Applicative

import BH.IP
import BH.Switch
import BH.Telnet


findPort :: TelnetCmd MacAddr (Maybe (M.Map SwName [PortNum])) ()
findPort t0 = do
    mac <- asks telnetIn
    sn  <- asks (swName . switchInfo)
    -- FIXME: Continue, if no mac was found. Current version will hang up due
    -- to 'Partial' result.
    -- I may parse 'Fa0/9' as complete type, like PortNum. But 'sw-1/9' parse
    -- as SwName, then lookup default port spec and parse '9' as 'PortNum'
    -- using default port spec.
    let parse :: [PortInfoEl] -> Maybe (M.Map SwName [PortNum])
        parse ps = if null ps
                     then Nothing
                     else Just $ M.singleton sn (map (PortNum . portNumber . elPort) ps)
    sendAndParse (parse <$> parseMacAddrTable)
          (defCmd $ "show mac address-table address " <> T.pack (show mac))
          t0
      >>= sendExit

main :: IO ()
main    = do
    swInfo <- parseSwInfo <$> T.readFile "authinfo.txt"
    print swInfo
    Right mac <- head . map (parseMacAddr . T.pack) <$> getArgs
    print mac
    res <- runExceptT $ do
      --mm <- flip runReaderT swInfo $ runTill mac findPort (const True)
      mm <- flip runReaderT swInfo $ runTill mac findPort isJust
      liftIO $ print $ "Found port:" ++ show mm
    case res of
      Right _ -> return ()
      Left err -> print err

