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


--queryIP :: TelnetCmd [IP] (Maybe (M.Map IP (SwPort, MacAddr))) ()
--queryMac :: TelnetCmd [MacAddr] (Maybe (M.Map MacAddr (SwPort, [IP]))) ()
findPort :: TelnetCmd MacAddr (Maybe (M.Map SwName [PortNum])) ()
findPort t0 = do
    mac <- asks telnetIn
    sn  <- asks (swName . switchInfo)
    let parse :: [PortInfoEl] -> Maybe (M.Map SwName [PortNum])
        parse ps = if null ps
                     then Nothing
                     else Just $ M.singleton sn (map elPort ps)
    sendAndParse (parse <$> parseMacAddrTable)
          (cmdWEcho $ "show mac address-table address " <> T.pack (showMacAddr mac))
          t0
      >>= sendExit

main :: IO ()
main    = do
    swi <- runExceptT $ readSwInfo "authinfo.yaml"
    swInfo <- case swi of
      Right s -> return s
      Left e -> error "huy"
    print swInfo
    Right mac <- head . map (A.parseOnly macP . T.pack) <$> getArgs
    print mac
    res <- runExceptT $ do
      --mm <- flip runReaderT swInfo $ runTill mac findPort (const True)
      mm <- flip runReaderT swInfo $ runTill mac findPort isJust
      liftIO $ print $ "Found port:" ++ show mm
    case res of
      Right _ -> return ()
      Left err -> print err

