{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Maybe
import qualified Data.Text as T
import Control.Monad.IO.Class
import System.Environment
import Control.Monad.Reader
import Control.Monad.Trans.Except
import qualified Data.Attoparsec.Text as A
import Data.Monoid
import qualified Data.Map as M
import Control.Monad.Except
import Data.List
import qualified Data.Set as S

import BH.IP
import BH.IP.Arp
import BH.Switch.Cisco
import BH.Telnet
import BH.Main

--queryIP :: TelnetCmd [IP] (Maybe (M.Map IP (SwPort, MacAddr))) ()
--queryMac :: TelnetCmd [MacAddr] (Maybe (M.Map MacAddr (SwPort, [IP]))) ()

findPort :: TelnetCmd MacAddr (First (SwName, [PortNum])) ()
findPort t0 = do
    mac <- asks telnetIn
    sn  <- asks (swName . switchInfo)
    let parse :: [PortInfoEl] -> First (SwName, [PortNum])
        parse ps
          | null ps   = mempty
          | otherwise = First . Just $ (sn, (map elPort ps))
    sendAndParse (parse <$> parseMacAddrTable)
          (cmd $ "show mac address-table address " <> T.pack (showMacAddr mac))
          t0
      >>= sendExit

main :: IO ()
main    = do
    swi <- runExceptT $ readSwInfo "authinfo.yaml"
    swInfo <- case swi of
      Right s -> return s
      Left e -> error $ "huy" <> e
    print swInfo
    Right mac <- head . map (A.parseOnly macP . T.pack) <$> getArgs
    print mac
    res <- runExceptT $ do
      --mm <- flip runReaderT swInfo $ runTill mac findPort (const True)
      mm <- flip runReaderT swInfo $ runTill mac findPort (isJust . getFirst)
      liftIO $ print $ "Found port:" ++ show mm
    case res of
      Right _ -> return ()
      Left err -> print err

