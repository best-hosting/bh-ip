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

queryMac :: 
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  MacAddr -> m (M.Map MacAddr (Maybe SwPort, S.Set IP))
queryMac mac = queryMacs [mac]

queryMacs ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  [MacAddr] -> m (M.Map MacAddr (Maybe SwPort, S.Set IP))
queryMacs macs = do
  Config{..} <- ask
  let sortedMacs = sort macs
  macPorts <- flip runReaderT swInfoMap $
    runTill macs findPorts (\m -> sortedMacs == sort (M.keys m))
  M.foldrWithKey go (return mempty) macPorts
 where
  go ::
    MonadReader Config m =>
    MacAddr -> Maybe SwPort -> m (M.Map MacAddr (Maybe SwPort, S.Set IP)) -> m (M.Map MacAddr (Maybe SwPort, S.Set IP))
  go mac Nothing mz      = M.insert mac (Nothing, mempty) <$> mz
  go mac swp@(Just _) mz = do
    ips <- macToIPs mac
    M.insert mac (swp, ips) <$> mz

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

findPorts :: TelnetCmd [MacAddr] (M.Map MacAddr (Maybe SwPort)) ()
findPorts t0 = do
    sn <- asks (swName . switchInfo)
    -- FIXME: Filter out already found macs.
    macs <- asks telnetIn
    foldM (flip (go sn)) t0 macs >>= sendExit
  where
    go :: SwName -> MacAddr -> TelnetCmd [MacAddr] (M.Map MacAddr (Maybe SwPort)) T.Text
    go portSw mac =
      sendAndParse (parse <$> parseMacAddrTable)
            (cmd $ "show mac address-table address " <> T.pack (showMacAddr mac))
      where
        parse :: [PortInfoEl] -> M.Map MacAddr (Maybe SwPort)
        parse ps
          | null ps   = mempty
          | otherwise =
            case map elPort ps of
              [portSpec] -> M.singleton mac $ Just SwPort{..}
              _ -> error "Huyase tut portov"

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

