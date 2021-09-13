{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Attoparsec.Text as A
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Options.Applicative as O
import qualified Data.Yaml as Y
import qualified Data.ByteString as B
import System.IO.Temp
import System.IO
import System.Directory
import Data.List

import BH.IP
import BH.IP.Arp
import BH.Main
import BH.Cache
import BH.Switch.Cisco

-- TODO: Work on sw-0.
-- TODO: Use `show cdp neighbors` to build switch connection tree. And
-- visualize with diagrams.

data Options = Options
  { authFile :: FilePath
  , macIpFile :: FilePath
  , queryHost :: T.Text
  }
  deriving (Show)

optParser ::
  (MonadError String m, MonadIO m) => O.Parser (m ())
optParser =
  initConfig
    <$> globalOptions
    <*> (   workQueryPorts
              <$> some
                ( O.strOption
                    ( O.long "switch-port"
                        <> O.short 'p'
                        <> O.metavar "SWITCH/PORT"
                        <> O.help "Switch port to look for."
                    )
                )
            <|> workQueryMacs
              <$> some
                ( O.option
                    (O.eitherReader (A.parseOnly macP . T.pack))
                    ( O.long "mac"
                        <> O.short 'm'
                        <> O.metavar "MAC-ADDRESS"
                        <> O.help "Mac address to look for."
                    )
                )
            <|> workQueryIPs
              <$> some
                ( O.option
                    (O.eitherReader (A.parseOnly ipP . T.pack))
                    ( O.long "ip"
                        <> O.short 'i'
                        <> O.metavar "IP-ADDRESS"
                        <> O.help "IP address to look for."
                    )
                )
        )
 where
  globalOptions :: O.Parser Options
  globalOptions =
    Options
      <$> O.strOption
        ( O.long "authfile"
            <> O.short 'a'
            <> O.metavar "FILENAME"
            <> O.help "File with switches authentication information."
            <> O.value "authinfo.yaml"
        )
      <*> O.strOption
        ( O.long "mac-ip-file"
            <> O.short 'c'
            <> O.metavar "FILENAME"
            <> O.help "File with mac to IP cache."
            <> O.value "mac-ip-cache.yaml"
        )
      <*> O.strOption
        ( O.long "query-host"
            <> O.short 'H'
            <> O.metavar "HOSTNAME"
            <> O.help "ssh hostname of server from where to gather mac/IPs info."
            <> O.value "certbot"
        )

initConfig ::
  (MonadError String m, MonadIO m) =>
  Options ->
  ReaderT Config m () ->
  m ()
initConfig Options{..} action = do
  swInfo <- readSwInfo authFile
  liftIO $ print swInfo
  (macIpMap, ipMacMap) <- queryLinuxArp macIpFile queryHost
  runReaderT action Config{..}

workQueryPorts ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  [T.Text] ->
  m ()
workQueryPorts ts = do
  Config{..} <- ask
  let getSwDefaults :: SwName -> (Maybe PortSpeed, Maybe Int)
      getSwDefaults sn =
        let m = M.lookup sn swInfo
         in (swDefaultPortSpeed <$> m, swDefaultPortSlot <$> m)
  swports <- mapM (liftEither . A.parseOnly (swPortP' getSwDefaults)) ts
  (res, newMacInfo) <- readYaml "swportinfo.yaml" >>= runStateT (queryPorts swports)
  liftIO $ B.putStr . Y.encode $ res
  liftIO $ do
    cwd <- getCurrentDirectory
    (f, _) <- openTempFile cwd "swportinfo.yaml"
    Y.encodeFile f newMacInfo
    renameFile f "swportinfo.yaml"


{--- | Lookup single mac port in a cache.
lookupMacPort :: MacAddr -> SwPortInfo -> SwPortInfo
lookupMacPort mac = M.filter (\PortData{..} -> M.member mac portAddrs)

-- | Lookup mac ports in cache (without verify).
lookupMacPorts :: [MacAddr] -> SwPortInfo -> SwPortInfo
lookupMacPorts macs swpInfo =
  foldr (\m -> (lookupMacPort m swpInfo <>)) mempty $ macs

-- | Lookup mac ports in cache and verify (query) info about found ports.
lookupMacPorts' ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  [MacAddr] -> SwPortInfo -> m SwPortInfo
lookupMacPorts' macs = queryPorts . M.keys . lookupMacPorts macs-}

-- FIXME: May this be the common function for all query types?
-- TODO: Query only cache, if requested. On the other hand, i probably may not
-- offer such functionality. After all, cache is yaml and `yq` will be better
-- in querying it anyway. But in case `yq` is not available.. well..
workQueryMacs ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  [MacAddr] ->
  m ()
workQueryMacs macs = do
  (res, newMacInfo) <- readYaml "macinfo.yaml" >>= runStateT (queryMacs macs)
  liftIO $ B.putStr . Y.encode $ res
  -- TODO: Use 'Config' parameter to store swport db filename.
  -- FIXME: If one file is missing, rebuild it from others.
  liftIO $ do
    cwd <- getCurrentDirectory
    (f, _) <- openTempFile cwd "macinfo.yaml"
    Y.encodeFile f newMacInfo
    renameFile f "macinfo.yaml"


workQueryIPs ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  [IP] ->
  m ()
workQueryIPs ips = do
  (res, newMacInfo) <- readYaml "ipinfo.yaml" >>= runStateT (queryIPs ips)
  liftIO $ B.putStr . Y.encode $ res
  -- TODO: Use 'Config' parameter to store swport db filename.
  liftIO $ do
    cwd <- getCurrentDirectory
    (f, _) <- openTempFile cwd "ipinfo.yaml"
    Y.encodeFile f newMacInfo
    renameFile f "ipinfo.yaml"


main :: IO ()
main = do
  main_ <-
    O.customExecParser (O.prefs O.showHelpOnError) $
      O.info
        (O.helper <*> optParser)
        ( O.fullDesc
            <> O.header "General program title/description"
            <> O.progDesc "What does this thing do?"
        )
  res <- runExceptT main_
  case res of
    Right () -> return ()
    Left err -> print err
