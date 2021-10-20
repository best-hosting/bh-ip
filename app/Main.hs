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
import qualified Data.Text.IO as T
import qualified Options.Applicative as O
import qualified Data.Yaml as Y
import qualified Data.ByteString as B
import System.IO.Temp
import System.IO
import System.Directory
import Data.List
import Data.Aeson
import Data.Maybe
import Data.Monoid
import Data.Time
import Text.Read
import qualified Data.Set as S

import BH.IP
import BH.IP.Arp
import BH.Main
import BH.Cache
import BH.Switch.Cisco

-- TODO: Work on sw-0.
-- TODO: Use `show cdp neighbors` to build switch connection tree. And
-- visualize with diagrams.

data VlanConfig = VlanConfig {vlanHost :: T.Text, vlanNet :: String}
  deriving (Show)

instance FromJSON VlanConfig where
  parseJSON = withObject "VlanConfig" $ \v ->
    VlanConfig
      <$> v .: "host"
      <*> v .: "net"

data MyOptions = MyOptions
  { optAuthFile :: FilePath
  , optCacheTimeStampFile :: FilePath
  , optCacheUpdateInterval :: NominalDiffTime
  , optMacIpFile :: FilePath
  , optVlan :: Maybe Vlan
  , optVlanConfig :: M.Map Vlan VlanConfig
  }
  deriving (Show)

instance FromJSON MyOptions where
  parseJSON = withObject "MyOptions" $ \v ->
    MyOptions
      <$> v .:? "authfile" .!= "authinfo.yaml"
      <*> v .:? "timestamp" .!= "bh-ip-timestamp.txt"
      <*> v .:? "cacheUpdate" .!= 600
      <*> pure "mac-ip-cache.yaml"
      <*> v .:? "defaultVlan"
      <*> v .: "vlans"

optParser ::
  (MonadError String m, MonadIO m) => MyOptions -> O.Parser (m ())
optParser MyOptions{..} =
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
  globalOptions :: O.Parser MyOptions
  globalOptions =
    MyOptions
      <$> O.strOption
        ( O.long "authfile"
            <> O.short 'a'
            <> O.metavar "FILENAME"
            <> O.help "File with switches authentication information."
            <> O.showDefault
            <> O.value optAuthFile
        )
      <*> O.strOption
        ( O.long "timestamp"
            <> O.metavar "FILENAME"
            <> O.help "File where last cache update time is stored"
            <> O.showDefault
            <> O.value optCacheTimeStampFile
        )
      <*> O.option (fromInteger <$> O.auto)
        ( O.long "cache-update"
            <> O.metavar "SECONDS"
            <> O.help "Time in seconds between cache updates"
            <> O.showDefault
            <> O.value optCacheUpdateInterval
        )
      <*> O.strOption
        ( O.long "mac-ip-file"
            <> O.metavar "FILENAME"
            <> O.help "File with mac to IP cache."
            <> O.value "mac-ip-cache.yaml"
        )
      <*> O.option
        (O.eitherReader (A.parseOnly (Just <$> vlanP) . T.pack))
        ( O.long "vlan"
            <> O.short 'l'
            <> O.metavar "INT"
            <> O.help "Vlan to run in."
            <> maybe mempty (\v -> O.showDefault <> O.value (Just v)) optVlan
        )
      <*> pure optVlanConfig

initConfig ::
  (MonadError String m, MonadIO m) =>
  MyOptions ->
  ReaderT Config m () ->
  m ()
initConfig opts@MyOptions{..} action = do
  liftIO $ print "Read config:"
  liftIO $ print opts

  swInfo <- readSwInfo optAuthFile
  liftIO $ print swInfo

  t <- liftIO getCurrentTime
  let updateInterval = optCacheUpdateInterval
      timeFile = optCacheTimeStampFile
      d = addUTCTime (negate updateInterval) t
  cacheTime <- either (const d) id . readEither <$> liftIO (readFile timeFile)

  (runVlan, VlanConfig{..}) <- maybe (throwError "Unknown vlan") return $ do
    v <- optVlan
    (v, ) <$> M.lookup v optVlanConfig
  let macIpFile = optMacIpFile
      nmapHost = vlanHost
      cf0 = Config{..}
  --(mi, im) <- runReaderT (queryLinuxArp macIpFile vlanHost) cf0
  pm <- readPorts
  runReaderT (readAll >>= execStateT queryLinuxArp2 . mergePorts pm >>= writeAll) cf0
  --runReaderT action cf0{macIpMap = mi, ipMacMap = im}

readPorts :: (MonadIO m, MonadError String m) => m (M.Map SwPort (PortState, S.Set MacAddr))
readPorts = readYaml "portmap.yml"

readAll :: (MonadIO m, MonadError String m) => m (IPInfo, MacInfo, SwPortInfo)
readAll =
  (,,)
    <$> readYaml "ipinfo2.yaml"
    <*> readYaml "macinfo2.yaml"
    <*> readYaml "swportinfo2.yaml"

writeAll :: (MonadIO m, MonadError String m) => (IPInfo, MacInfo, SwPortInfo) -> m ()
writeAll (ipInfo, macInfo, swPortInfo) = do
  writeCache "ipinfo2.yaml" ipInfo
  writeCache "macinfo2.yaml" macInfo
  writeCache "swportinfo2.yaml" swPortInfo
 where
  writeCache :: (MonadIO m, ToJSON a) => FilePath -> a -> m ()
  writeCache f x = liftIO $ do
    cwd <- getCurrentDirectory
    (t, _) <- openTempFile cwd f
    Y.encodeFile t x
    renameFile t f

workQuery ::
  (MonadReader Config m, MonadError String m, MonadIO m, InfoDb c
      , FromJSONKey (InfoKey c)
      , FromJSON (InfoData c)
      , ToJSONKey (InfoKey c)
      , ToJSON (InfoData c)
  ) =>
  FilePath -> [InfoKey c] -> m c
workQuery p xs = do
  (res, newMacInfo) <- readYaml p >>= runStateT (query xs)
  -- TODO: Use 'Config' parameter to store swport db filename.
  -- FIXME: Update all dbs after each query.
  liftIO $ do
    cwd <- getCurrentDirectory
    (f, _) <- openTempFile cwd p
    Y.encodeFile f newMacInfo
    renameFile f p
  return res

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
  let f :: SwPortInfo -> SwPortInfo
      f = id
  workQuery "swportinfo.yaml" swports >>= liftIO . B.putStr . Y.encode . f

-- TODO: Query only cache, if requested. On the other hand, i probably may not
-- offer such functionality. After all, cache is yaml and `yq` will be better
-- in querying it anyway. But in case `yq` is not available.. well..
workQueryMacs ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  [MacAddr] ->
  m ()
workQueryMacs macs = do
  workQuery "macinfo.yaml" macs >>= \res -> liftIO . B.putStr . Y.encode $ (res :: MacInfo)

workQueryIPs ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  [IP] ->
  m ()
workQueryIPs ips =
  workQuery "ipinfo.yaml" ips >>= \res -> liftIO . B.putStr . Y.encode $ (res :: IPInfo)


main :: IO ()
main = do
  res <- runExceptT $ do
    cfOpts <- readYaml "bh-ip.yaml"
    main_ <- liftIO $ O.customExecParser (O.prefs O.showHelpOnError) $
      O.info
        (O.helper <*> optParser cfOpts)
        ( O.fullDesc
            <> O.header "General program title/description"
            <> O.progDesc "What does this thing do?"
        )
    main_
  case res of
    Right () -> return ()
    Left err -> print err

