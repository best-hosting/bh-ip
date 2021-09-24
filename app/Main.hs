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
import Data.Aeson
import Data.Maybe
import Data.Monoid

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

{-instance ToJSON VlanConfig where
  toJSON VlanConfig{..} =
    object ["host" .= vlanHost, "net" .= vlanNet]-}

instance FromJSON VlanConfig where
  parseJSON = withObject "VlanConfig" $ \v ->
    VlanConfig
      <$> v .: "host"
      <*> v .: "net"

-- FIXME: Add 'opt' prefix to fields.
data MyOptions = MyOptions
  { optConfFile :: FilePath
  , optAuthFile :: FilePath
  , optMacIpFile :: FilePath
  , optVlan :: Last Vlan
  , optVlanConfig :: M.Map Vlan VlanConfig
  }
  deriving (Show)

instance Semigroup MyOptions where
  x <> y = y
            { optVlan = optVlan x <> optVlan y
            , optVlanConfig = optVlanConfig y <> optVlanConfig x
            }

{-instance ToJSON MyOptions where
  toJSON MyOptions{..} =
    object
      $ maybe [] (\v -> ["optVlan" .= v]) optVlan
        ++  [ "authfile" .= optAuthFile
            , "vlans" .= optVlanConfig
            ]
-}

instance FromJSON MyOptions where
  parseJSON = withObject "MyOptions" $ \v ->
    MyOptions
      <$> pure ""
      <*> v .:? "authfile" .!= "authinfo.yaml"
      <*> pure "mac-ip-cache.yaml"
      <*> (Last <$> v .:? "defaultVlan")
      <*> v .: "vlans"

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
  globalOptions :: O.Parser MyOptions
  globalOptions =
    MyOptions
      <$> O.strOption
        ( O.long "confifile"
            <> O.short 'c'
            <> O.metavar "FILENAME"
            <> O.help "Config file."
            <> O.value "bh-ip.yaml"
        )
      <*> O.strOption
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
      <*> O.option O.auto
        ( O.long "vlan"
            <> O.short 'c'
            <> O.metavar "INT"
            <> O.help "Vlan to run in."
            <> O.value (Last Nothing)
        )
      <*> pure M.empty

initConfig ::
  (MonadError String m, MonadIO m) =>
  MyOptions ->
  ReaderT Config m () ->
  m ()
initConfig cmdOp action = do
  readOp <- readYaml (optConfFile cmdOp)
  let cf@MyOptions{..} = readOp <> cmdOp
  liftIO $ print "Resulting config:"
  liftIO $ print cf
  swInfo <- readSwInfo optAuthFile
  liftIO $ print swInfo
  (runVlan, VlanConfig{..}) <- maybe (throwError "Unknown vlan") return $ do
    v <- getLast optVlan
    (v, ) <$> M.lookup v optVlanConfig
  (macIpMap, ipMacMap) <- queryLinuxArp optMacIpFile vlanHost
  runReaderT action Config{..}


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
