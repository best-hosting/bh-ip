{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Attoparsec.Text as A
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Options.Applicative as O
import qualified Data.Yaml as Y
import qualified Data.ByteString as B

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
  queryPorts swports >>= liftIO . B.putStr . Y.encode

workQueryMacs ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  [MacAddr] ->
  m ()
workQueryMacs macs0 = do
  swpInfo0 <- readYaml "switches.yaml"
  let foundMacPorts :: M.Map MacAddr SwPortInfo
      foundMacPorts = foldr (\m -> M.insert m (lookupMacPort m swpInfo0)) mempty macs0
      foundMacs = M.keys . M.filter (/= M.empty) $ foundMacPorts
      macs1 = filter (`notElem` foundMacs) macs0
  liftIO $ print "Found in cache: "
  liftIO $ print foundMacPorts
  liftIO $ print $ "Yet to query: " ++ show macs1
  queriedMacPorts <- queryMacs macs1
  let allMacPorts = M.unionWith (<>) foundMacPorts queriedMacPorts
      swpInfo1 = foldr (<>) swpInfo0 (M.elems queriedMacPorts)
  liftIO $ do
    B.putStr . Y.encode $ allMacPorts
    Y.encodeFile "switches1.yaml" swpInfo1


lookupMacPort :: MacAddr -> SwPortInfo -> SwPortInfo
lookupMacPort mac = M.foldrWithKey go mempty
 where
  go :: SwPort -> PortData -> SwPortInfo -> SwPortInfo
  go swp pd@PortData{..} z
    | M.member mac portAddrs = M.insert swp pd z
    | otherwise = z

workQueryIPs ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  [IP] ->
  m ()
workQueryIPs ips = queryIPs ips >>= liftIO . B.putStr . Y.encode

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
