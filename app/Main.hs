{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.IO.Class
import qualified Data.Map as M
import System.Environment
import Control.Monad.Trans.Cont
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.Maybe
import qualified Shelly as Sh
import Control.Concurrent
import qualified Data.Yaml as Y
import System.Directory
import Text.HTML.TagSoup
import qualified Data.Attoparsec.Text as A
import qualified Options.Applicative  as O
import Control.Monad (join)
import Control.Applicative
import qualified Data.Set as S
import Control.Monad.Except
import Data.Either.Combinators

import BH.Main
import BH.IP
import BH.IP.Arp
import BH.Switch.Cisco
import BH.Telnet

data Options =
  Options
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
  <*> (
    workQueryPorts <$> some
      ( O.strOption
          (  O.long "switch-port"
          <> O.short 'p'
          <> O.metavar "SWITCH/PORT"
          <> O.help "Switch port to look for."
          )
      )
  <|> workQueryMacs <$> some
      ( O.option
          (O.eitherReader (A.parseOnly macP . T.pack))
          (  O.long "mac"
          <> O.short 'm'
          <> O.metavar "MAC-ADDRESS"
          <> O.help "Mac address to look for."
          )
      )
  <|> workQueryIPs <$> some
      ( O.option
          (O.eitherReader (A.parseOnly ipP . T.pack))
          (  O.long "ip"
          <> O.short 'i'
          <> O.metavar "IP-ADDRESS"
          <> O.help "IP address to look for."
         )
      )
    )
 where
  globalOptions :: O.Parser Options
  globalOptions = Options
    <$> O.strOption
        (  O.long "authfile"
        <> O.short 'a'
        <> O.metavar "FILENAME"
        <> O.help "File with switches authentication information."
        <> O.value "authinfo.yaml"
        )
    <*> O.strOption
        (  O.long "mac-ip-file"
        <> O.short 'c'
        <> O.metavar "FILENAME"
        <> O.help "File with mac to IP cache."
        <> O.value "mac-ip-cache.yaml"
        )
    <*> O.strOption
        (  O.long "query-host"
        <> O.short 'H'
        <> O.metavar "HOSTNAME"
        <> O.help "ssh hostname of server from where to gather mac/IPs info."
        <> O.value "certbot"
        )



initConfig ::
  (MonadError String m, MonadIO m) =>
  Options -> ReaderT Config m () -> m ()
initConfig Options{..} action = do
  swInfoMap <- readSwInfo authFile
  liftIO $ print swInfoMap
  (macIpMap, ipMacMap) <- queryLinuxArp macIpFile queryHost
  runReaderT action Config{..}

workQueryPorts ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  [T.Text] -> m ()
workQueryPorts ts = do
  Config{..} <- ask
  let getSwDefaults :: SwName -> (Maybe PortSpeed, Maybe Int)
      getSwDefaults sn =
        let m = M.lookup sn swInfoMap
        in  (swDefaultPortSpeed <$> m, swDefaultPortSlot <$> m)
  swports <- mapM (liftEither . A.parseOnly (swPortP' getSwDefaults)) ts
  queryPorts swports >>= liftIO . print
 where
  getSwDefaults :: SwInfoMap -> SwName -> (Maybe PortSpeed, Maybe Int)
  getSwDefaults swInfo sn =
    let m = M.lookup sn swInfo
    in  (swDefaultPortSpeed <$> m, swDefaultPortSlot <$> m)

workQueryMacs ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  [MacAddr] -> m ()
workQueryMacs macs = queryMacs macs >>= liftIO . print

workQueryIPs ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  [IP] -> m ()
workQueryIPs ips = undefined

main :: IO ()
main = do
    main_ <- O.customExecParser (O.prefs O.showHelpOnError) $
      O.info (O.helper <*> optParser)
      (  O.fullDesc
      <> O.header "General program title/description"
      <> O.progDesc "What does this thing do?"
      )
    res <- runExceptT main_
    case res of
      Right () -> return ()
      Left err -> print err

