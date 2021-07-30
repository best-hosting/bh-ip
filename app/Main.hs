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

-- TODO: queryX functions must return some unified information structure
-- containing all (missed) info, like vlan, port state, IPs, macs, etc.
--data SwPortInfo = SwPortInfo
--                { swPort :: SwPort
--                , swPortAddrs :: M.Map MacAddr [IP]
--                }

data Options = Options {switchPorts :: [SwPort]}
  deriving (Show)

-- FIXME: If i want to make authinfo file location to be configurable, i won't
-- be able to apply switch defaults here, in options parser. Then i should
-- parse here 'switch-port' option arguments as plain text, then read config
-- file, and then parse this text arguments into real 'SwPort'. But if i
-- hardcode 'authinfo' file name, i may leave everything as is..
optParser :: M.Map SwName SwInfo -> O.Parser Options
optParser swInfo = Options
    <$> ( some
            ( O.option
                (O.eitherReader (A.parseOnly (swPortP' getSwDefaults) . T.pack))
                (  O.long "switch-port"
                <> O.short 'p'
                <> O.metavar "SWITCH/PORT"
                <> O.help "Switch port to look for."
                )
            )
        )
  where
    getSwDefaults :: SwName -> (Maybe PortSpeed, Maybe Int)
    getSwDefaults sn = let m = M.lookup sn swInfo
                       in  (swDefaultPortSpeed <$> m, swDefaultPortSlot <$> m)

data Options2 a where
  QuerySwPort :: [SwPort] -> Options2 SwPort
  QueryMacAddr :: [MacAddr] -> Options2 MacAddr
  QueryIP :: [IP] -> Options2 IP

data GlobalOptions =
  GlobalOptions {authFile :: FilePath}


optParser2 ::
    (MonadError String m, MonadIO m) => O.Parser (m ())
optParser2 =
  initConfig
  <$> globalOptions
  <*> (
    workQueryPorts <$> some
      ( O.strOption
          --(O.eitherReader (A.parseOnly (swPortP' getSwDefaults) . T.pack))
          --(O.eitherReader (A.parseOnly swPortP . T.pack))
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
  globalOptions :: O.Parser GlobalOptions
  globalOptions = GlobalOptions <$>
    O.strOption
        (  O.long "authfile"
        <> O.short 'f'
        <> O.metavar "FILENAME"
        <> O.help "File with switches authentication information."
        <> O.value "authinfo.yaml"
        )

main :: IO ()
main = do
    -- TODO: I may use single for storing authinfo and other config. This file
    -- may be encrypted with gpg and edited with sops. Alternatively, i may
    -- split config..
    action <- O.customExecParser (O.prefs O.showHelpOnError) $
      O.info (O.helper <*> optParser2)
      (  O.fullDesc
      <> O.header "General program title/description"
      <> O.progDesc "What does this thing do?"
      )
    res <- runExceptT action
    case res of
      Right () -> return ()
      Left err -> print err
{-main :: IO ()
main = do
    -- TODO: I may use single for storing authinfo and other config. This file
    -- may be encrypted with gpg and edited with sops. Alternatively, i may
    -- split config..
    swi <- runExceptT $ readSwInfo "authinfo.yaml"
    print swi
    swInfoMap <- case swi of
      Right s -> return s
      Left e -> error "huy"
    Right (macIpMap, ipMacMap) <- runExceptT $ queryLinuxArp "mac-ip-cache.yml" "certbot"
    opts <- O.customExecParser (O.prefs O.showHelpOnError) $
      O.info (O.helper <*> optParser swInfoMap)
      (  O.fullDesc
      <> O.header "General program title/description"
      <> O.progDesc "What does this thing do?"
      )
    res <- runExceptT . flip runReaderT (Config{..}) $ work opts
    case res of
      Right () -> return ()
      Left err -> print err-}

-- TODO: MacAddr makes a pair with host. But MacAddr may be of several types:
-- - "physical" - mac address used by server. It may be considered bind to
-- server.
-- - "virtual" - mac address of virtual machine. It may migrate from server
-- (switch port) to server, though physical server connection does not change.
-- - "unknown" - mac address is not assigned to any server.

{-SwPortOptions <$> ..
MacAddrOptions <$> ..
IPOptions <$> ..-}

initConfig ::
  (MonadError String m, MonadIO m) =>
  GlobalOptions -> ReaderT Config m () -> m ()
initConfig GlobalOptions{..} action = do
  swInfoMap <- readSwInfo "authinfo.yaml"
  liftIO $ print swInfoMap
  (macIpMap, ipMacMap) <- queryLinuxArp "mac-ip-cache.yml" "certbot"
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
  queryPorts (S.fromList swports) >>= liftIO . print
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

{-workMacs :: Options MacAddr -> ...
workIPs :: Options IP -> ...-}

work :: (MonadReader Config m, MonadError String m, MonadIO m) => Options -> m ()
work Options{..} = do
    liftIO $ print switchPorts
    queryPorts (S.fromList switchPorts) >>= liftIO . print

