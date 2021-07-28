{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

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
import BH.Switch
import BH.Telnet

-- TODO: queryX functions must return some unified information structure
-- containing all (missed) info, like vlan, port state, IPs, macs, etc.

-- | Query single port.
queryPort :: (MonadReader Config m, MonadError String m, MonadIO m) =>
             SwPort -> m (M.Map SwPort [(MacAddr, S.Set IP)])
queryPort swPort = queryPorts (S.singleton swPort)

-- | Query several ports.
queryPorts :: (MonadReader Config m, MonadError String m, MonadIO m) =>
             (S.Set SwPort) -> m (M.Map SwPort [(MacAddr, S.Set IP)])
queryPorts switches = do
  Config{..} <- ask
  portMacs <- flip runReaderT swInfoMap $ run (S.toList switches) getMacs (head . S.toList $ S.map portSw switches)
  liftIO $ putStrLn "Gathered ac map:"
  liftIO $ print portMacs
  liftIO $ putStrLn "Finally, ips..."
  forM portMacs $ mapM (\m -> (m, ) <$> macToIPs m)

-- I'm intresting in receiving all ports in question as input, because then i
-- may connect to each switch only once and iterate over all asked ports from
-- this switch.
getMacs :: TelnetCmd [SwPort] (M.Map SwPort [MacAddr]) ()
getMacs t0 = do
    curSn <- asks (swName . switchInfo)
    ps    <- asks (filter ((== curSn) . portSw) . telnetIn)
    foldM (flip go) t0 ps >>= sendExit
  where
    go :: SwPort -> TelnetCmd [SwPort] (M.Map SwPort [MacAddr]) T.Text
    go swPort@SwPort{..} =
        sendAndParse (parse <$> parseMacAddrTable)
          (cmd $ "show mac address-table interface " <> ciscoPortNum portSpec)
      where
        parse :: [PortInfoEl] -> M.Map SwPort [MacAddr]
        parse xs = if null xs
                      then mempty
                      else M.singleton swPort (map elMac xs)

data Options = Options {switchPorts :: [SwPort]}
  deriving (Show)

-- FIXME: If i want to make authinfo file location to be configurable, i won't
-- be able to apply switch defaults here, in options parser. Then i should
-- parse here 'switch-port' option arguments as plain text, then read config
-- file, and then parse this text arguments into real 'SwPort'. But if i
-- hardcode 'authinfo' file name, i may leave everything as is..
optParser :: M.Map SwName SwInfo -> O.Parser Options
optParser swInfo = Options
    <$> some
        ( O.option
            (O.eitherReader (A.parseOnly (swPortP' getSwDefaults) . T.pack))
            (  O.long "switch-port"
            <> O.short 'p'
            <> O.metavar "SWITCH/PORT"
            <> O.help "Switch port to look for."
            )
        )
  where
    getSwDefaults :: SwName -> (Maybe PortSpeed, Maybe Int)
    getSwDefaults sn = let m = M.lookup sn swInfo
                       in  (swDefaultPortSpeed <$> m, swDefaultPortSlot <$> m)

main :: IO ()
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
      Left err -> print err

-- TODO: MacAddr makes a pair with host. But MacAddr may be of several types:
-- - "physical" - mac address used by server. It may be considered bind to
-- server.
-- - "virtual" - mac address of virtual machine. It may migrate from server
-- (switch port) to server, though physical server connection does not change.
-- - "unknown" - mac address is not assigned to any server.

work :: (MonadReader Config m, MonadError String m, MonadIO m) => Options -> m ()
work Options{..} = do
    liftIO $ print switchPorts
    queryPorts (S.fromList switchPorts) >>= liftIO . print

