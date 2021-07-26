{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

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

import BH.Main
import BH.IP
import BH.IP.Arp
import BH.Switch
import BH.Telnet

getMacs :: TelnetCmd [SwPort] (Maybe (M.Map SwPort [MacAddr])) ()
getMacs t0 = do
    curSn <- asks (swName . switchInfo)
    ps    <- asks (filter ((== curSn) . portSw) . telnetIn)
    foldM (flip go) t0 ps >>= sendExit
  where
    go :: SwPort -> TelnetCmd [SwPort] (Maybe (M.Map SwPort [MacAddr])) T.Text
    go pid@SwPort{portSpec = pn} ts =
        sendAndParse (parse <$> parseMacAddrTable)
          (defCmd $ "show mac address-table interface " <> ciscoPortNum pn) ts
      where
        parse :: [PortInfoEl] -> Maybe (M.Map SwPort [MacAddr])
        parse xs = if null xs
                      then Nothing
                      else Just $ M.singleton pid (map elMac xs)

data Options = Options {switchPorts :: [SwPort]}
  deriving (Show)

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
                       in  (defaultPortSpeed <$> m, defaultPortSlot <$> m)

main :: IO ()
main = do
    -- TODO: I may use single for storing authinfo and other config. This file
    -- may be encrypted with gpg and edited with sops. Alternatively, i may
    -- split config..
    swInfoMap <- parseSwInfo <$> T.readFile "authinfo.txt"
    print swInfoMap
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
work opts = do
    Config{..} <- ask
    liftIO $ print opts
    let swports = M.fromList . map (\x -> (x, [])) $ switchPorts opts
        sw = head . M.keys $ swports
    mm <- flip runReaderT swInfoMap $ run (M.keys swports) getMacs (portSw sw)
    case mm of
      Just portMacs -> do
        liftIO $ putStrLn "Gathered ac map:"
        liftIO $ print portMacs
        liftIO $ putStrLn "Finally, ips..."
        portIPs <- forM portMacs $ \macs -> mconcat . catMaybes <$> mapM macToIP macs
        liftIO $ print portIPs
      Nothing -> throwError "Huynya"

