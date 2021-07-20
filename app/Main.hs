{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

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
    swInfo <- parseSwInfo <$> T.readFile "authinfo.txt"
    print swInfo
    opts <- O.customExecParser (O.prefs O.showHelpOnError) $
      O.info (O.helper <*> optParser swInfo)
      (  O.fullDesc
      <> O.header "General program title/description"
      <> O.progDesc "What does this thing do?"
      )
    work swInfo opts

work :: M.Map SwName SwInfo -> Options -> IO ()
work swInfo opts = do
    print opts
    let swports = M.fromList . map (\x -> (x, [])) $ switchPorts opts
        sw = head . M.keys $ swports
    res <- runExceptT $ do
      Just mm <- flip runReaderT swInfo $ run (M.keys swports) getMacs (portSw sw)
      --mm <-  flip runReaderT swInfo $ Main.run
      --mm <-  flip runReaderT swInfo $ runOn
      liftIO $ putStrLn "Gathered ac map:"
      liftIO $ print mm
      arp1 <- queryLinuxArp "certbot"
      liftIO $ putStrLn "Finally, ips..."
      liftIO $ print (macsToIPs arp1 mm)
    case res of
      Right () -> return ()
      Left err -> print err

getIPs :: PortMacMap -> MacIpMap -> [IP]
getIPs portMac macIp = foldr go [] portMac
  where
    go :: Maybe [MacAddr] -> [IP] -> [IP]
    go Nothing zs   = zs
    go (Just ms) zs = foldr goMacs zs ms
    goMacs :: MacAddr -> [IP] -> [IP]
    goMacs m zs = maybe zs (++ zs) (M.lookup m macIp)

macsToIPs :: MacIpMap -> M.Map SwPort [MacAddr] -> M.Map SwPort [IP]
macsToIPs macIp = M.map (foldr go [])
  where
    go :: MacAddr -> [IP] -> [IP]
    go m zs = fromMaybe [] (M.lookup m macIp) ++ zs

