{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module BH.Switch.Cisco (
  module BH.Switch,
  readSwInfo,
  parseMacAddrTable,
  parseCiscoConfig,
  getMacs,
  queryPorts,
  queryPort,
  queryMacs,
  queryMac
) where

import qualified Data.Map as M
import qualified Data.Text as T
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Attoparsec.Combinator as A
import qualified Data.Attoparsec.Text as A
import Data.Either.Combinators
import Data.Functor
import qualified Data.Set as S
import qualified Data.Yaml as Y
import System.Directory
import Data.List

import BH.Common
import BH.IP
import BH.IP.Arp
import BH.Main
import BH.Switch
import BH.Telnet

-- FIXME: Use generic yaml reading func.
readSwInfo :: (MonadIO m, MonadError String m) => FilePath -> m SwInfoMap
readSwInfo file = do
  b <- liftIO (doesFileExist file)
  if b
    then (liftIO (Y.decodeFileEither file) >>= liftEither . mapLeft show) <&> toSwInfoMap
    else throwError ("File with switch info not found " <> file)
 where
  toSwInfoMap :: [SwInfo] -> SwInfoMap
  toSwInfoMap = M.fromList . map (\x -> (swName x, x))

-- | Dashes underlining header of _one_ column. Trailing spaces are consumed,
-- but newline does _not_ .
dashLineA :: A.Parser T.Text
dashLineA =
  A.takeWhile1 (== '-') <* A.takeWhile A.isHorizontalSpace
    A.<?> "column header dash lines for one column"

topHeaderA :: A.Parser T.Text
topHeaderA =
  A.takeWhile1 A.isHorizontalSpace
    *> A.string "Mac Address Table"
    <* A.endOfLine
    <* dashLineA
    <* A.endOfLine
    <* A.skipSpace
    A.<?> "top header"

parseMacAddrTable :: A.Parser [PortInfoEl]
parseMacAddrTable = do
  void $ optional topHeaderA
  portP <-
    symbolA "Vlan" *> symbolA "Mac Address"
      *> ( symbolA "Type" *> symbolA "Ports" $> (symbolA "DYNAMIC" *> lexemeA portNumP)
            <|> symbolA "Ports" *> symbolA "Type" $> (lexemeA portNumP <* symbolA "DYNAMIC")
         )
      <* A.endOfLine
      <* A.count 4 dashLineA
      <* A.endOfLine
  many $
    A.takeWhile A.isHorizontalSpace
      *> (PortInfoEl <$> lexemeA vlanP <*> lexemeA macP <*> portP)
      -- 'endOfInput' should never match here, because mac table must be
      -- always followed by telnet prompt and 'many' parser should not match
      -- with prompt. On the other hand, matching (or using 'lookAhead') with
      -- prompt explicitly here has little sense either, because prompt will
      -- always be on the new line and i may just terminate match at
      -- 'endOfLine'.
      <* (void A.endOfLine <|> A.endOfInput)

parseCiscoConfig :: A.Parser T.Text
parseCiscoConfig =
  A.takeTill A.isEndOfLine
    <<>> ( A.string "\r\nend\r\n"
            <|> A.takeWhile1 A.isEndOfLine <<>> parseCiscoConfig
         )

-- I'm intresting in receiving all ports in question as input, because then i
-- may connect to each switch only once and iterate over all asked ports from
-- this switch.
getMacs :: TelnetCmd [SwPort] (M.Map SwPort [MacAddr]) ()
getMacs t0 = do
  curSn <- asks (swName . switchInfo)
  -- Filter out input ports just to be sure, but, really, this should be done
  -- by 'runOn' (see 'queryPorts').
  ports <- asks (filter ((== curSn) . portSw) . telnetIn)
  foldM (flip go) t0 ports >>= sendExit
 where
  go :: SwPort -> TelnetCmd [SwPort] (M.Map SwPort [MacAddr]) T.Text
  go swPort@SwPort{..} =
    sendAndParse
      (parse <$> parseMacAddrTable)
      (cmd $ "show mac address-table interface " <> ciscoPortNum portSpec)
   where
    parse :: [PortInfoEl] -> M.Map SwPort [MacAddr]
    parse xs
      | null xs = mempty
      | otherwise = M.singleton swPort (map elMac xs)

-- | Query several ports.
queryPorts ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  [SwPort] ->
  m (M.Map SwPort [(MacAddr, S.Set IP)])
queryPorts switches = do
  Config{..} <- ask
  portMacs <-
    flip runReaderT swInfoMap $
      runOn getPorts getMacs (map portSw switches)
  liftIO $ putStrLn "Gathered ac map:"
  liftIO $ print portMacs
  liftIO $ putStrLn "Finally, ips..."
  forM portMacs $ mapM (\m -> (m,) <$> macToIPs m)
 where
  getPorts :: SwName -> [SwPort]
  getPorts sn = filter ((== sn) . portSw) switches

-- | Query single port.
queryPort ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  SwPort ->
  m (M.Map SwPort [(MacAddr, S.Set IP)])
queryPort = queryPorts . (: [])

findPorts :: TelnetCmd [MacAddr] (M.Map MacAddr (Maybe SwPort)) ()
findPorts t0 = do
    sn <- asks (swName . switchInfo)
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

queryMac :: 
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  MacAddr -> m (M.Map MacAddr (Maybe SwPort, S.Set IP))
queryMac mac = queryMacs [mac]

queryMacs ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  [MacAddr] -> m (M.Map MacAddr (Maybe SwPort, S.Set IP))
queryMacs macs = do
  Config{..} <- ask
  macPorts <- flip runReaderT swInfoMap $
    runTill getMacs findPorts
  M.foldrWithKey go (return mempty) macPorts
 where
  getMacs :: M.Map MacAddr (Maybe SwPort) -> Maybe [MacAddr]
  getMacs res = case filter (`notElem` M.keys res) macs of
    [] -> Nothing
    xs -> Just xs
  go ::
    MonadReader Config m =>
    MacAddr -> Maybe SwPort -> m (M.Map MacAddr (Maybe SwPort, S.Set IP)) -> m (M.Map MacAddr (Maybe SwPort, S.Set IP))
  go mac Nothing mz      = M.insert mac (Nothing, mempty) <$> mz
  go mac swp@(Just _) mz = do
    ips <- macToIPs mac
    M.insert mac (swp, ips) <$> mz

