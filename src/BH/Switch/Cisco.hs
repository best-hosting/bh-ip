{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module BH.Switch.Cisco (
  module BH.Switch,
  readSwInfo,
  parseMacAddrTable,
  parseCiscoConfig,
  parsePortState,
  findPortMacs,
  findPortState,
  findPortData,
  findPortInfo,
  findMacPort,
  findMacsPort,
  queryPort,
  queryPorts,
  queryMac,
  queryMacs,
  queryIP,
  queryIPs,
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Attoparsec.Combinator as A
import qualified Data.Attoparsec.Text as A
import Data.Either.Combinators
import Data.Functor
import Data.List
import qualified Data.Map as M
import qualified Data.Map.Merge.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Yaml as Y
import System.Directory
import Data.Char
import Control.Applicative.Combinators
import Data.Maybe

import BH.Common
import BH.IP
import BH.IP.Arp
import BH.Main
import BH.Switch

import qualified BH.Telnet2 as T2

-- TODO: Use HsYAML instead of yaml.
-- And, probably, yaml-combinators for parsing?

-- FIXME: New port and switch types:
-- swName, (portNum, portSpeed), [(Mac, Vlan)]
-- swName, portSpec :: PortNum, [(Mac, Vlan)]
-- Map : (swName, portSpec :: PortNum) -> [(Mac, Vlan)]
-- I may want to also add:
-- - vendor to 'MacAddr'
-- - 'Vlan' to 'MacAddr'
-- - port mode (access/trunk) to 'SwPort'
-- - port state (enabled/disabled) to 'SwPort'

-- TODO: MacAddr makes a pair with host. But MacAddr may be of several types:
-- - "physical" - mac address used by server. It may be considered bind to
-- server.
-- - "virtual" - mac address of virtual machine. It may migrate from server
-- (switch port) to server, though physical server connection does not change.
-- - "unknown" - mac address is not assigned to any server.

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

-- FIXME: Rename to 'macAddrTableP'. I should not prepend "cisco" here,
-- because this is entire module is cisco-only.
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

parsePortState :: PortNum -> A.Parser PortState
parsePortState PortNum{..} = do
  x <-
    (,)
      <$> ( (A.string portNumStr A.<?> "wrong port") *> A.string " is "
              *> A.takeWhile1 isWords
              <* A.string ", "
              A.<?> "port status"
          )
      <*> ( A.string "line protocol is " *> A.skipWhile isWords
              *> between (A.string "(") (A.string ")") (A.takeWhile1 isAlpha)
              A.<?> "line protocol"
          )
  toPortState x
 where
  portNumStr :: T.Text
  portNumStr = T.pack $ show portSpeed <> show portSlot <> "/" <> show portNumber
  toPortState :: (T.Text, T.Text) -> A.Parser PortState
  toPortState t
    | t == ("up", "connected") = pure Up
    | t == ("down", "notconnect") = pure NotConnect
    | t == ("administratively down", "disabled") = pure Disabled
    | otherwise = fail $ "Unrecognized port state: '" <> show t <> "'"

-- | Find all mac addresses visible on port.
findPortMacs :: PortNum -> T2.TelnetRunM TelnetParserResult a b MacInfo
findPortMacs p =
  foldMap toMacInfo
    <$> T2.sendAndParse pResPortInfoL
          parseMacAddrTable
          (T2.cmd $ "show mac address-table interface " <> showCiscoPortShort p)

findPortState :: PortNum -> T2.TelnetRunM TelnetParserResult a b PortState
findPortState p =
  T2.sendAndParse pResPortStateL
          (parsePortState p)
          (T2.cmd $ "show interfaces " <> showCiscoPort p)

-- | Find all mac addresses visible on port and return 'SwPortData'.
findPortData :: PortNum -> T2.TelnetRunM TelnetParserResult a b SwPortData
findPortData p = do
  portAddrs <- findPortMacs p
  portState <- findPortState p
  return SwPortData{..}

findPortInfo :: [PortNum] -> T2.TelnetRunM TelnetParserResult a b PortInfo
findPortInfo = foldr (\p mz -> M.insert p <$> findPortData p <*> mz) (pure mempty)

-- FIXME: The same mac may be used in different vlans. Should i handle this
-- correctly? That means, i should have maps indexed by (MacAddr, Vlan)
-- tuple. [network]
-- FIXME: The same mac may be seen on different ports. Should i handle thie
-- too? [network]
findMacPort :: MacAddr -> T2.TelnetRunM TelnetParserResult a b PortInfo
findMacPort mac = do
  SwInfo{..} <- asks T2.switchInfo
  let notTrunks = filter ((`notElem` swTrunkPorts) . elPort)
  ps <- notTrunks
        <$> T2.sendAndParse pResPortInfoL
            parseMacAddrTable
            (T2.cmd $ "show mac address-table address " <> T.pack (showMacAddr mac))
  case ps of
    []    -> return mempty
    (_:_) ->
      let upPort portAddrs = SwPortData{portState = Up, ..}
      in  foldr
            (\p mz -> M.insertWith (<>) p <$> (upPort <$> findPortMacs p) <*> mz)
            (pure mempty)
            (map elPort ps)

findMacsPort :: [MacAddr] -> T2.TelnetRunM TelnetParserResult a b (M.Map MacAddr PortInfo)
findMacsPort = foldr (\p mz -> M.insert p <$> findMacPort p <*> mz) (pure mempty)

-- | Query several ports.
queryPorts ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  [SwPort] ->
  m SwPortInfo
queryPorts switches = do
  Config{..} <- ask
  portMacs <- flip runReaderT swInfoMap
    $ T2.runOn getPorts (map portSw switches) queryPorts'
  liftIO $ putStrLn "Gathered ac map:"
  liftIO $ print portMacs
  liftIO $ putStrLn "Finally, ips..."
  return (resolveIPs2 macIpMap portMacs)
 where
  getPorts :: SwName -> [PortNum]
  getPorts sn = map portSpec . filter ((== sn) . portSw) $ switches
  queryPorts' :: T2.TelnetRunM TelnetParserResult [PortNum] SwPortInfo ()
  queryPorts' = do
    portSw <- asks (swName . T2.switchInfo)
    ports  <- asks T2.telnetIn
    T2.sendCmd (T2.cmd "terminal length 0")
    res <- M.mapKeys (\p -> SwPort{portSpec = p, ..}) <$> findPortInfo ports
    T2.putResult res
    T2.sendExit

-- | Query single port.
queryPort ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  SwPort ->
  m SwPortInfo
queryPort = queryPorts . (: [])

queryMac ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  MacAddr ->
  m (M.Map MacAddr SwPortInfo)
queryMac mac = queryMacs [mac]

resolveIPs2 :: MacIpMap -> SwPortInfo -> SwPortInfo
resolveIPs2 macIpMap = M.map $ modifyL portAddrsL (resolveIPs macIpMap)

queryMacs ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  [MacAddr] ->
  m (M.Map MacAddr SwPortInfo)
queryMacs macs = do
  Config{..} <- ask
  macPorts <- flip runReaderT swInfoMap $ T2.runTill getMacs queryMacs'
  return (M.map (resolveIPs2 macIpMap) macPorts)
 where
  getMacs :: M.Map MacAddr SwPortInfo -> Maybe [MacAddr]
  getMacs res
    | res == M.empty = Just macs
    | otherwise = case M.keys . M.filter (== M.empty) $ res of
                    [] -> Nothing
                    xs -> Just xs
  queryMacs' :: T2.TelnetRunM TelnetParserResult [MacAddr] (M.Map MacAddr SwPortInfo) ()
  queryMacs' = do
    portSw <- asks (swName . T2.switchInfo)
    macs  <- asks T2.telnetIn
    res <- M.map (M.mapKeys (\p -> SwPort{portSpec = p, ..})) <$> findMacsPort macs
    T2.putResult res
    T2.sendExit

queryIP ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  IP ->
  m (M.Map IP SwPortInfo)
queryIP = queryIPs . (: [])

queryIPs ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  [IP] ->
  m (M.Map IP SwPortInfo)
queryIPs ips = do
  Config{..} <- ask
  let macs = mapMaybe (flip M.lookup ipMacMap) ips
  macPorts <- queryMacs macs
  let go :: IP -> M.Map IP SwPortInfo -> M.Map IP SwPortInfo
      go ip acc = flip (M.insert ip) acc . fromMaybe mempty $ do
        mac <- M.lookup ip ipMacMap
        M.lookup mac macPorts
  return (foldr go mempty ips)

