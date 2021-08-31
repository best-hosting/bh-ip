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
  findMac,
  findMacs,
  findPorts,
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
import BH.Telnet

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

-- TODO: queryX functions must return some unified information structure
-- containing all (missed) info, like vlan, port state, IPs, macs, etc.
--data SwPortInfo = SwPortInfo
--                { swPort :: SwPort
--                , swPortAddrs :: M.Map MacAddr [IP]
--                }

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

-- I'm intresting in receiving all ports in question as input, because then i
-- may connect to each switch only once and iterate over all asked ports from
-- this switch.
-- FIXME: Change input to [PortNum]. This module shouldn't care about
-- different switches. In fact, it's quite hard to change ReaderT context. If
-- i want to parametrize over input and require only 'PortNum' part of it, i
-- may use lens 'LensC a PortNum' and then just use it. Then correct switch
-- filtering should be hardcoded into lens in caller function.
{-findMacs :: TelnetCmd [SwPort] (M.Map SwPort [MacAddr]) ()
findMacs t0 = do
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
      (cmd $ "show mac address-table interface " <> showCiscoPortShort portSpec)
   where
    parse :: [PortInfoEl] -> M.Map SwPort [MacAddr]
    parse xs
      | null xs = mempty
      | otherwise = M.singleton swPort (map elMac xs)-}

-- FIXME: May be make this findX functions the most parametric: accept input
-- as first argument, not s Reader: '[PortNum] -> TelnetRunM p a b (M.Map
-- PortNum ..)' and return result as usual. And do all combining in queryX
-- functions.
findMac :: PortNum -> T2.TelnetRunM TelnetParserResult a b MacInfo
findMac p =
  foldMap toMacInfo
    <$> T2.sendAndParse pResPortInfoL
          parseMacAddrTable
          (T2.cmd $ "show mac address-table interface " <> showCiscoPortShort p)

findMac' :: PortNum -> T2.TelnetRunM TelnetParserResult a b SwPortData
findMac' p = do
  portAddrs <- findMac p
  portState <- findPortState' p
  return SwPortData{..}

findMacs :: [PortNum] -> T2.TelnetRunM TelnetParserResult a b (M.Map PortNum MacInfo)
findMacs = foldr (\p mz -> M.insert p <$> findMac p <*> mz) (pure mempty)

findMacs' :: [PortNum] -> T2.TelnetRunM TelnetParserResult a b PortInfo
findMacs' = foldr (\p mz -> M.insert p <$> findMac' p <*> mz) (pure mempty)

-- FIXME: Rewrite to `foldM findPortState` form.
findPortState :: [PortNum] -> T2.TelnetRunM TelnetParserResult a b (M.Map PortNum PortState)
findPortState = foldM go mempty
 where
  go :: M.Map PortNum PortState -> PortNum -> T2.TelnetRunM TelnetParserResult a b (M.Map PortNum PortState)
  go z port = flip (M.insert port) z
    <$> T2.sendAndParse pResPortStateL
          (parsePortState port)
          (T2.cmd $ "show interfaces " <> showCiscoPort port)

findPortState' :: PortNum -> T2.TelnetRunM TelnetParserResult a b PortState
findPortState' p =
  T2.sendAndParse pResPortStateL
          (parsePortState p)
          (T2.cmd $ "show interfaces " <> showCiscoPort p)

findPorts :: TelnetCmd [MacAddr] (M.Map MacAddr (Maybe SwPort)) ()
findPorts t0 = do
  SwInfo{..} <- asks switchInfo
  macs <- asks telnetIn
  foldM (flip (go swName swTrunkPorts)) t0 macs >>= sendExit
 where
  go ::
    SwName ->
    [PortNum] ->
    MacAddr ->
    TelnetCmd [MacAddr] (M.Map MacAddr (Maybe SwPort)) T.Text
  go portSw trunks mac =
    sendAndParse
      (parse <$> parseMacAddrTable)
      (cmd $ "show mac address-table address " <> T.pack (showMacAddr mac))
   where
    parse :: [PortInfoEl] -> M.Map MacAddr (Maybe SwPort)
    parse [] = mempty
    parse [p] =
      let portSpec = elPort p
       in if portSpec `elem` trunks
            then mempty
            else M.singleton mac $ Just SwPort{..}
    parse _ = error "Huyase tut portov"

-- FIXME: The same mac may be used in different vlans. Should i handle this
-- correctly? That means, i should have maps indexed by (MacAddr, Vlan)
-- tuple.
-- FIXME: The same mac may be seen on different ports. Should i handle thie
-- too?
findPort :: MacAddr -> T2.TelnetRunM TelnetParserResult a b PortInfo
findPort mac = do
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
            (\p mz -> M.insertWith (<>) p <$> (upPort <$> findMac p) <*> mz)
            (pure mempty)
            (map elPort ps)

findPorts2 :: [MacAddr] -> T2.TelnetRunM TelnetParserResult a b (M.Map MacAddr PortInfo)
findPorts2 = foldr (\p mz -> M.insert p <$> findPort p <*> mz) (pure mempty)

-- FIXME: Do not use pairs in map value, because printing pair in yaml will
-- result in list, which i confusing. I may define some type with named
-- records.

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
    pState   <- findPortState ports
    pMacInfo <- findMacs ports
    let res :: PortInfo
        res = M.merge M.dropMissing M.dropMissing
                (M.zipWithMatched (\_ portState portAddrs -> SwPortData{..}))
                pState
                pMacInfo
    T2.putResult (M.mapKeys (\p -> SwPort{portSpec = p, ..}) res)
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
    pInfo <- findPorts2 macs
    let res :: M.Map MacAddr SwPortInfo
        res = M.map (M.mapKeys (\p -> SwPort{portSpec = p, ..})) pInfo
    T2.putResult res
    T2.sendExit

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

queryIP ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  IP ->
  m (M.Map IP SwPortInfo)
queryIP = queryIPs . (: [])

