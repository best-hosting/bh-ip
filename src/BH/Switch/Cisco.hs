{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module BH.Switch.Cisco (
  module BH.Switch,
  parseMacAddrTable,
  parseCiscoConfig,
  parsePortState,
  findPortMacs,
  findPortState,
  findPortData,
  findPortInfo,
  findMacPort,
  findMacsPort,
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
import System.Directory
import Data.Char
import Control.Applicative.Combinators
import Data.Maybe

import BH.Common
import BH.IP
import BH.IP.Arp
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
parseMacAddrTable :: A.Parser [MacTableEl]
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
      *> (MacTableEl <$> lexemeA vlanP <*> lexemeA macP <*> portP)
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

-- | Find all mac addresses visible on port and return 'PortData'.
findPortData :: PortNum -> T2.TelnetRunM TelnetParserResult a b PortData
findPortData p = do
  portAddrs <- findPortMacs p
  portState <- findPortState p
  return PortData{..}

findPortInfo :: [PortNum] -> T2.TelnetRunM TelnetParserResult a b PortInfo
findPortInfo = foldr (\p mz -> M.insert p <$> findPortData p <*> mz) (pure mempty)

-- FIXME: The same mac may be used in different vlans. Should i handle this
-- correctly? That means, i should have maps indexed by (MacAddr, Vlan)
-- tuple. [network]
-- FIXME: The same mac may be seen on different ports. Should i handle thie
-- too? [network]
findMacPort :: MacAddr -> T2.TelnetRunM TelnetParserResult a b PortInfo
findMacPort mac = do
  SwData{..} <- asks T2.switchData
  let notTrunks = filter ((`notElem` swTrunkPorts) . elPort)
  ps <- notTrunks
        <$> T2.sendAndParse pResPortInfoL
            parseMacAddrTable
            (T2.cmd $ "show mac address-table address " <> T.pack (showMacAddr mac))
  case ps of
    []    -> return mempty
    (_:_) ->
      let upPort portAddrs = PortData{portState = Up, ..}
      in  foldr
            (\p mz -> M.insertWith (<>) p <$> (upPort <$> findPortMacs p) <*> mz)
            (pure mempty)
            (map elPort ps)

findMacsPort :: [MacAddr] -> T2.TelnetRunM TelnetParserResult a b (M.Map MacAddr PortInfo)
findMacsPort = foldr (\p mz -> M.insert p <$> findMacPort p <*> mz) (pure mempty)

