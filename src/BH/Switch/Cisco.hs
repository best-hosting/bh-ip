{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module BH.Switch.Cisco (
  module BH.Switch,
  macTableP,
  switchConfigP,
  portStateP,
  findPortMacs,
  findPortState,
  findPortData,
  findPortInfo,
  findMacPort,
  findMacsPort,
  findMacData,
  findMacInfo,
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import qualified Data.Attoparsec.Combinator as A
import qualified Data.Attoparsec.Text as A
import Data.Functor
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Char
import Control.Applicative.Combinators
import Data.Monoid

import BH.Common
import BH.IP
import BH.Switch
import BH.Main.Types

import BH.Telnet

-- TODO: Use HsYAML instead of yaml.  And, probably, yaml-combinators for
-- parsing? [pkg]


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

macTableP :: A.Parser [MacTableEl]
macTableP = do
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

switchConfigP :: A.Parser T.Text
switchConfigP =
  A.takeTill A.isEndOfLine
    <<>> ( A.string "\r\nend\r\n"
            <|> A.takeWhile1 A.isEndOfLine <<>> switchConfigP
         )

portStateP :: PortNum -> A.Parser PortState
portStateP PortNum{..} = do
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
findPortMacs :: PortNum -> TelnetRunM TelnetParserResult a b MacInfo
findPortMacs p = do
  SwData{..} <- asks switchData
  foldMap (toMacInfo swName)
    <$> sendAndParse pMacTableL
          macTableP
          (cmd $ "show mac address-table interface " <> showCiscoPortShort p)

findPortState :: PortNum -> TelnetRunM TelnetParserResult a b PortState
findPortState p = do
  sendAndParse pResPortStateL
          (portStateP p)
          (cmd $ "show interfaces " <> showCiscoPort p)

-- | Find all mac addresses visible on port and return 'PortData'.
findPortData :: PortNum -> TelnetRunM TelnetParserResult a b PortData
findPortData p = do
  portAddrs <- findPortMacs p
  portState <- Last . Just <$> findPortState p
  return PortData{..}

findPortInfo :: [PortNum] -> TelnetRunM TelnetParserResult a b PortInfo
findPortInfo = foldr (\p mz -> M.insert p <$> findPortData p <*> mz) (pure mempty)

-- FIXME: The same mac may be used in different vlans. Should i handle this
-- correctly? That means, i should have maps indexed by (MacAddr, Vlan)
-- tuple. [network]
-- FIXME: The same mac may be seen on different ports. Should i handle thie
-- too? [network]
-- FIXME: During single run this function may be run only on _single_ switch.
-- And on single switch each mac may be only on _single_ port. Thus, i should
-- return 'PortNum' here, not 'PortInfo'.
findMacPort :: MacAddr -> TelnetRunM TelnetParserResult a b PortInfo
findMacPort mac = do
  SwData{..} <- asks switchData
  let notTrunks = filter ((`notElem` swTrunkPorts) . elPort)
  mt <- notTrunks
        <$> sendAndParse pMacTableL
            macTableP
            (cmd $ "show mac address-table address " <> showMacAddr mac)
  case mt of
    []    -> return mempty
    (_:_) ->
      let upPort portAddrs = PortData{portState = Last (Just Up), ..}
      in  foldr
            (\p mz -> M.insertWith (<>) p <$> (upPort <$> findPortMacs p) <*> mz)
            (pure mempty)
            (map elPort mt)

findMacData :: MacAddr -> TelnetRunM TelnetParserResult a b (Maybe MacData)
findMacData mac = do
  SwData{..} <- asks switchData
  let notTrunks = filter ((`notElem` swTrunkPorts) . elPort)
  mt <- notTrunks
        <$> sendAndParse pMacTableL
            macTableP
            (cmd $ "show mac address-table address " <> showMacAddr mac)
  case mt of
    []    -> return Nothing
    [x]   -> return . Just $ toMacData swName x
    (_:_) -> error "Several ports for a single mac"

findMacsPort :: [MacAddr] -> TelnetRunM TelnetParserResult a b (M.Map MacAddr PortInfo)
findMacsPort = foldr (\m mz -> M.insert m <$> findMacPort m <*> mz) (pure mempty)

findMacInfo :: [MacAddr] -> TelnetRunM TelnetParserResult a b MacInfo
findMacInfo = foldr (\m mz -> maybe id (M.insert m) <$> findMacData m <*> mz) (pure mempty)

