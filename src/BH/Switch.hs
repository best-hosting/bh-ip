{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards  #-}

module BH.Switch
    ( SwName (..)
    , SwInfo (..)
    , parseSwInfo
    , SwPort (..)
    , PortMacMap
    , MacPortMap
    , SwConfig
    , MacIpMap
    , PortInfoEl (..)
    , defaultPortInfoEl
    , PortNum (..)
    , Vlan (..)
    , ciscoPortNum
    , PortSpeed (..)
    , parseMacAddrTable
    , parseCiscoConfig
    )
  where

import qualified Data.Text as T
import           Network.Simple.TCP
import qualified Data.Map as M

import Control.Applicative
import qualified Data.Attoparsec.Text as A
import Data.Char
import Control.Monad

import BH.IP


newtype SwName      = SwName T.Text
 deriving (Eq, Ord, Show)

data SwInfo         = SwInfo
                        { swName   :: SwName
                        , hostName :: HostName
                        , userName :: T.Text
                        , password :: T.Text
                        , enablePassword :: T.Text
                        , defaultPortSpec :: T.Text
                        }
  deriving (Show)

parseSwInfo :: T.Text -> M.Map SwName SwInfo
parseSwInfo   = M.fromList . map go .  T.lines
  where
    go :: T.Text -> (SwName, SwInfo)
    go t =  let [sn, hn, un, pw, enpw, ds] = T.splitOn ", " t
            in  ( SwName sn
                , SwInfo    { swName   = SwName sn
                            , hostName = T.unpack hn
                            , userName = un
                            , password = pw
                            , enablePassword = enpw
                            , defaultPortSpec = ds
                            }
                )

-- TODO: Add and check for 'disabled' port state.
data SwPort        = SwPort {portSw :: SwName, portSpec :: PortNum}
  deriving (Eq, Ord, Show)

type PortMacMap    = M.Map SwPort (Maybe [MacAddr])

-- FIXME: New port and switch types:
-- swName, (portNum, portSpeed), [(Mac, Vlan)]
-- swName, portSpec :: PortNum, [(Mac, Vlan)]
-- Map : (swName, portSpec :: PortNum) -> [(Mac, Vlan)]

type MacPortMap     = M.Map MacAddr (Maybe [SwPort])

type MacIpMap       = M.Map MacAddr [IP]

type PortMap       = M.Map SwPort [(MacAddr, [IP])]

type SwConfig       = M.Map SwName T.Text


newtype Vlan = Vlan Int
  deriving (Show)
data PortInfoEl = PortInfoEl { elVlan :: Vlan
                             , elMac  :: T.Text
                             , elPort :: PortNum
                             }
  deriving (Show)

defaultPortInfoEl :: PortInfoEl
defaultPortInfoEl = PortInfoEl {elVlan = Vlan 0, elMac = "0000", elPort = PortNum {portSpeed = FastEthernet, portSlot = 0, portNumber = 0}}

data PortNum  = PortNum { portSpeed  :: PortSpeed
                        , portSlot   :: Int
                        , portNumber :: Int
                        }
  deriving (Eq, Ord, Show)

-- | Print 'PortNum' in a format understand by cisco.
ciscoPortNum :: PortNum -> T.Text
ciscoPortNum PortNum{..} =
    T.pack $ show portSpeed <> " " <> show portSlot <> "/" <> show portNumber

data PortSpeed  = FastEthernet | GigabitEthernet
  deriving (Eq, Ord, Show)

-- Below are attoparsec version.
symbolA :: T.Text -> A.Parser T.Text
symbolA   = lexemeA . A.string

lexemeA :: A.Parser a -> A.Parser a
lexemeA p = p <* A.takeWhile A.isHorizontalSpace

parseVlanA :: A.Parser Vlan
parseVlanA  = lexemeA $ do
      v <- lexemeA A.decimal A.<?> "vlan number"
      if v < 4096
        then return (Vlan v)
        else fail "Nihuya sebe vlan"

-- FIXME: Rewrite with 'count'
parseMacAddressA :: A.Parser T.Text
parseMacAddressA = let isMacChars = (||) <$> isHexDigit <*> (== '.')
                   in  lexemeA (A.takeWhile1 isMacChars) A.<?> "mac address"

parsePortNumA :: A.Parser PortNum
parsePortNumA    = lexemeA
    $ ( PortNum
        <$> (symbolA "Fa" *> pure FastEthernet <|> symbolA "Gi" *> pure GigabitEthernet)
        <*> A.decimal
        <*> (symbolA "/" *> A.decimal)
        A.<?> "port number"
      )

-- | Dashes underlining header of _one_ column. Trailing spaces are consumed,
-- but newline does _not_ .
dashLineA :: A.Parser T.Text
dashLineA = A.takeWhile1 (== '-') <* A.takeWhile A.isHorizontalSpace
            A.<?> "column header dash lines for one column"

topHeaderA :: A.Parser T.Text
topHeaderA = A.takeWhile1 A.isHorizontalSpace
    *> A.string "Mac Address Table" <* A.endOfLine
    <* dashLineA <* A.endOfLine
    <* A.skipSpace
    A.<?> "top header"

parseMacAddrTable :: A.Parser [PortInfoEl]
parseMacAddrTable = do
    void $ optional topHeaderA
    portNumP <- symbolA "Vlan" *> symbolA "Mac Address"
      *> (      symbolA "Type"  *> symbolA "Ports" *> pure (symbolA "DYNAMIC" *> parsePortNumA)
            <|> symbolA "Ports" *> symbolA "Type"  *> pure (parsePortNumA <* symbolA "DYNAMIC")
         ) <* A.endOfLine
      <* A.count 4 dashLineA <* A.endOfLine
    many $ A.takeWhile A.isHorizontalSpace
      *> (PortInfoEl <$> parseVlanA <*> parseMacAddressA <*> portNumP)
      <* (void A.endOfLine <|> A.endOfInput)
-- FIXME: end of input termination is probably wrong, because attoparsec may
-- run on whole stream. Probably, it's better to terminate at prompt or smth
-- similar. Or just at end of line. After all, prompt must always be after
-- table and prompt itself won't match with 'many'.

infixr 4 <<>>
(<<>>) :: (Applicative f, Monoid a) => f a -> f a -> f a
(<<>>) = liftA2 (<>)

parseCiscoConfig :: A.Parser T.Text
parseCiscoConfig =
    A.takeTill A.isEndOfLine
      <<>> (   A.string "\r\nend\r\n"
           <|> A.takeWhile1 A.isEndOfLine <<>> parseCiscoConfig
           )

