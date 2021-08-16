{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module BH.Switch (
  SwName (..),
  SwInfo (..),
  SwInfoMap,
  SwPort (..),
  swPortP,
  swPortP',
  showSwPort,
  PortState (..),
  portStateP,
  SwPortInfo (..),
  SwConfig,
  PortInfoEl (..),
  toMacInfo,
  PortNum (..),
  portNumP,
  portNumP',
  showCiscoPortShort,
  showCiscoPort,
  PortSpeed (..),
  portSpeedP,
  showPortSpeedShort,
  TelnetParserResult,
  pResPortInfoL,
  pResTextL,
) where

import Control.Applicative
import Control.Applicative.Combinators
import Data.Aeson
import qualified Data.Aeson.Encoding as J
import qualified Data.Attoparsec.Combinator as A
import qualified Data.Attoparsec.Text as A
import Data.Char
import qualified Data.Map as M
import qualified Data.Text as T
import Network.Simple.TCP
import Data.Monoid

import BH.Common
import BH.IP

newtype SwName = SwName {getSwName :: T.Text}
  deriving (Eq, Ord, Show)

instance ToJSON SwName where
  toJSON (SwName v) = toJSON v
  toEncoding (SwName v) = toEncoding v

instance FromJSON SwName where
  parseJSON = withText "SwName" (\t -> pure (SwName t))

-- FIXME: 'swHost' should be 'Either HostName IP'.
data SwInfo = SwInfo
  { swName :: SwName
  , swHost :: HostName
  , swUser :: T.Text
  , swPassword :: T.Text
  , swRootPassword :: T.Text
  , swDefaultPortSpeed :: PortSpeed
  , swDefaultPortSlot :: Int
  , swTrunkPorts :: [PortNum]
  }
  deriving (Show)

instance ToJSON SwInfo where
  toJSON SwInfo{..} =
    object $
      [ "name" .= swName
      , "host" .= swHost
      , "user" .= swUser
      , "password" .= swPassword
      , "rootPassword" .= swRootPassword
      , "defaultPortSpeed" .= swDefaultPortSpeed
      , "defaultPortSlot" .= swDefaultPortSlot
      , "trunkPorts" .= swTrunkPorts
      ]

instance FromJSON SwInfo where
  parseJSON = withObject "SwInfo" $ \v ->
    SwInfo
      <$> v .: "name"
      <*> v .: "host"
      <*> v .: "user"
      <*> v .: "password"
      <*> v .: "rootPassword"
      <*> v .: "defaultPortSpeed"
      <*> v .: "defaultPortSlot"
      <*> v .:? "trunkPorts" .!= []

type SwInfoMap = M.Map SwName SwInfo

-- TODO: Add and check for 'disabled' port state.
data SwPort = SwPort {portSw :: SwName, portSpec :: PortNum}
  deriving (Eq, Ord, Show)

instance ToJSON SwPort where
  toJSON = toJSON . showSwPort
  toEncoding = toEncoding . showSwPort
instance ToJSONKey SwPort where
  toJSONKey = ToJSONKeyText showSwPort (J.text . showSwPort)

instance FromJSON SwPort where
  parseJSON = withText "SwPort" (either fail pure . A.parseOnly swPortP)
instance FromJSONKey SwPort where
  fromJSONKey = FromJSONKeyTextParser (either fail pure . A.parseOnly swPortP)

-- FIXME: "show interfaces fa0/3 switchport" and there look for
-- "Administrative Mode" and "Trunking Native Mode VLAN". "show interfaces
-- switchport gi3" and "show interfaces status gi3" for sw0.
data PortMode
  = Access
  | Trunk {defVlan :: Vlan}
  deriving (Show)

-- FIXME: "show interfaces configuration gi3" for sw0.
-- FIXME: Move all cisco-related code to Sw.Cisco module.
data PortState = Up | NotConnect | Disabled
  deriving (Show)

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

data SwPortInfo = SwPortInfo
  { portState :: PortState
  , --, portMode :: PortMode
    portAddrs :: [MacInfo]
  }

-- | Parse fully specified switch port.
swPortP :: A.Parser SwPort
swPortP = swPortP' (const (Nothing, Nothing))

-- | Parse switch port providing function for determining defaults for port
-- specification parser.
swPortP' :: (SwName -> (Maybe PortSpeed, Maybe Int)) -> A.Parser SwPort
swPortP' getDefs = do
  pn <- SwName <$> A.takeWhile1 (/= '/') <* A.string "/" A.<?> "switch name"
  let (defSpeed, defSlot) = getDefs $ pn
  SwPort pn <$> portNumP' defSpeed defSlot A.<?> "switch port"

showSwPort :: SwPort -> T.Text
showSwPort SwPort{..} = getSwName portSw <> "/" <> showCiscoPortShort portSpec

type SwConfig = M.Map SwName T.Text

data PortInfoEl = PortInfoEl
  { elVlan :: Vlan
  , elMac :: MacAddr
  , elPort :: PortNum
  }
  deriving (Show)

toMacInfo :: PortInfoEl -> MacInfo
toMacInfo PortInfoEl{..} = MacInfo{macAddr = elMac, macVlan = elVlan, macIPs = mempty}

data PortSpeed = FastEthernet | GigabitEthernet
  deriving (Eq, Ord, Read, Show)

instance ToJSON PortSpeed where
  toJSON = toJSON . show
  toEncoding = toEncoding . show

instance FromJSON PortSpeed where
  parseJSON = withText "PortSpeed" (either fail pure . A.parseOnly portSpeedP)

portSpeedP :: A.Parser PortSpeed
portSpeedP =
  A.string "FastEthernet" *> pure FastEthernet
    <|> A.string "GigabitEthernet" *> pure GigabitEthernet
    A.<?> "port speed"

showPortSpeedShort :: PortSpeed -> T.Text
showPortSpeedShort FastEthernet = "fa"
showPortSpeedShort GigabitEthernet = "gi"

data PortNum = PortNum
  { portSpeed :: PortSpeed
  , portSlot :: Int
  , portNumber :: Int
  }
  deriving (Eq, Ord, Show)

-- FIXME: Provide 'toEncoding' instances derived from Generics.
instance ToJSON PortNum where
  toJSON = toJSON . showCiscoPortShort
  toEncoding = toEncoding . showCiscoPortShort

instance FromJSON PortNum where
  parseJSON = withText "PortNum" (either fail pure . A.parseOnly portNumP)

-- | Parse fully specified port number.
portNumP :: A.Parser PortNum
portNumP = portNumP' Nothing Nothing

-- | Parse port number providing default port speed and default port slot.
portNumP' ::
  -- | Default port speed.
  Maybe PortSpeed ->
  -- | Default port slot.
  Maybe Int ->
  A.Parser PortNum
portNumP' defSpeed defSlot = do
  p <-
    A.lookAhead $
      ( A.string "FastEthernet" <|> A.string "Fa" <|> A.string "fa"
          <|> A.string "GigabitEthernet"
          <|> A.string "Gi"
          <|> A.string "gi"
      )
        *> pure p1
        <|> pure p2
  p <*> A.decimal A.<?> "port number"
 where
  -- Parse speed and port number from 'fa0/3', 'fa/3' and 'fa3'.  In last
  -- two variants default port slot is /required/.
  p1 :: A.Parser (Int -> PortNum)
  p1 =
    PortNum
      <$> ( (A.string "FastEthernet" <|> A.string "Fa" <|> A.string "fa")
              *> pure FastEthernet
              <|> (A.string "GigabitEthernet" <|> A.string "Gi" <|> A.string "gi")
                *> pure GigabitEthernet
          )
      <*> ( A.decimal <* A.string "/"
              <|> maybe (fail "No default port slot") pure defSlot <* optional (A.string "/")
              A.<?> "slot number"
          )
  -- If port speed is not specified explicitly, both default port speed and
  -- port slot are /required/.
  p2 :: A.Parser (Int -> PortNum)
  p2 =
    PortNum
      <$> maybe (fail "No default port speed") pure defSpeed
      <*> maybe (fail "No default port slot") pure defSlot A.<?> "slot number"

-- FIXME: Some cisco switches does not have slots.

-- | Print 'PortNum' in a format understand by cisco.
showCiscoPortShort :: PortNum -> T.Text
showCiscoPortShort PortNum{..} =
  showPortSpeedShort portSpeed <> T.pack (show portSlot) <> "/" <> T.pack (show portNumber)

showCiscoPort :: PortNum -> T.Text
showCiscoPort PortNum{..} =
  T.pack $ show portSpeed <> show portSlot <> "/" <> show portNumber

data TelnetParserResult = TelnetParserResult
    { pResPortInfo :: First (A.Result [PortInfoEl])
    , pResText :: First (A.Result T.Text)
    }
  deriving (Show)

instance Semigroup TelnetParserResult where
  x <> y = TelnetParserResult
            { pResPortInfo = pResPortInfo x <> pResPortInfo y
            , pResText = pResText x <> pResText y
            }

instance Monoid TelnetParserResult where
  mempty = TelnetParserResult
            { pResPortInfo = mempty
            , pResText = mempty
            }

pResPortInfoL :: LensC TelnetParserResult (Maybe (A.Result [PortInfoEl]))
pResPortInfoL g z@TelnetParserResult{pResPortInfo = First x} = (\x' -> z{pResPortInfo = First x'}) <$> g x

pResTextL :: LensC TelnetParserResult (Maybe (A.Result T.Text))
pResTextL g z@TelnetParserResult{pResText = First x} = (\x' -> z{pResText = First x'}) <$> g x

