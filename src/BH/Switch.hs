{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module BH.Switch (
  SwName (..),
  SwInfo (..),
  SwInfoMap,
  readSwInfo,
  SwPort (..),
  swPortP,
  swPortP',
  PortMacMap,
  MacPortMap,
  SwConfig,
  PortInfoEl (..),
  defaultPortInfoEl,
  PortNum (..),
  portNumP,
  portNumP',
  ciscoPortNum,
  PortSpeed (..),
  portSpeedP,
  parseMacAddrTable,
  parseCiscoConfig,
) where

import qualified Data.Map as M
import qualified Data.Text as T
import Network.Simple.TCP

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Attoparsec.Combinator as A
import qualified Data.Attoparsec.Text as A
import Data.Char
import Data.Either.Combinators
import qualified Data.Yaml as Y
import System.Directory

import BH.Common
import BH.IP

newtype SwName = SwName T.Text
  deriving (Eq, Ord, Show)

instance ToJSON SwName where
  toJSON (SwName v) = String v

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

type SwInfoMap = M.Map SwName SwInfo

readSwInfo :: (MonadIO m, MonadError String m) => FilePath -> m SwInfoMap
readSwInfo file = do
  b <- liftIO (doesFileExist file)
  if b
    then liftIO (Y.decodeFileEither file) >>= liftEither . mapLeft show >>= \x -> return (toSwInfoMap x)
    else throwError ("File with switch info not found " <> file)
 where
  toSwInfoMap :: [SwInfo] -> SwInfoMap
  toSwInfoMap = M.fromList . map (\x -> (swName x, x))

-- TODO: Add and check for 'disabled' port state.
data SwPort = SwPort {portSw :: SwName, portSpec :: PortNum}
  deriving (Eq, Ord, Show)

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

type PortMacMap = M.Map SwPort (Maybe [MacAddr])

-- FIXME: New port and switch types:
-- swName, (portNum, portSpeed), [(Mac, Vlan)]
-- swName, portSpec :: PortNum, [(Mac, Vlan)]
-- Map : (swName, portSpec :: PortNum) -> [(Mac, Vlan)]

type MacPortMap = M.Map MacAddr (Maybe [SwPort])

type PortMap = M.Map SwPort [(MacAddr, [IP])]

type SwConfig = M.Map SwName T.Text

data PortInfoEl = PortInfoEl
  { elVlan :: Vlan
  , elMac :: MacAddr
  , elPort :: PortNum
  }
  deriving (Show)

-- FIXME: Do not use this.
defaultPortInfoEl :: PortInfoEl
defaultPortInfoEl = PortInfoEl{elVlan = Vlan 0, elMac = defMacAddr, elPort = PortNum{portSpeed = FastEthernet, portSlot = 0, portNumber = 0}}

data PortSpeed = FastEthernet | GigabitEthernet
  deriving (Eq, Ord, Read, Show)

instance ToJSON PortSpeed where
  toJSON = String . T.pack . show

instance FromJSON PortSpeed where
  parseJSON = withText "PortSpeed" (either fail pure . A.parseOnly portSpeedP)

portSpeedP :: A.Parser PortSpeed
portSpeedP =
  A.string "FastEthernet" *> pure FastEthernet
    <|> A.string "GigabitEthernet" *> pure GigabitEthernet
    A.<?> "port speed"

data PortNum = PortNum
  { portSpeed :: PortSpeed
  , portSlot :: Int
  , portNumber :: Int
  }
  deriving (Eq, Ord, Show)

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
      (A.string "Fa" <|> A.string "fa" <|> A.string "Gi" <|> A.string "gi")
        *> pure p1
        <|> pure p2
  p <*> A.decimal A.<?> "port number"
 where
  -- Parse speed and port number from 'fa0/3', 'fa/3' and 'fa3'.  In last
  -- two variants default port slot is /required/.
  p1 :: A.Parser (Int -> PortNum)
  p1 =
    PortNum
      <$> ( (A.string "Fa" <|> A.string "fa") *> pure FastEthernet
              <|> (A.string "Gi" <|> A.string "gi") *> pure GigabitEthernet
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

-- | Print 'PortNum' in a format understand by cisco.
ciscoPortNum :: PortNum -> T.Text
ciscoPortNum PortNum{..} =
  T.pack $ show portSpeed <> " " <> show portSlot <> "/" <> show portNumber

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
  portNumP <-
    symbolA "Vlan" *> symbolA "Mac Address"
      *> ( symbolA "Type" *> symbolA "Ports" *> pure (symbolA "DYNAMIC" *> lexemeA portNumP)
            <|> symbolA "Ports" *> symbolA "Type" *> pure (lexemeA portNumP <* symbolA "DYNAMIC")
         )
      <* A.endOfLine
      <* A.count 4 dashLineA
      <* A.endOfLine
  many $
    A.takeWhile A.isHorizontalSpace
      *> (PortInfoEl <$> lexemeA vlanP <*> lexemeA macP <*> portNumP)
      <* (void A.endOfLine <|> A.endOfInput)

-- 'endOfInput' should never match here, because mac table must be always
-- followed by telnet prompt and 'many' parser should not match with
-- prompt. On the other hand, matching (or using 'lookAhead') with prompt
-- explicitly here has little sense either, because prompt will always be
-- on the new line and i may just terminate match at 'endOfLine'.

infixr 4 <<>>
(<<>>) :: (Applicative f, Monoid a) => f a -> f a -> f a
(<<>>) = liftA2 (<>)

parseCiscoConfig :: A.Parser T.Text
parseCiscoConfig =
  A.takeTill A.isEndOfLine
    <<>> ( A.string "\r\nend\r\n"
            <|> A.takeWhile1 A.isEndOfLine <<>> parseCiscoConfig
         )
