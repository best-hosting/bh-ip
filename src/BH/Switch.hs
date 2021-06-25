{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TupleSections  #-}

module BH.Switch
    ( SwName (..)
    , SwInfo (..)
    , parseSwInfo
    , PortNum (..)
    , SwPort (..)
    , PortMacMap
    , MacPortMap
    , SwConfig
    , MacIpMap
    , PortMap
    )
  where

-- FIXME: Merge parsing functions for show mac address table. Make them return
-- complete result, disregarding of request. And then requestor should choose,
-- what he wants.

import qualified Data.Text as T
import           Data.List
import           Network.Simple.TCP
import qualified Data.Map as M

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error
import Text.Megaparsec.Debug
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Char
import Control.Applicative (Alternative)
import Data.Function
import Data.Foldable (asum)
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

data Port     = Port {portPrefix :: T.Text, portNum :: Int}
  deriving (Eq, Ord, Show)

{---parsePort :: T.Text -> Either Port
parsePort :: T.Text -> Maybe T.Text
parsePort ts = do
        join $ find (`T.stripPrefix` ts) ["FastEthernet", "Fa", "fa"] >> Just "FastEthernet"
    <|> find (`T.stripPrefix` ts) ["GigabitEthernet", "Gi", "gi"] >> Just "GigabitEthernet"-}

newtype PortNum     = PortNum Int
  deriving (Eq, Ord, Show)

data SwPort2        = SwPort2 {portSw2 :: SwName, portSpec2 :: Port}
  deriving (Eq, Ord, Show)
data SwPort         = SwPort {portSw :: SwName, portSpec :: T.Text, port :: PortNum}
  deriving (Eq, Ord, Show)

type PortMacMap     = M.Map SwPort (Maybe [MacAddr])

type MacPortMap     = M.Map MacAddr (Maybe [SwPort])

type MacIpMap       = M.Map MacAddr [IP]

type PortMap        = M.Map SwPort [(MacAddr, [IP])]

type SwConfig       = M.Map SwName T.Text

type Parser = Parsec Void T.Text


t :: IO ()
t = do
    c <- readFile "1.tmp"
    case runParser parseMacAddressTable2 "huy" (T.pack c) of
      Left err -> putStrLn (errorBundlePretty err)
      Right ts -> print ts

data PortInfoEl = PortInfoEl { elVlan :: Int
                             , elMac  :: T.Text
                             , elPort :: PortNum2
                             }
  deriving (Show)

defaultPortInfoEl :: PortInfoEl
defaultPortInfoEl = PortInfoEl {elVlan = 0, elMac = "0000", elPort = PortNum2 {portSpeed = FastEthernet, portNumber = 0}}

data PortNum2   = PortNum2  { portSpeed :: PortSpeed
                            , portNumber :: Int
                            }
  deriving (Show)

data PortSpeed  = FastEthernet | GigabitEthernet
  deriving (Show)

parseMacAddressTable :: Parser String
parseMacAddressTable = do
    let header =
            space1 *> string "Mac Address Table" <* hspace <* newline
            <* takeWhile1P (Just "dashes") (== '-') <* some newline
            <* string "Vlan" <* hspace1 <* string "Mac Address" <* hspace1 <* string "Type" <* hspace1 <* string "Ports" <* hspace <* newline
            <* takeWhile1P (Just "huynya vsyakaya") (`elem` ['-', ' ']) <* newline
    header *> hspace1 *> (some digitChar <* hspace1)
    *> (some (hexDigitChar <|> oneOf @[] ":.") <* hspace1)
    *> (string "DYNAMIC" <* hspace1)
    *> (foldr (\p r -> (++) <$> p <*> r) (pure [])  [some alphaNumChar, T.unpack <$> string "/", some digitChar] <* hspace) <* newline

sc :: Parser ()
sc = L.space hspace1 empty empty

scn :: Parser ()
scn = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme  = L.lexeme sc

symbol :: Tokens T.Text -> Parser (Tokens T.Text)
symbol  = L.symbol sc

topHeader :: Parser T.Text
topHeader  = hspace1
    *> symbol "Mac Address Table" <* eol
    <* takeWhile1P (Just "top header dashes line") (== '-') <* eol
    <* skipMany space1
    <?> "top header"

colHeader :: Parser T.Text
colHeader =
        symbol "Vlan"
    <|> symbol "Mac Address"
    <|> symbol "Type"
    <|> symbol "Ports"
    <?> "column header"

colParsers :: Parser (PortInfoEl -> Parser PortInfoEl)
colParsers =
        (symbol "Vlan" *> pure parseVlan')
    <|> (symbol "Mac Address" *> pure parseMacAddress')
    <|> (symbol "Type" *> pure (\p -> string "DYNAMIC" *> pure p))
    <|> (symbol "Ports" *> pure parsePortNum')
    <?> "column header"

-- | The same as 'choice', but returns list of _unmatched_ choices.  This may
-- be used for implementing applying each choice only once.
choiceOnce :: Alternative m => [m a] -> m (a, [m a])
choiceOnce ps =
    fmap (go <$>) . asum . zipWith (\n mx -> (, n) <$> mx) [1..] $ ps
  where
    --go :: Alternative m => Int -> [m a]
    go n = foldr (\(i, x) zs -> if n == i then zs else x : zs) [] . zip [1..] $ ps

-- | Apply each choice only once until nothing left.
choiceEachOnce :: (Alternative m, Monad m) => [m a] -> m [a]
choiceEachOnce ps = fix go ps
  where
    go :: (Alternative m, Monad m) => ([m a] -> m [a]) -> [m a] -> m [a]
    go rec ps
      | null ps     = pure []
      | otherwise   = choiceOnce ps >>= \(p, zs) -> (p :) <$> rec zs


vV :: Parser (String, T.Text)
vV = symbol "Vlan" *> pure ("Vlan", "Parse vlan")
vM :: Parser (String, T.Text)
vM = symbol "Mac Address" *> pure ("Mac", "Parse mac")
vP :: Parser (String, T.Text)
vP = symbol "Port" *> pure ("Port", "Parse port")

vV' :: Parser T.Text
vV' = symbol "Vlan" *> pure "Parse vlan"
vM' :: Parser T.Text
vM' = symbol "Mac Address" *> pure "Parse mac"
vP' :: Parser T.Text
vP' = symbol "Port" *> pure "Parse port"

runParserList :: a -> [a -> Parser a] -> Parser a
runParserList z = foldl (>>=) (pure z)

fullHeader :: Parser [T.Text]
fullHeader = optional topHeader
    *> count 4 colHeader
    <* takeWhile1P (Just "column header dashes") (`elem` ['-', ' ']) <* scn
    <?> "full table header"

-- | Parse table header using supplied list of column header parsers. All columns
-- are mandatory, though they can be used in any order. Return a list columns
-- in column header parser results in _actual_ column order.
--
-- This can be used to build a table row parser, if each column header parser
-- return corresponding column cell parser.
parseHeader :: [Parser a] -> Parser [a]
parseHeader cs =
    let l = length cs
        dashLine = lexeme $ takeWhile1P (Just "column header dash lines for _each_ column") (== '-')
    in  optional topHeader
          *> choiceEachOnce cs <* eol
          <* count l dashLine <* eol

parseTable :: Parser [PortInfoEl]
parseTable = do
    rs <- parseHeader [parseVlan'2, parseMacAddress'2, parseType'2 , parsePortNum'2]
    many $ hspace *> runParserList defaultPortInfoEl rs <* (void eol <|> eof)

parseVlan :: Parser Int
parseVlan  = lexeme $ do
    o <- getOffset
    region (setErrorOffset o) . label "vlan number" $ do
      v <- lexeme L.decimal
      if v < 4096
        then return v
        else fail "Nihuya sebe vlan"

parseVlan' :: PortInfoEl -> Parser PortInfoEl
parseVlan' p = (\x -> p{elVlan = x}) <$> parseVlan

parseVlan'2 :: Parser (PortInfoEl -> Parser PortInfoEl)
parseVlan'2 = symbol "Vlan" *> pure (\p -> (\x -> p{elVlan = x}) <$> parseVlan)
                <?> "column header 'Vlan'"

parseMacAddress :: Parser T.Text
parseMacAddress = lexeme $ takeWhile1P (Just "mac address") ((||) <$> isHexDigit <*> (== '.'))

parseMacAddress' :: PortInfoEl -> Parser PortInfoEl
parseMacAddress' p = (\x -> p{elMac = x}) <$> parseMacAddress

parseMacAddress'2 :: Parser (PortInfoEl -> Parser PortInfoEl)
parseMacAddress'2 = symbol "Mac Address" *> pure (\p -> (\x -> p{elMac = x}) <$> parseMacAddress)
    <?> "column header 'Mac Address'"

parsePortNum :: Parser PortNum2
parsePortNum    = lexeme $ PortNum2
    <$> (symbol "Fa0" *> pure FastEthernet <|> symbol "Gi0" *> pure GigabitEthernet)
    <*> (symbol "/" *> L.decimal)

parsePortNum' :: PortInfoEl -> Parser PortInfoEl
parsePortNum' p = (\x -> p{elPort = x}) <$> parsePortNum

parsePortNum'2 :: Parser (PortInfoEl -> Parser PortInfoEl)
parsePortNum'2 = symbol "Ports" *> pure (\p -> (\x -> p{elPort = x}) <$> parsePortNum)
    <?> "column header 'Ports'"

parseType'2 :: Parser (PortInfoEl -> Parser PortInfoEl)
parseType'2 = symbol "Type" *> pure (\p -> symbol "DYNAMIC" *> pure p) <?> "column header 'Type'"

parseRow :: Parser PortInfoEl
parseRow    = PortInfoEl <$> parseVlan <*> parseMacAddress <*> parsePortNum

parseMacAddressTable2 :: Parser String
parseMacAddressTable2 = do
    let header =
            space1 *> string "Mac Address Table" <* hspace <* newline
            <* takeWhile1P (Just "dashes") (== '-') <* some newline
            <* string "Vlan" <* hspace1 <* string "Mac Address" <* hspace1 <* string "Type" <* hspace1 <* string "Ports" <* hspace <* newline
            <* takeWhile1P (Just "huynya vsyakaya") (`elem` ['-', ' ']) <* newline
    header *> hspace1 *> (some digitChar <* hspace1)
    *> (some (hexDigitChar <|> oneOf @[] ":.") <* hspace1)
    *> (string "DYNAMIC" <* hspace1)
    *> (foldr (\p r -> (++) <$> p <*> r) (pure [])  [some alphaNumChar, T.unpack <$> string "/", some digitChar] <* hspace) <* newline

