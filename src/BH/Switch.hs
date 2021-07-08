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

import qualified Data.Attoparsec.Text as A
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
    case runParser parseMacAddrTable2 "huy" (T.pack c) of
      Left err -> putStrLn (errorBundlePretty err)
      Right ts -> print ts

t2 :: IO ()
t2 = do
    c <- readFile "1.tmp"
    case runParser parseMacAddrTable3 "huy" (T.pack c) of
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


vV' :: Parser T.Text
vV' = symbol "Vlan" *> pure "Parse vlan"
vM' :: Parser T.Text
vM' = symbol "Mac Address" *> pure "Parse mac"
vP' :: Parser T.Text
vP' = symbol "Port" *> pure "Parse port"


parseVlan :: Parser Int
parseVlan  = lexeme $ do
    o <- getOffset
    region (setErrorOffset o) . label "vlan number" $ do
      v <- lexeme L.decimal
      if v < 4096
        then return v
        else fail "Nihuya sebe vlan"

parseMacAddress :: Parser T.Text
parseMacAddress = lexeme $ takeWhile1P (Just "mac address") ((||) <$> isHexDigit <*> (== '.'))

parsePortNum :: Parser PortNum2
parsePortNum    = lexeme $ PortNum2
    <$> (symbol "Fa0" *> pure FastEthernet <|> symbol "Gi0" *> pure GigabitEthernet)
    <*> (symbol "/" *> L.decimal)

columnVlanP :: Parser (PortInfoEl -> Parser PortInfoEl)
columnVlanP = symbol "Vlan" *> pure (\p -> (\x -> p{elVlan = x}) <$> parseVlan)
                <?> "column header 'Vlan'"

columnMacAddressP :: Parser (PortInfoEl -> Parser PortInfoEl)
columnMacAddressP = symbol "Mac Address" *> pure (\p -> (\x -> p{elMac = x}) <$> parseMacAddress)
                      <?> "column header 'Mac Address'"

columnPortNumP :: Parser (PortInfoEl -> Parser PortInfoEl)
columnPortNumP = symbol "Ports" *> pure (\p -> (\x -> p{elPort = x}) <$> parsePortNum)
                   <?> "column header 'Ports'"

columnTypeP :: Parser (PortInfoEl -> Parser PortInfoEl)
columnTypeP = symbol "Type" *> pure (\p -> symbol "DYNAMIC" *> pure p)
                <?> "column header 'Type'"

macAddrTableColumns :: [Parser (PortInfoEl -> Parser PortInfoEl)]
macAddrTableColumns =
    [ columnVlanP
    , columnMacAddressP
    , columnTypeP
    , columnPortNumP
    ]

topHeader :: Parser T.Text
topHeader  = hspace1
    *> symbol "Mac Address Table" <* eol
    <* takeWhile1P (Just "top header dashes line") (== '-') <* eol
    <* skipMany space1
    <?> "top header"

-- | Parse table header using supplied list of column header parsers. All columns
-- are mandatory, though they can be used in any order. Return a list columns
-- in column header parser results in _actual_ column order.
--
-- This can be used to build a table row parser, if each column header parser
-- return corresponding column cell parser.
parseTableHeader :: [Parser a] -> Parser [a]
parseTableHeader cols =
    let l = length cols
        dashLine = lexeme $ takeWhile1P (Just "column header dash lines for _each_ column") (== '-')
    in  optional topHeader
          *> choiceEachOnce cols <* eol
          <* count l dashLine <* eol

-- | Parse table using supplied column /header/ parsers, which return
-- corresponding row cell parser. Row value is build based on empty row value.
parseTable :: a -> [Parser (a -> Parser a)] -> Parser [a]
parseTable emptyRow cols = do
    cellPs <- parseTableHeader cols
    let rowP = hspace *> foldl (>>=) (pure emptyRow) cellPs <* (void eol <|> eof)
    many rowP

parseMacAddrTable2 :: Parser [PortInfoEl]
parseMacAddrTable2 = parseTable defaultPortInfoEl macAddrTableColumns

-- | Another (simpler) mac table parsing variant.
parseMacAddrTable3 :: Parser [PortInfoEl]
parseMacAddrTable3 = do
    let dashLine = lexeme $ takeWhile1P (Just "column header dash lines for one column") (== '-')
    optional topHeader
    portNumP <- symbol "Vlan" *> symbol "Mac Address"
      *> (  try (symbol "Type"  *> symbol "Ports" *> pure (symbol "DYNAMIC" *> parsePortNum))
            <|>  symbol "Ports" *> symbol "Type"  *> pure (parsePortNum <* symbol "DYNAMIC")
         ) <* eol
      <* count 4 dashLine <* eol
    many $ hspace
      *> (PortInfoEl <$> parseVlan <*> parseMacAddress <*> portNumP)
      <* (void eol <|> eof)

topHeader4 :: A.Parser T.Text
topHeader4  = A.takeWhile1 A.isHorizontalSpace
    *> A.string "Mac Address Table" <* A.endOfLine
    <* dashLine4 <* A.endOfLine
    <* A.skipSpace
    A.<?> "top header"

dashLine4 :: A.Parser T.Text
dashLine4 = lexemeA (A.takeWhile1 (== '-'))
            A.<?> "column header dash lines for one column"

symbolA :: T.Text -> A.Parser T.Text
symbolA t = A.string t <* A.takeWhile1 A.isHorizontalSpace

lexemeA :: A.Parser a -> A.Parser a
lexemeA p = p <* A.takeWhile1 A.isHorizontalSpace

parseMacAddrTable4 :: A.Parser [PortInfoEl]
parseMacAddrTable4 = do
    optional topHeader4
    portNumP <- symbolA "Vlan" *> symbolA "Mac Address"
      *> (  try (symbolA "Type"  *> symbolA "Ports" *> pure (symbolA "DYNAMIC" *> parsePortNum))
            <|>  symbolA "Ports" *> symbolA "Type"  *> pure (parsePortNum <* symbolA "DYNAMIC")
         ) <* A.endOfLine
      <* count 4 dashLine <* eol
    many $ hspace
      *> (PortInfoEl <$> parseVlan <*> parseMacAddress <*> portNumP)
      <* (void eol <|> eof)
    return []

{-
    optional topHeader
    portNumP <- symbol "Vlan" *> symbol "Mac Address"
      *> (  try (symbol "Type"  *> symbol "Ports" *> pure (symbol "DYNAMIC" *> parsePortNum))
            <|>  symbol "Ports" *> symbol "Type"  *> pure (parsePortNum <* symbol "DYNAMIC")
         ) <* eol
      <* count 4 dashLine <* eol
    many $ hspace
      *> (PortInfoEl <$> parseVlan <*> parseMacAddress <*> portNumP)
      <* (void eol <|> eof)-}

