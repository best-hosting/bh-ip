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
    , PortInfoEl (..)
    , PortNum2 (..)
    , parseMacAddrTable
    , parseCiscoConfig
    )
  where

-- FIXME: Merge parsing functions for show mac address table. Make them return
-- complete result, disregarding of request. And then requestor should choose,
-- what he wants.

import qualified Data.Text as T
import           Data.List
import           Network.Simple.TCP
import qualified Data.Map as M

import Control.Applicative
import qualified Data.Attoparsec.Text as A
import qualified Text.Megaparsec        as M
import qualified Text.Megaparsec.Char   as M
import qualified Text.Megaparsec.Error  as M
import qualified Text.Megaparsec.Debug  as M
import qualified Text.Megaparsec.Char.Lexer as ML
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

type Parser = M.Parsec Void T.Text


t :: IO ()
t = do
    c <- readFile "1.tmp"
    case M.runParser parseMacAddrTableM2 "huy" (T.pack c) of
      Left err -> putStrLn (M.errorBundlePretty err)
      Right ts -> print ts

t2 :: IO ()
t2 = do
    c <- readFile "1.tmp"
    case M.runParser parseMacAddrTableM "huy" (T.pack c) of
      Left err -> putStrLn (M.errorBundlePretty err)
      Right ts -> print ts

tA :: IO ()
tA = do
    c <- readFile "1.tmp"
    case A.parse parseMacAddrTable (T.pack c) of
      A.Fail c xs ys -> do
        print "Huyita"
        print c
        print xs
        print ys
      A.Partial _ -> print "Partial"
      A.Done rs res -> print "Done" >> print res >> print rs

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

sc :: Parser ()
sc = ML.space M.hspace1 empty empty

scn :: Parser ()
scn = ML.space M.space1 empty empty

lexemeM :: Parser a -> Parser a
lexemeM  = ML.lexeme sc

symbol :: M.Tokens T.Text -> Parser (M.Tokens T.Text)
symbol  = ML.symbol sc

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

parseVlan :: Parser Int
parseVlan  = lexemeM $ do
    o <- M.getOffset
    M.region (M.setErrorOffset o) . M.label "vlan number" $ do
      v <- lexemeM ML.decimal
      if v < 4096
        then return v
        else fail "Nihuya sebe vlan"

parseMacAddress :: Parser T.Text
parseMacAddress = lexemeM $ M.takeWhile1P (Just "mac address") ((||) <$> isHexDigit <*> (== '.'))

parsePortNum :: Parser PortNum2
parsePortNum    = lexemeM $ PortNum2
    <$> (symbol "Fa0" *> pure FastEthernet <|> symbol "Gi0" *> pure GigabitEthernet)
    <*> (symbol "/" *> ML.decimal)

columnVlanP :: Parser (PortInfoEl -> Parser PortInfoEl)
columnVlanP = symbol "Vlan" *> pure (\p -> (\x -> p{elVlan = x}) <$> parseVlan)
                M.<?> "column header 'Vlan'"

columnMacAddressP :: Parser (PortInfoEl -> Parser PortInfoEl)
columnMacAddressP = symbol "Mac Address" *> pure (\p -> (\x -> p{elMac = x}) <$> parseMacAddress)
                      M.<?> "column header 'Mac Address'"

columnPortNumP :: Parser (PortInfoEl -> Parser PortInfoEl)
columnPortNumP = symbol "Ports" *> pure (\p -> (\x -> p{elPort = x}) <$> parsePortNum)
                   M.<?> "column header 'Ports'"

columnTypeP :: Parser (PortInfoEl -> Parser PortInfoEl)
columnTypeP = symbol "Type" *> pure (\p -> symbol "DYNAMIC" *> pure p)
                M.<?> "column header 'Type'"

macAddrTableColumns :: [Parser (PortInfoEl -> Parser PortInfoEl)]
macAddrTableColumns =
    [ columnVlanP
    , columnMacAddressP
    , columnTypeP
    , columnPortNumP
    ]

topHeader :: Parser T.Text
topHeader  = M.hspace1
    *> symbol "Mac Address Table" <* M.eol
    <* M.takeWhile1P (Just "top header dashes line") (== '-') <* M.eol
    <* M.skipMany M.space1
    M.<?> "top header"

-- | Parse table header using supplied list of column header parsers. All columns
-- are mandatory, though they can be used in any order. Return a list columns
-- in column header parser results in _actual_ column order.
--
-- This can be used to build a table row parser, if each column header parser
-- return corresponding column cell parser.
parseTableHeader :: [Parser a] -> Parser [a]
parseTableHeader cols =
    let l = length cols
        dashLine = lexemeM $ M.takeWhile1P (Just "column header dash lines for _each_ column") (== '-')
    in  optional topHeader
          *> choiceEachOnce cols <* M.eol
          <* M.count l dashLine <* M.eol

-- | Parse table using supplied column /header/ parsers, which return
-- corresponding row cell parser. Row value is build based on empty row value.
parseTable :: a -> [Parser (a -> Parser a)] -> Parser [a]
parseTable emptyRow cols = do
    cellPs <- parseTableHeader cols
    let rowP = M.hspace *> foldl (>>=) (pure emptyRow) cellPs <* (void M.eol <|> M.eof)
    many rowP

parseMacAddrTableM2 :: Parser [PortInfoEl]
parseMacAddrTableM2 = parseTable defaultPortInfoEl macAddrTableColumns

-- | Another (simpler) mac table parsing variant.
parseMacAddrTableM :: Parser [PortInfoEl]
parseMacAddrTableM = do
    let dashLine = lexemeM $ M.takeWhile1P (Just "column header dash lines for one column") (== '-')
    optional topHeader
    portNumP <- symbol "Vlan" *> symbol "Mac Address"
      *> (  M.try (symbol "Type"  *> symbol "Ports" *> pure (symbol "DYNAMIC" *> parsePortNum))
            <|>  symbol "Ports" *> symbol "Type"  *> pure (parsePortNum <* symbol "DYNAMIC")
         ) <* M.eol
      <* M.count 4 dashLine <* M.eol
    many $ M.hspace
      *> (PortInfoEl <$> parseVlan <*> parseMacAddress <*> portNumP)
      <* (void M.eol <|> M.eof)

-- Below are attoparsec version.
symbolA :: T.Text -> A.Parser T.Text
symbolA   = lexemeA . A.string

lexemeA :: A.Parser a -> A.Parser a
lexemeA p = p <* A.takeWhile A.isHorizontalSpace

parseVlanA :: A.Parser Int
parseVlanA  = lexemeA $ do
      v <- lexemeA A.decimal A.<?> "vlan number"
      if v < 4096
        then return v
        else fail "Nihuya sebe vlan"

-- FIXME: Rewrite with 'count'
parseMacAddressA :: A.Parser T.Text
parseMacAddressA = let isMacChars = (||) <$> isHexDigit <*> (== '.')
                   in  lexemeA (A.takeWhile1 isMacChars) A.<?> "mac address"

parsePortNumA :: A.Parser PortNum2
parsePortNumA    = lexemeA
    $ ( PortNum2
        <$> (symbolA "Fa0" *> pure FastEthernet <|> symbolA "Gi0" *> pure GigabitEthernet)
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
    optional topHeaderA
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
  where
    parseLine :: A.Parser T.Text
    parseLine = A.takeTill A.isEndOfLine

