{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

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
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Char
import Control.Applicative (Alternative)
import Data.Function

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
    *> symbol "Mac Address Table"
    <* scn <* takeWhile1P (Just "top header dashes") (== '-') <* skipSome scn
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

data NamedParser m a = NamedParser (String, m a)

instance Functor m => Functor (NamedParser m) where
    fmap f (NamedParser t) = NamedParser (fmap (f <$>) t)
instance Applicative m => Applicative (NamedParser m) where
    pure x = NamedParser ("", pure x)
    NamedParser mf <*> NamedParser mx = NamedParser (fmap (<*>) mf <*> mx)
instance Alternative m => Alternative (NamedParser m) where
    empty = NamedParser (empty, empty)
    --NamedParser (sx, mx) <|> NamedParser (sy, my) = NamedParser 

{-colParsers'1 :: Parser [a -> Parser b] -> [Parser (a -> Parser b)] -> Parser [a -> Parser b]
colParsers'1 rec xs
  | null xs     = pure []
  | otherwise   = do
        m <- choice xs
        rec (filter xs-}

-- | Apply choice until nothing left. Each option may be choosed only _once_.
--
-- For that to work, each option must be accompanied with unique identifier
-- and _the same_ identifier should be returned by option itself.
uniqChoices :: (Eq i, Alternative m, Monad m) => [(i, m (i, b))] -> m [b]
uniqChoices xs = fix go xs
  where
    go :: (Eq i, Alternative m, Monad m) =>
            ([(i, m (i, b))] -> m [b]) -> [(i, m (i, b))] -> m [b]
    go rec ps
      | null ps     = pure []
      | otherwise   = do
            (n, p) <- choice (map snd ps)
            let ys = filter ((/= n) . fst) ps
            (p :) <$> rec (filter ((/= n) . fst) ps)

vV :: Parser (String, T.Text)
vV = symbol "Vlan" *> pure ("Vlan", "Parse vlan")
vM :: Parser (String, T.Text)
vM = symbol "Mac Address" *> pure ("Mac", "Parse mac")
vP :: Parser (String, T.Text)
vP = symbol "Port" *> pure ("Port", "Parse port")

runParserList :: a -> [a -> Parser a] -> Parser a
--runParserList z = foldr (\m z -> z >>= m) (pure z)
runParserList z = foldl (\z m -> z >>= m) (pure z)

fullHeader :: Parser [T.Text]
fullHeader = optional topHeader
    *> count 4 colHeader
    <* takeWhile1P (Just "column header dashes") (`elem` ['-', ' ']) <* scn
    <?> "full table header"

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

parseMacAddress :: Parser T.Text
parseMacAddress = lexeme $ takeWhile1P (Just "mac address") ((||) <$> isHexDigit <*> (== '.'))

parseMacAddress' :: PortInfoEl -> Parser PortInfoEl
parseMacAddress' p = (\x -> p{elMac = x}) <$> parseMacAddress

parsePortNum :: Parser PortNum2
parsePortNum    = lexeme $ PortNum2
    <$> (symbol "Fa0" *> pure FastEthernet <|> symbol "Gi0" *> pure GigabitEthernet)
    <*> (symbol "/" *> L.decimal)

parsePortNum' :: PortInfoEl -> Parser PortInfoEl
parsePortNum' p = (\x -> p{elPort = x}) <$> parsePortNum

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

