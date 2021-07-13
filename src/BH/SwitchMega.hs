{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections  #-}

module BH.SwitchMega where

import qualified Data.Text as T
import Data.Char
import Data.Void
import Data.Functor
import Data.Function
import Data.Foldable
import Control.Applicative
import qualified Text.Megaparsec        as M
import qualified Text.Megaparsec.Char   as M
import qualified Text.Megaparsec.Char.Lexer as ML

import BH.Switch

type Parser = M.Parsec Void T.Text

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
    fmap (go <$>) . asum . zipWith (\n mx -> (, n) <$> mx) [(1 :: Int)..] $ ps
  where
    --go :: Alternative m => Int -> [m a]
    go n = foldr (\(i, x) zs -> if n == i then zs else x : zs) [] . zip [1..] $ ps

-- | Apply each choice only once until nothing left.
choiceEachOnce :: (Alternative m, Monad m) => [m a] -> m [a]
choiceEachOnce ps = fix go ps
  where
    go :: (Alternative m, Monad m) => ([m a] -> m [a]) -> [m a] -> m [a]
    go rec xs
      | null xs     = pure []
      | otherwise   = choiceOnce xs >>= \(p, zs) -> (p :) <$> rec zs

parseVlan :: Parser Vlan
parseVlan  = lexemeM $ do
    o <- M.getOffset
    M.region (M.setErrorOffset o) . M.label "vlan number" $ do
      v <- lexemeM ML.decimal
      if v < 4096
        then return (Vlan v)
        else fail "Nihuya sebe vlan"

parseMacAddress :: Parser T.Text
parseMacAddress = lexemeM $ M.takeWhile1P (Just "mac address") ((||) <$> isHexDigit <*> (== '.'))

parsePortNum :: Parser PortNum
parsePortNum    = lexemeM $ PortNum
    <$> (symbol "Fa" *> pure FastEthernet <|> symbol "Gi" *> pure GigabitEthernet)
    <*> ML.decimal
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
    void $ optional topHeader
    portNumP <- symbol "Vlan" *> symbol "Mac Address"
      *> (  M.try (symbol "Type"  *> symbol "Ports" *> pure (symbol "DYNAMIC" *> parsePortNum))
            <|>  symbol "Ports" *> symbol "Type"  *> pure (parsePortNum <* symbol "DYNAMIC")
         ) <* M.eol
      <* M.count 4 dashLine <* M.eol
    many $ M.hspace
      *> (PortInfoEl <$> parseVlan <*> parseMacAddress <*> portNumP)
      <* (void M.eol <|> M.eof)

