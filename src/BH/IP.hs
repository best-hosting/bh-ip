{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards  #-}

module BH.IP
    ( MacAddr (..)
    , macP
    --, parseMacAddr
    , IP (..)
    , parseIP
    , Vlan (..)
    )
  where

import Data.Char
import qualified Data.Text as T
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as J
import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Combinator as A
import Numeric
import Text.Read
import Text.Read.Lex
import Text.ParserCombinators.ReadP
import Control.Applicative

import BH.Common


data MacAddr    = MacAddr
                    { macOctet1 :: Int
                    , macOctet2 :: Int
                    , macOctet3 :: Int
                    , macOctet4 :: Int
                    , macOctet5 :: Int
                    , macOctet6 :: Int
                    }
  deriving (Eq, Ord)

instance Show MacAddr where
    showsPrec d MacAddr{..} = showParen (d > 10) $
          showString "MacAddr "
        . showOctet macOctet1 . showString ":"
        . showOctet macOctet2 . showString ":"
        . showOctet macOctet3 . showString ":"
        . showOctet macOctet4 . showString ":"
        . showOctet macOctet5 . showString ":"
        . showOctet macOctet6
      where
        showOctet :: Int -> ShowS
        showOctet d = showHex (d `div` 16) . showHex (d `mod` 16)

instance Read MacAddr where
    readPrec = parens . prec 10 . lift $ do
        Ident "MacAddr" <- Text.Read.Lex.lex
        skipSpaces
        MacAddr
          <$> readHexP <* expect (Symbol ":")
          <*> readHexP <* expect (Symbol ":")
          <*> readHexP <* expect (Symbol ":")
          <*> readHexP <* expect (Symbol ":")
          <*> readHexP <* expect (Symbol ":")
          <*> readHexP

instance J.ToJSON MacAddr where
    toJSON mac = J.toJSON (show mac)
instance J.ToJSONKey MacAddr where
    toJSONKey = J.ToJSONKeyText (T.pack . show) (J.string . show)

instance J.FromJSON MacAddr where
    parseJSON (J.String t) = either fail return (A.parseOnly macP t)
instance J.FromJSONKey MacAddr where
    fromJSONKey = J.FromJSONKeyTextParser (either fail return . A.parseOnly macP)

-- | Parse single mac octet.
macOctetP1 :: A.Parser Int
macOctetP1 = do
    x <- A.hexadecimal A.<?> "mac octet"
    if x > 255
      then fail "Too great number for mac octet"
      else pure x

-- | Parse two mac octets written as single hexadecimal number.
macOctetP2 :: A.Parser [Int]
macOctetP2 = do
    x <- A.hexadecimal A.<?> "2-byte mac octet"
    if x > 65535
      then fail "Too great number for 2-byte mac octet"
      else let (o1, o2) = x `divMod` 256 in pure [o1, o2]

-- | Parser for mac address written as "11:22:33:44:55:66".
macP1 :: A.Parser [Int]
macP1 =
    (:) <$> macOctetP1
        <*> (A.count 5 (A.char ':' *> macOctetP1) A.<?> "Too few octets for mac")

-- | Parser for mac address written as "1122.3344.5566".
macP2 :: A.Parser [Int]
macP2 = fmap concat $
    (:) <$> macOctetP2
        <*> (A.count 2 (A.char '.' *> macOctetP2) A.<?> "Too few octets for 2-byte mac")

-- | Parser for mac addresses.
macP :: A.Parser MacAddr
macP = do
    -- 'lookAhead' allows to choose parsing branch first and then fail entire
    -- parser, if choosed branch fails. If on the other hand <|> would be
    -- applied to branches itself, like 'macP1 <|> macP2', then e.g.
    -- erroneous mac address ("x:1:2:3:4:5") conforming to 1st branch 'macP1'
    -- will fail in 2nd branch with misleading error message.
    p <-    A.lookAhead (A.takeWhile1 (`notElem` [':', '.']) <* A.char ':') *> return macP1
        <|> A.lookAhead (A.takeWhile1 (`notElem` [':', '.']) <* A.char '.') *> return macP2
    [macOctet1, macOctet2, macOctet3, macOctet4, macOctet5, macOctet6]
      <- p
    return MacAddr{..}

-- | Old text-based mac address implementation.
newtype MacAddr2     = MacAddr2 T.Text
  deriving (Eq, Ord)

parseMacAddr2 :: T.Text -> Either String MacAddr2
parseMacAddr2 t = MacAddr2 <$> T.foldr go end t 1
  where
    go :: Char -> (Int -> Either String T.Text) -> Int -> Either String T.Text
    go c zf n
      | n > 12              = Left "Too many chars for mac address."
      | isHexDigit c        = let mz = zf (n + 1) in (T.cons . toLower $ c) <$> mz
      | c `elem` [':', '.'] = zf n
      | otherwise           = Left "Not a mac address."
    end :: Int -> Either String T.Text
    end n
      | n /= 13             = Left "Too few chars for mac address."
      | otherwise           = Right T.empty

instance Show MacAddr2 where
    showsPrec d (MacAddr2 t) = showMac (T.unpack t)
      where
        showMac :: String -> ShowS
        showMac t r = foldr (\(n, x) zs ->
                                if n > 1 && n `mod` 2 == 1 then ':' : x : zs else x : zs)
                            r
                        . zip [1..12]
                        $ t

instance J.ToJSON MacAddr2 where
    toJSON mac = J.toJSON (show mac)
instance J.ToJSONKey MacAddr2 where
    toJSONKey = J.ToJSONKeyText (T.pack . show) (J.string . show)

instance J.FromJSON MacAddr2 where
    parseJSON (J.String t) = either fail return (parseMacAddr2 t)
instance J.FromJSONKey MacAddr2 where
    fromJSONKey = J.FromJSONKeyTextParser (either fail return . parseMacAddr2)


newtype IP          = IP (Int, Int, Int, Int)
  deriving (Eq)

instance Show IP where
    showsPrec d ip = (showIP ip ++)
      where
        showIP :: IP -> String
        showIP (IP (o1, o2, o3, o4)) =
            show o1 ++ "." ++ show o2 ++ "." ++ show o3 ++ "." ++ show o4

instance J.ToJSON IP where
    toJSON ip   = J.toJSON (show ip)

instance J.FromJSON IP where
    parseJSON (J.String t) = either fail return (parseIP t)

parseIP :: T.Text -> Either String IP
parseIP t = do
    os <- mapM parseOctet . T.split (== '.') $ t
    case os of
      [o1, o2, o3, o4]  -> Right (IP (o1, o2, o3, o4))
      _                 -> Left "Too few or too many octets."
  where
    parseOctet :: T.Text -> Either String Int
    parseOctet ds = case reads (T.unpack ds) of
        [(d, "")]
          | 0 <= d && d <= 255  -> Right d
          | otherwise           -> Left $ "Incorrect IP octet '" ++ show d ++ "'"
        []                      -> Left "Can't read IP octet."

newtype Vlan = Vlan Int
  deriving (Show)
