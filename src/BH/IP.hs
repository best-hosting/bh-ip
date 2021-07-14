{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards  #-}

module BH.IP
    ( MacAddr (..)
    , parseMacAddr
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
import Numeric
import Text.Read
import Text.Read.Lex
import Text.ParserCombinators.ReadP
import Control.Applicative

import BH.Common


newtype MacAddr     = MacAddr T.Text
  deriving (Eq, Ord)

data MacAddr2       = MacAddr2
                        { macOctet1 :: Int
                        , macOctet2 :: Int
                        , macOctet3 :: Int
                        , macOctet4 :: Int
                        , macOctet5 :: Int
                        , macOctet6 :: Int
                        }
  deriving (Eq, Ord)

instance Show MacAddr2 where
    showsPrec d MacAddr2{..} = showParen (d > 10) $
          showString "MacAddr2 "
        . showHex macOctet1 . showString ":"
        . showHex macOctet2 . showString ":"
        . showHex macOctet3 . showString ":"
        . showHex macOctet4 . showString ":"
        . showHex macOctet5 . showString ":"
        . showHex macOctet6

instance Read MacAddr2 where
    readPrec = parens . prec 10 . lift $ do
        Ident "MacAddr2" <- Text.Read.Lex.lex
        skipSpaces
        MacAddr2
          <$> readHexP <* expect (Symbol ":")
          <*> readHexP <* expect (Symbol ":")
          <*> readHexP <* expect (Symbol ":")
          <*> readHexP <* expect (Symbol ":")
          <*> readHexP <* expect (Symbol ":")
          <*> readHexP

-- FIXME: Rewrite with 'count'
parseMacAddrA :: A.Parser MacAddr
parseMacAddrA = let isMacChars = (||) <$> isHexDigit <*> (== '.')
                in  MacAddr <$> lexemeA (A.takeWhile1 isMacChars) A.<?> "mac address"

    --(++) <$> count 5 (A.hexadecimal <* A.char ':') <*> ((: []) <$> A.hexadecimal)

macOctetP :: A.Parser Int
macOctetP = do
    x <- A.hexadecimal A.<?> "mac octet"
    if x > 255
      then fail "Too great number for mac octet"
      else pure x

macOctetP2 :: A.Parser [Int]
macOctetP2 = do
    x <- A.hexadecimal A.<?> "2-byte mac octet"
    if x > 65535
      then fail "Too great number for 2-byte mac octet"
      else let (o1, o2) = x `divMod` 256 in pure [o1, o2]

mac :: A.Parser MacAddr2
mac = do
    [macOctet1, macOctet2, macOctet3, macOctet4, macOctet5, macOctet6]
        <-                  (:) <$> macOctetP  <*> A.count 5 (A.char ':' *> macOctetP)
            <|> concat <$> ((:) <$> macOctetP2 <*> A.count 2 (A.char '.' *> macOctetP2))
    return MacAddr2{..}

parseMacAddr :: T.Text -> Either String MacAddr
parseMacAddr t = MacAddr <$> T.foldr go end t 1
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

instance Show MacAddr where
    showsPrec d (MacAddr t) = showMac (T.unpack t)
      where
        showMac :: String -> ShowS
        showMac t r = foldr (\(n, x) zs ->
                                if n > 1 && n `mod` 2 == 1 then ':' : x : zs else x : zs)
                            r
                        . zip [1..12]
                        $ t

instance J.ToJSON MacAddr where
    toJSON mac = J.toJSON (show mac)
instance J.ToJSONKey MacAddr where
    toJSONKey = J.ToJSONKeyText (T.pack . show) (J.string . show)

instance J.FromJSON MacAddr where
    parseJSON (J.String t) = either fail return (parseMacAddr t)
instance J.FromJSONKey MacAddr where
    fromJSONKey = J.FromJSONKeyTextParser (either fail return . parseMacAddr)

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
