{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module BH.IP
    ( MacAddr (..)
    , defMacAddr
    , macP
    , showMacAddr
    , MacInfo(..)
    , IP (..)
    , defIP
    , showIP
    , ipP
    , IPInfo (..)
    , Vlan (..)
    , vlanP
    )
  where

import Data.Char
import qualified Data.Text as T
import Data.Aeson
import qualified Data.Aeson.Encoding as JE
import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Combinator as A
import Numeric
import Text.Read
import Text.Read.Lex
import Text.ParserCombinators.ReadP
import Control.Applicative
import Control.DeepSeq
import GHC.Generics (Generic)
import qualified Data.Set as S

import BH.Common


-- TODO: Use 'ip' package instead of this module.
-- TODO: May be add 'vlan' field to 'IP' ? And 'vendor' field to 'MacAddr' type ?
-- May it's better leave 'MacAddr' and 'IP' as "pure" addresses and create
-- wrapper type containing more info, like MacInfo{ macVlan :: Vlan, macAddr
-- :: MacAddr} and IPInfo {ipVlan :: Vlan, ipAddr ;: IP} .

data MacAddr    = MacAddr
                    { macOctet1 :: Int
                    , macOctet2 :: Int
                    , macOctet3 :: Int
                    , macOctet4 :: Int
                    , macOctet5 :: Int
                    , macOctet6 :: Int
                    }
  deriving (Eq, Ord, Generic, NFData)

defMacAddr :: MacAddr
defMacAddr  = MacAddr
                    { macOctet1 = 0
                    , macOctet2 = 0
                    , macOctet3 = 0
                    , macOctet4 = 0
                    , macOctet5 = 0
                    , macOctet6 = 0
                    }

showsMacAddr :: MacAddr -> ShowS
showsMacAddr MacAddr{..} =
          showOctet macOctet1 . showString ":"
        . showOctet macOctet2 . showString ":"
        . showOctet macOctet3 . showString ":"
        . showOctet macOctet4 . showString ":"
        . showOctet macOctet5 . showString ":"
        . showOctet macOctet6
      where
        showOctet :: Int -> ShowS
        showOctet d = showHex (d `div` 16) . showHex (d `mod` 16)

-- FIXME: Change result to 'T.Text' to be more uniform across the lib.
showMacAddr :: MacAddr -> String
showMacAddr m = showsMacAddr m ""

instance Show MacAddr where
    showsPrec d m = showParen (d > 10) $ showString "MacAddr " . showsMacAddr m

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

instance ToJSON MacAddr where
    toJSON mac = toJSON (showMacAddr mac)
instance ToJSONKey MacAddr where
    toJSONKey = ToJSONKeyText (T.pack . showMacAddr) (JE.string . showMacAddr)

instance FromJSON MacAddr where
    parseJSON (Data.Aeson.String t) = either fail return (A.parseOnly macP t)
instance FromJSONKey MacAddr where
    fromJSONKey = FromJSONKeyTextParser (either fail return . A.parseOnly macP)

-- | Parse single mac octet.
macOctetP1 :: A.Parser Int
macOctetP1 = do
    x <- A.hexadecimal A.<?> "mac octet"
    if x > 256
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
-- Note, to print mac address in parser-compatible form use 'showMacAddr'.
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

-- FIXME: Humans works with IPs, not with mac addresses. So, it's more natural
-- to have IP-mac relation, than mac-[IP] .
data MacInfo = MacInfo
  { macAddr :: MacAddr
  , macVlan :: Vlan
  --, macVendor :: T.Text
  , macIPs :: S.Set IP
  }
  deriving (Show)

instance ToJSON MacInfo where
  toJSON MacInfo {..} =
    object $
      [ "mac"  .= macAddr
      , "vlan" .= macVlan
      , "ips"  .= macIPs
      ]

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

instance ToJSON MacAddr2 where
    toJSON mac = toJSON (show mac)
instance ToJSONKey MacAddr2 where
    toJSONKey = ToJSONKeyText (T.pack . show) (JE.string . show)

instance FromJSON MacAddr2 where
    parseJSON (Data.Aeson.String t) = either fail return (parseMacAddr2 t)
instance FromJSONKey MacAddr2 where
    fromJSONKey = FromJSONKeyTextParser (either fail return . parseMacAddr2)


data IP = IP
            { ipOctet1 :: Int
            , ipOctet2 :: Int
            , ipOctet3 :: Int
            , ipOctet4 :: Int
            }
  deriving (Eq, Ord, Generic, NFData)

defIP :: IP
defIP   = IP
            { ipOctet1 = 0
            , ipOctet2 = 0
            , ipOctet3 = 0
            , ipOctet4 = 0
            }

showsIP :: IP -> ShowS
showsIP IP{..} =
          shows ipOctet1 . showString "."
        . shows ipOctet2 . showString "."
        . shows ipOctet3 . showString "."
        . shows ipOctet4

showIP :: IP -> String
showIP m = showsIP m ""

instance Show IP where
    showsPrec d m = showParen (d > 10) $ showString "IP " . showsIP m

instance Read IP where
    readPrec = parens . prec 10 . lift $ do
        Ident "IP" <- Text.Read.Lex.lex
        skipSpaces
        IP <$> readDecP <* expect (Symbol ".")
           <*> readDecP <* expect (Symbol ".")
           <*> readDecP <* expect (Symbol ".")
           <*> readDecP

instance ToJSON IP where
    toJSON ip   = toJSON (showIP ip)
instance ToJSONKey IP where
  toJSONKey = ToJSONKeyText (T.pack . showIP) (JE.string . showIP)

instance FromJSON IP where
    parseJSON (Data.Aeson.String t) = either fail return (A.parseOnly ipP t)
instance FromJSONKey IP where
  fromJSONKey = FromJSONKeyTextParser (either fail return . A.parseOnly ipP)

-- | Parser for IP address octet.
ipOctetP :: A.Parser Int
ipOctetP = do
    x <- A.decimal A.<?> "ip octet"
    if x > 256
      then fail "Too great number for ip octet"
      else pure x

-- | Parser for IP address.
-- Note, to print IP address in parser-compatible form use 'showIP'.
ipP :: A.Parser IP
ipP = do
    [ipOctet1, ipOctet2, ipOctet3, ipOctet4]
      <- (:) <$> ipOctetP <*> A.count 3 (A.char '.' *> ipOctetP)
    return IP{..}

-- | Plain (no parsec) parser for IP address.
parseIP :: T.Text -> Either String IP
parseIP t = do
    os <- mapM parseOctet . T.split (== '.') $ t
    case os of
      [ipOctet1, ipOctet2, ipOctet3, ipOctet4]  -> Right (IP {..})
      _                 -> Left "Too few or too many octets."
  where
    parseOctet :: T.Text -> Either String Int
    parseOctet ds = case reads (T.unpack ds) of
        [(d, "")]
          | 0 <= d && d <= 255  -> Right d
          | otherwise           -> Left $ "Incorrect IP2 octet '" ++ show d ++ "'"
        []                      -> Left "Can't read IP2 octet."

data IPInfo = IPInfo
  { ipAddr :: IP
  , ipMacAddr :: MacAddr
  , ipVlan :: Vlan
  , ipSubnet :: T.Text
  }
 deriving (Show)

newtype Vlan = Vlan Int
  deriving (Show)

instance ToJSON Vlan where
  toJSON (Vlan x) = toJSON x

-- | Parser for vlan number.
vlanP :: A.Parser Vlan
vlanP = do
    v <- A.decimal A.<?> "vlan number"
    if v < 4096
      then return (Vlan v)
      else fail "Nihuya sebe vlan"

