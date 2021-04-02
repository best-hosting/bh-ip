{-# LANGUAGE OverloadedStrings #-}

module BH.IP
    ( MacAddr (..)
    , parseMacAddr
    , IP (..)
    , parseIP
    )
  where

import Data.Char
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding as A


newtype MacAddr     = MacAddr T.Text
  deriving (Eq, Ord)

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

instance A.ToJSON MacAddr where
    toJSON mac = A.toJSON (show mac)
instance A.ToJSONKey MacAddr where
    toJSONKey = A.ToJSONKeyText (T.pack . show) (A.string . show)

instance A.FromJSON MacAddr where
    parseJSON (A.String t) = either fail return (parseMacAddr t)
instance A.FromJSONKey MacAddr where
    fromJSONKey = A.FromJSONKeyTextParser (either fail return . parseMacAddr)

newtype IP          = IP (Int, Int, Int, Int)
  deriving (Eq)

instance Show IP where
    showsPrec d ip = (showIP ip ++)
      where
        showIP :: IP -> String
        showIP (IP (o1, o2, o3, o4)) =
            show o1 ++ "." ++ show o2 ++ "." ++ show o3 ++ "." ++ show o4

instance A.ToJSON IP where
    toJSON ip   = A.toJSON (show ip)

instance A.FromJSON IP where
    parseJSON (A.String t) = either fail return (parseIP t)

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

