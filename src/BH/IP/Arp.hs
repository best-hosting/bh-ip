{-# LANGUAGE OverloadedStrings #-}

module BH.IP.Arp
    ( queryLinuxArp
    )
  where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.IO.Class
import qualified Data.Map as M
import System.Environment
import Control.Monad.Trans.Cont
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.Maybe
import qualified Shelly as Sh
import Control.Concurrent
import qualified Data.Yaml as Y
import System.Directory
import Text.HTML.TagSoup
import qualified Data.Attoparsec.Text as A
import Control.Monad (join)
import Control.Applicative

import BH.Common
import BH.IP
import BH.Switch


queryMikrotikArp :: T.Text -> IO MacIpMap
queryMikrotikArp host   = Sh.shelly . Sh.silently $
    Sh.runFoldLines M.empty (\zs -> go zs . T.words) "ssh" [host, "/ip", "arp", "print"]
  where
    go :: MacIpMap -> [T.Text] -> MacIpMap
    go zs (_ : _ : x : y : _) =
        either (const zs) (\(w, t) -> uncurry (M.insertWith addIp) (w, t) zs) $ do
          ip <- A.parseOnly ipP x
          ma <- A.parseOnly macP y
          return (ma, [ip])
    go zs _                   = zs

-- Use 'ip neigh'.
queryLinuxArp :: FilePath   -- ^ Path to yaml cache.
              -> T.Text     -- ^ ssh hostname of host, from which to query.
              -> ExceptT String IO MacIpMap
queryLinuxArp file host = do
    b <- liftIO $ doesFileExist file
    if b
      then catchE (ExceptT $ Y.decodeFileEither file) $ \e -> do
        liftIO (print e)
        updateArpCache
      else updateArpCache
  where
    updateArpCache :: ExceptT String IO MacIpMap
    updateArpCache  = do
        --mi <- nmapCache
        mi <- ipNeighCache host
        liftIO $ Y.encodeFile file mi
        return mi

ipNeighCache :: T.Text -> ExceptT String IO MacIpMap
ipNeighCache host = ExceptT . Sh.shelly . Sh.silently $ do
    liftIO $ putStrLn "Updating arp cache using `nping` and `ip neighbour`..."
    Sh.run_ "ssh"
            (host : T.words "nping --quiet -N --rate=100 -c1 213.108.248.0/21")
    liftIO $ threadDelay 5000000
    mi <- Sh.runFoldLines (return M.empty) (\zs -> go zs . T.words) "ssh"
            (host : T.words "ip neighbour show nud reachable nud stale")
    Sh.run_ "ssh" (host : T.words "ip neighbour flush all")
    return mi
  where
    go :: Either String MacIpMap -> [T.Text] -> Either String MacIpMap
    go mzs (x : _ : _ : _ : y : s : _)
      | s == "REACHABLE" || s == "STALE" = do
        zs <- mzs
        ip <- A.parseOnly ipP x
        ma <- A.parseOnly macP y
        return (M.insertWith addIp ma [ip] zs)
      | otherwise   = mzs
    go _ _          = Left "Unrecognized `ip neigh` output line."

go2 :: A.Parser (IP, MacAddr)
go2 = (,)
    <$> lexemeA ipP  <*  A.count 3 (A.takeWhile1 (/= ' ') *> A.string " ")
    <*> lexemeA macP <*  A.takeWhile1 (not . A.isEndOfLine)
    A.<?> "ip neigh output line"

nmapCache :: T.Text -> ExceptT String IO MacIpMap
nmapCache host = do
    liftIO $ putStrLn "Updating arp cache using `nmap`..."
    nxml <- liftIO . Sh.shelly . Sh.silently $ do
      Sh.run_ "ssh" (host : T.words "nmap -sn -PR -oX nmap_arp_cache.xml 213.108.248.0/21")
      Sh.run  "ssh" (host : T.words "cat ./nmap_arp_cache.xml")
    let emi = mapM go
                . sections (~== ("<status state=\"up\" reason=\"arp-response\">" :: String))
                . parseTags
                $ nxml
    M.fromListWith addIp <$> ExceptT (return emi)
  where
    go :: [Tag T.Text] -> Either String (MacAddr, [IP])
    go xs =
        let oneHost = filter (~== ("<address>" :: String)) . takeWhile (~/= ("</host>" :: String))
        in  case oneHost xs of
              [x, y]  -> do
                          ip  <- A.parseOnly ipP (fromAttrib "addr" x)
                          mac <- A.parseOnly macP (fromAttrib "addr" y)
                          return (mac, [ip])
              _       -> Left "Unrecognized ip, mac pair in nmap output."

{-

{-parseMikrotikArp :: [T.Text -> MacIpMap
parseMikrotikArp zs t = case T.words t of
    (_ : _ : x : y : _) ->
      either (const zs) (\(w, y) -> uncurry (M.insertWith addIp) (w, y) zs) $ do
        ip <- A.parseOnly ipP x
        ma <- parseMacAddr y
        return (ma, [ip])
    _                   -> zs-}

parseMikrotikArp :: T.Text -> Either String (MacAddr, [IP])
parseMikrotikArp    = go . T.words
  where
    go :: [T.Text] -> Either String (MacAddr, [IP])
    go (_ : _ : x : y : _) = do
      ip <- A.parseOnly ipP x
      ma <- parseMacAddr y
      return (ma, [ip])
    go _ = Left "Huy"

parseLinuxArp :: T.Text -> Either String (MacAddr, [IP])
parseLinuxArp   = go . T.words
  where
    go :: [T.Text] -> Either String (MacAddr, [IP])
    go (_ : x : _ : y : _) = do
      ip <- A.parseOnly ipP x
      ma <- parseMacAddr y
      return (ma, [ip])
    go _ = Left "Huy"


parseLinuxArp :: [T.Text] -> Either String (MacAddr, [IP])
parseLinuxArp (_ : x : _ : y : _) = do
      ip <- A.parseOnly ipP x
      ma <- parseMacAddr y
      return (ma, [ip])
parseLinuxArp _ = Left "Huy"

{-parseLinuxArp :: MacIpMap -> T.Text -> MacIpMap
parseLinuxArp zs t = case T.words t of
    (_ : x : _ : y : _) -> either (const zs) (\(w, y) -> uncurry (M.insertWith addIp) (w, y) zs) $ do
      ip <- A.parseOnly ipP x
      ma <- parseMacAddr y
      return (ma, [ip])
    _                   -> zs-}-}

addIp :: [IP] -> [IP] -> [IP]
addIp xs zs0 = foldr (\x zs -> if x `elem` zs then zs else x : zs) zs0 xs

