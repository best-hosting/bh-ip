{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Control.Monad.Loops
import qualified Data.ByteString.Char8 as B8
import           Network.Simple.TCP
import qualified Network.Telnet.LibTelnet as TL
import qualified Network.Telnet.LibTelnet.Options as TL
import qualified Data.List as L
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map as M
import System.IO.Unsafe
import System.Environment
import Control.Monad.Trans.Cont
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad
import Data.Maybe
import Data.Foldable
import qualified Shelly as Sh
import Control.Concurrent
import qualified Data.Yaml as Y
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding as A
import System.Directory
import Text.HTML.TagSoup

import BH.IP
import BH.Switch


parseShowMacAddrTable :: T.Text -> [SwPort]
parseShowMacAddrTable ts
  | "Mac Address Table" `T.isInfixOf` ts || "Mac Address" `T.isInfixOf` ts
              = foldr go [] (T.lines ts)
  | otherwise = []
  where
    go :: T.Text -> [SwPort] -> [SwPort]
    go t zs = case (T.words t) of
        (_ : _ : _ : p : _) -> either (const zs) (: zs) (parsePort p)
        _               -> zs

parsePort :: T.Text -> Either String SwPort
parsePort t = case reads . drop 1 . dropWhile (/= '/') . T.unpack $ t of
  (n, _) : _ -> Right (SwPort n)
  _          -> Left "Huy"

findPort :: TelnetCmd MacAddr (M.Map SwName [SwPort]) ()
findPort ts0 = do
    mac <- asks telnetIn
    sn  <- asks (swName . switchInfo4)
    let parse ts _ = let xs = parseShowMacAddrTable ts
                     in  if null xs then Partial mempty else Final (pure (M.singleton sn xs)) (last $ T.lines ts)
    pure ts0 >>=
      sendTelnetCmd (CmdText $ "show mac address-table address " <> T.pack (show mac)) >>=
      parseTelnetCmdOut parse >>=
      sendTelnetExit

main :: IO ()
main    = do
    swInfo <- parseSwInfo <$> T.readFile "authinfo.txt"
    print swInfo
    Right mac <- head . map (parseMacAddr . T.pack) <$> getArgs
    print mac
    res <- runExceptT $ do
      mm <- flip runReaderT swInfo $ runTill mac findPort (const True)
      liftIO $ print $ "Found port:" ++ show mm
    case res of
      Right _ -> return ()
      Left err -> print err

