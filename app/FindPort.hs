{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.IO.Class
import qualified Data.Map as M
import System.Environment
import Control.Monad.Reader
import Control.Monad.Trans.Except

import BH.IP
import BH.Switch
import BH.Telnet


parseShowMacAddrTable :: T.Text -> [PortNum]
parseShowMacAddrTable ts
  | "Mac Address Table" `T.isInfixOf` ts || "Mac Address" `T.isInfixOf` ts
              = foldr go [] (T.lines ts)
  | otherwise = []
  where
    go :: T.Text -> [PortNum] -> [PortNum]
    go t zs = case T.words t of
        (_ : _ : _ : p : _) -> either (const zs) (: zs) (parsePort p)
        _               -> zs
    parsePort :: T.Text -> Either String PortNum
    parsePort t = case reads . drop 1 . dropWhile (/= '/') . T.unpack $ t of
      (n, _) : _ -> Right (PortNum n)
      _          -> Left "Huy"

findPort :: TelnetCmd MacAddr (M.Map SwName [PortNum]) ()
findPort t0 = do
    mac <- asks telnetIn
    sn  <- asks (swName . switchInfo)
    -- FIXME: Continue, if no mac was found. Current version will hang up due
    -- to 'Partial' result.
    -- I may parse 'Fa0/9' as complete type, like PortNum. But 'sw-1/9' parse
    -- as SwName, then lookup default port spec and parse '9' as 'PortNum'
    -- using default port spec.
    let parse ts _ = let xs = parseShowMacAddrTable ts
                     in  if null xs
                            then Partial mempty
                            else Final (pure (M.singleton sn xs)) (last $ T.lines ts)
    sendAndParse parse (defCmd $ "show mac address-table address " <> T.pack (show mac)) t0 >>=
      sendExit

findPortA :: TelnetCmd MacAddr (M.Map SwName [PortNum]) ()
findPortA t0 = do
    mac <- asks telnetIn
    sn  <- asks (swName . switchInfo)
    -- FIXME: Continue, if no mac was found. Current version will hang up due
    -- to 'Partial' result.
    -- I may parse 'Fa0/9' as complete type, like PortNum. But 'sw-1/9' parse
    -- as SwName, then lookup default port spec and parse '9' as 'PortNum'
    -- using default port spec.
    let parse mz = (fromMaybe [] mz :) parseMacAddrTable
    let parse ts _ = let xs = A.parseOnly ts
                     in  if null xs
                            then Partial mempty
                            else Final (pure (M.singleton sn xs)) (last $ T.lines ts)
    sendAndParseA parseMacAddrTable (defCmd $ "show mac address-table address " <> T.pack (show mac)) t0 >>= sendExit
    st    <- liftIO (readIORef stRef)
    let res = maybe (A.parse p) A.feed (getL l st) ts
    liftIO $ atomicModifyIORef stRef (\s -> (setL l (Just res) s, ()))
    case res of
      A.Partial _ -> liftIO $ print "Partial result.."
      A.Fail i xs err -> error $ "Naebnulos vse: " <> T.unpack i <> concat xs <> err
      A.Done unparsedTxt _ -> do
        liftIO $ print $ "Finished output parsing"
        liftIO $ print $ "Unparsed text left: '" <> unparsedTxt <> "'"
        saveResume k
        lift (k unparsedTxt)

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

