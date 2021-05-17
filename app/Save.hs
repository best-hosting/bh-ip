{-# LANGUAGE OverloadedStrings #-}

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

import BH.Switch


saveSwitch :: TelnetCmd () T.Text ()
saveSwitch ts0 = pure ts0 >>=
    sendCmd (defCmd "write") >>=
    sendCmd (defCmd "terminal length 0") >>=
    sendAndParse parseCf (defCmd "show running") >>=
    sendExit

-- | Parse cisco config. It uses "\r\n" line-ending. And config ends at 'end'
-- string. The part of input after end string is returned as unparsed text.
parseCf :: TelnetParser T.Text
parseCf ts z    = let endCf  = "\nend\r\n"
                  in  case T.splitOn endCf ts of
                        [c]     -> Partial  { parserResult = z <> pure ts}
                        [c, y]  -> Final    { parserResult = z <> pure (c <> endCf)
                                            , unparsedText_ = y
                                            }
                        _ -> error "Impossible.."

main :: IO ()
main    = do
    swInfo <- parseSwInfo <$> T.readFile "authinfo.txt"
    print swInfo
    sns <- parseArgs <$> getArgs
    print sns
    res <- runExceptT . flip runReaderT swInfo $
      if null sns
        then runOn () saveSwitch (M.keys swInfo)
        else runOn () saveSwitch sns
    case res of
      Right m -> do
        forM_ (M.toList m) $ \(SwName s, cf) -> do
          print $ "Writing config for " ++ T.unpack s
          writeFile (T.unpack s ++ ".cf") (T.unpack cf)
      Left err -> print err

parseArgs :: [String] -> [SwName]
parseArgs = map (SwName . T.pack)
