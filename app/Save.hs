{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import System.Environment
import Control.Monad.Reader
import Control.Monad.Trans.Except

import BH.Switch
import BH.Telnet


saveSwitch :: TelnetCmd () T.Text ()
saveSwitch t0 =
    sendCmd (defCmd "write") t0 >>=
    sendCmd (defCmd "terminal length 0") >>=
    sendAndParse parseCf (defCmd "show running") >>=
    sendExit

-- | Parse cisco config. It uses "\r\n" line-ending. And config ends at 'end'
-- string. The part of input after end string is returned as unparsed text.
parseCf :: TelnetParser T.Text
parseCf ts z    = let endCf  = "\nend\r\n"
                  in  case T.splitOn endCf ts of
                        [_]     -> Partial  { parserResult = z <> pure ts}
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
