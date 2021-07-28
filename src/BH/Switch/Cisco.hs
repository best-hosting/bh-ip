{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module BH.Switch.Cisco (
  module BH.Switch,
  readSwInfo,
  parseMacAddrTable,
  parseCiscoConfig,
  getMacs,
  queryPorts,
  queryPort,
) where

import qualified Data.Map as M
import qualified Data.Text as T

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Attoparsec.Combinator as A
import qualified Data.Attoparsec.Text as A
import Data.Either.Combinators
import qualified Data.Set as S
import qualified Data.Yaml as Y
import System.Directory

import BH.Common
import BH.IP
import BH.IP.Arp
import BH.Main
import BH.Switch
import BH.Telnet

-- FIXME: Use generic yaml reading func.
readSwInfo :: (MonadIO m, MonadError String m) => FilePath -> m SwInfoMap
readSwInfo file = do
  b <- liftIO (doesFileExist file)
  if b
    then liftIO (Y.decodeFileEither file) >>= liftEither . mapLeft show >>= return . toSwInfoMap
    else throwError ("File with switch info not found " <> file)
 where
  toSwInfoMap :: [SwInfo] -> SwInfoMap
  toSwInfoMap = M.fromList . map (\x -> (swName x, x))

-- | Dashes underlining header of _one_ column. Trailing spaces are consumed,
-- but newline does _not_ .
dashLineA :: A.Parser T.Text
dashLineA =
  A.takeWhile1 (== '-') <* A.takeWhile A.isHorizontalSpace
    A.<?> "column header dash lines for one column"

topHeaderA :: A.Parser T.Text
topHeaderA =
  A.takeWhile1 A.isHorizontalSpace
    *> A.string "Mac Address Table"
    <* A.endOfLine
    <* dashLineA
    <* A.endOfLine
    <* A.skipSpace
    A.<?> "top header"

parseMacAddrTable :: A.Parser [PortInfoEl]
parseMacAddrTable = do
  void $ optional topHeaderA
  portP <-
    symbolA "Vlan" *> symbolA "Mac Address"
      *> ( symbolA "Type" *> symbolA "Ports" *> pure (symbolA "DYNAMIC" *> lexemeA portNumP)
            <|> symbolA "Ports" *> symbolA "Type" *> pure (lexemeA portNumP <* symbolA "DYNAMIC")
         )
      <* A.endOfLine
      <* A.count 4 dashLineA
      <* A.endOfLine
  many $
    A.takeWhile A.isHorizontalSpace
      *> (PortInfoEl <$> lexemeA vlanP <*> lexemeA macP <*> portP)
      -- 'endOfInput' should never match here, because mac table must be
      -- always followed by telnet prompt and 'many' parser should not match
      -- with prompt. On the other hand, matching (or using 'lookAhead') with
      -- prompt explicitly here has little sense either, because prompt will
      -- always be on the new line and i may just terminate match at
      -- 'endOfLine'.
      <* (void A.endOfLine <|> A.endOfInput)

parseCiscoConfig :: A.Parser T.Text
parseCiscoConfig =
  A.takeTill A.isEndOfLine
    <<>> ( A.string "\r\nend\r\n"
            <|> A.takeWhile1 A.isEndOfLine <<>> parseCiscoConfig
         )

-- I'm intresting in receiving all ports in question as input, because then i
-- may connect to each switch only once and iterate over all asked ports from
-- this switch.
getMacs :: TelnetCmd [SwPort] (M.Map SwPort [MacAddr]) ()
getMacs ts = do
  curSn <- asks (swName . switchInfo)
  ports <- asks (filter ((== curSn) . portSw) . telnetIn)
  foldM (flip go) ts ports >>= sendExit
 where
  go :: SwPort -> TelnetCmd [SwPort] (M.Map SwPort [MacAddr]) T.Text
  go swPort@SwPort{..} =
    sendAndParse
      (parse <$> parseMacAddrTable)
      (cmd $ "show mac address-table interface " <> ciscoPortNum portSpec)
   where
    parse :: [PortInfoEl] -> M.Map SwPort [MacAddr]
    parse xs =
      if null xs
        then mempty
        else M.singleton swPort (map elMac xs)

-- | Query several ports.
queryPorts ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  S.Set SwPort ->
  m (M.Map SwPort [(MacAddr, S.Set IP)])
queryPorts switches = do
  Config{..} <- ask
  portMacs <- flip runReaderT swInfoMap
    $ run (S.toList switches) getMacs (head . S.toList $ S.map portSw switches)
  liftIO $ putStrLn "Gathered ac map:"
  liftIO $ print portMacs
  liftIO $ putStrLn "Finally, ips..."
  forM portMacs $ mapM (\m -> (m,) <$> macToIPs m)

-- | Query single port.
queryPort ::
  (MonadReader Config m, MonadError String m, MonadIO m) =>
  SwPort ->
  m (M.Map SwPort [(MacAddr, S.Set IP)])
queryPort = queryPorts . S.singleton
