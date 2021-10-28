{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BH.Main (
  module BH.Main.Types,
  readSwInfo,
  searchPorts2,
  searchMacs2,
  searchIPs2,
)
where

import Data.Aeson
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe
import Data.List
import qualified Data.Set as S
import Control.Monad.State
import Data.Monoid

import BH.Main.Types
import BH.Common
import BH.IP
import BH.Switch
import BH.Switch.Cisco
import BH.Cache
import BH.Telnet
import BH.IP.Arp

-- FIXME: Use generic yaml reading func.
readSwInfo :: (MonadIO m, MonadError String m) => FilePath -> m SwInfo
readSwInfo file = toSwInfo <$> readYaml file
 where
  toSwInfo :: [SwData] -> SwInfo
  toSwInfo = M.fromList . map (\x -> (swName x, x))

-- | This is not really "search", but just quering info about ports. It names
-- this way just to make show its relevant to 'searchX' family of functions.
searchPorts2 ::
  (MonadReader Config m, MonadError String m, MonadIO m, MonadState (IPInfo, MacInfo, SwPortInfo) m) =>
  [SwPort] -> m ()
searchPorts2 swPorts = do
  Config{..} <- ask
  f <- mergePorts <$> runReaderT (runOn onPorts swNames go) swInfo
  modify f
 where
  -- FIXME: Change input to (S.Set PortNum). But for this to have any sense, i
  -- need to change it everywhere, includeing findX funcs.. [refactor]
  onPorts :: SwName -> [PortNum]
  onPorts sn = nub . map portSpec . filter ((== sn) . portSw) $ swPorts
  swNames :: [SwName]
  swNames = nub . map portSw $ swPorts
  go :: TelnetRunM TelnetParserResult [PortNum] (M.Map SwPort (PortState, S.Set MacAddr)) ()
  go = do
    portSw <- asks (swName . switchData)
    ps <- asks telnetIn
    res <- M.mapKeys (\p -> SwPort{portSpec = p, ..}) <$> findPortInfo2 ps
    putResult res
    sendExit

-- TODO: I may use hash to determine changed db file. And then treat /that/
-- file as source and generate others from it.
-- FIXME: I should obtain full info. Here or not, but query over mac implies
-- full result, including both port and IP. And if some part is missing, i
-- should query it as well. [current]
-- | Search mac on all switches.
searchMacs2 ::
  (MonadReader Config m, MonadError String m, MonadIO m, MonadState (IPInfo, MacInfo, SwPortInfo) m) =>
  [MacAddr] ->
  m ()
searchMacs2 macs = do
  Config{..} <- ask
  f <- mergeMacs <$> runReaderT (runTill maybeMacs go) swInfo
  modify f
 where
  -- FIXME: Hardcoded vlan! [current]
  maybeMacs :: M.Map MacAddr (SwPort, PortState) -> Maybe [MacAddr]
  maybeMacs res = let found = M.keys res
                  in  case filter (`notElem` found) macs of
                        [] -> Nothing
                        xs -> Just xs
  go :: TelnetRunM TelnetParserResult [MacAddr] (M.Map MacAddr (SwPort, PortState)) ()
  go = do
    portSw <- asks (swName . switchData)
    ms  <- asks telnetIn
    M.map (\p -> (SwPort{portSpec = p, ..}, Up)) <$> (findMacInfo2 ms) >>= putResult
    sendExit

searchIPs2 ::
  (MonadReader Config m, MonadError String m, MonadIO m, MonadState (IPInfo, MacInfo, SwPortInfo) m) =>
  [IP] ->
  m ()
searchIPs2 ips = do
  Config{..} <- ask
  f <- mergeIP2 <$> nmapCache3 ips nmapHost
  modify f

-- FIXME: Make a newtype wrapper around (IPInfo, MacIpMap, SwPortInfo) ?
-- And then add function for obtaining 'InfoKey' indexed map from a generic db
-- type [current]
