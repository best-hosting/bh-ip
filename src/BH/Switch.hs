{-# LANGUAGE OverloadedStrings #-}

module BH.Switch
    ( SwName (..)
    , SwInfo (..)
    , parseSwInfo
    , PortNum (..)
    , SwPort (..)
    , PortMacMap
    , MacPortMap
    , SwConfig
    , MacIpMap
    , PortMap
    )
  where

-- FIXME: Merge parsing functions for show mac address table. Make them return
-- complete result, disregarding of request. And then requestor should choose,
-- what he wants.

import qualified Data.Text as T
import           Data.List
import           Control.Applicative
import           Network.Simple.TCP
import qualified Data.Map as M

import BH.IP


newtype SwName      = SwName T.Text
 deriving (Eq, Ord, Show)

data SwInfo         = SwInfo
                        { swName   :: SwName
                        , hostName :: HostName
                        , userName :: T.Text
                        , password :: T.Text
                        , enablePassword :: T.Text
                        , defaultPortSpec :: T.Text
                        }
  deriving (Show)

parseSwInfo :: T.Text -> M.Map SwName SwInfo
parseSwInfo   = M.fromList . map go .  T.lines
  where
    go :: T.Text -> (SwName, SwInfo)
    go t =  let [sn, hn, un, pw, enpw, ds] = T.splitOn ", " t
            in  ( SwName sn
                , SwInfo    { swName   = SwName sn
                            , hostName = T.unpack hn
                            , userName = un
                            , password = pw
                            , enablePassword = enpw
                            , defaultPortSpec = ds
                            }
                )

data Port     = Port {portPrefix :: T.Text, portNum :: Int}
  deriving (Eq, Ord, Show)

{---parsePort :: T.Text -> Either Port
parsePort :: T.Text -> Maybe T.Text
parsePort ts = do
        join $ find (`T.stripPrefix` ts) ["FastEthernet", "Fa", "fa"] >> Just "FastEthernet"
    <|> find (`T.stripPrefix` ts) ["GigabitEthernet", "Gi", "gi"] >> Just "GigabitEthernet"-}

newtype PortNum     = PortNum Int
  deriving (Eq, Ord, Show)

data SwPort2        = SwPort2 {portSw2 :: SwName, portSpec2 :: Port}
  deriving (Eq, Ord, Show)
data SwPort         = SwPort {portSw :: SwName, portSpec :: T.Text, port :: PortNum}
  deriving (Eq, Ord, Show)

type PortMacMap     = M.Map SwPort (Maybe [MacAddr])

type MacPortMap     = M.Map MacAddr (Maybe [SwPort])

type MacIpMap       = M.Map MacAddr [IP]

type PortMap        = M.Map SwPort [(MacAddr, [IP])]

type SwConfig       = M.Map SwName T.Text

