{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module BH.Main.Types (
  MacIpMap,
  IpMacMap,
  Config(..),
  MacInfo,
  toMacInfo,
  MacData(..),
  macIPsL,
  IPInfo,
  IPData(..),
  SwPortInfo,
  PortInfo,
  PortData(..),
  portAddrsL,
)
where

import Data.Aeson
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import BH.Common
import BH.IP
import BH.Switch


-- FIXME: Replace this with 'MacInfo' and 'IPInfo'.
type MacIpMap = M.Map MacAddr (S.Set IP)
type IpMacMap = M.Map IP MacAddr

data Config = Config
                { macIpMap :: MacIpMap
                , ipMacMap :: IpMacMap
                , swInfo :: SwInfo
                }
  deriving (Show)

-- TODO: Humans works with IPs, not with mac addresses. So, it's more natural
-- to have IP-mac relation, than mac-[IP] .
type MacInfo = M.Map MacAddr MacData
data MacData = MacData
  { macVlan :: Vlan
  --, macVendor :: T.Text
  , macIPs :: S.Set IP
  , macSwPorts :: S.Set SwPort
  }
  deriving (Eq, Show)

macIPsL :: LensC MacData (S.Set IP)
macIPsL g z@MacData{macIPs = x} = (\x' -> z{macIPs = x'}) <$> g x

instance ToJSON MacData where
  toJSON MacData {..} =
    object $
      [ "vlan" .= macVlan
      , "ips"  .= macIPs
      , "ports" .= macSwPorts
      ]

instance FromJSON MacData where
  parseJSON = withObject "MacData" $ \v ->
    MacData
      <$> v .: "vlan"
      <*> v .: "ips"
      <*> v .: "ports"

toMacInfo :: MacTableEl -> MacInfo
toMacInfo MacTableEl{..} = M.singleton elMac $
  MacData
    { macVlan = elVlan
    , macIPs = mempty
    , macSwPorts = S.empty
    }

-- TODO: Subnets and vlans for IPs.
type IPInfo = M.Map IP IPData

data IPData = IPData
  { ipMacData :: MacData
  , ipVlan :: Vlan
  , ipSubnet :: T.Text
  }
 deriving (Show)

type SwPortInfo = M.Map SwPort PortData
type PortInfo = M.Map PortNum PortData

-- TODO: Read port mode (access/trunk) to 'PortData'.
data PortData = PortData
  { portState :: PortState
  , --, portMode :: PortMode
    portAddrs :: MacInfo
  }
  deriving (Eq, Show)

portAddrsL :: LensC PortData MacInfo
portAddrsL g z@PortData{portAddrs = x} = (\x' -> z{portAddrs = x'}) <$> g x

instance ToJSON PortData where
  toJSON PortData{..} =
    object $
      [ "state" .= portState
      , "addrs" .= portAddrs
      ]

instance FromJSON PortData where
  parseJSON = withObject "PortData" $ \v ->
    PortData
      <$> v .: "state"
      <*> v .: "addrs"

instance Semigroup PortData where
    x <> y = PortData{portState = portState x, portAddrs = portAddrs x <> portAddrs y}

