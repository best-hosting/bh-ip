{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module BH.Main.Types (
  MacIpMap,
  IpMacMap,
  Config(..),
  MacInfo,
  toMacInfo,
  MacData(..),
  swPortInfoToMacInfo,
  resolveMacIPs,
  macIPsL,
  IPInfo,
  IPData(..),
  macInfoToIPInfo,
  swPortInfoToIPInfo,
  SwPortInfo,
  PortInfo,
  PortData(..),
  portAddrsL,
  resolvePortIPs,
)
where

import Data.Aeson
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Maybe

import BH.Common
import BH.IP
import BH.Switch


-- FIXME: Replace 'MacIpMap' and 'IpMacMap' this with 'MacInfo' and 'IPInfo'.
-- In fact, i may build 'MacInfo' with vlan and ips directly from nmap xml,
-- because when nmap scans network, vlan is already known. Moreover, 'vlan'
-- should be parent element for 'ips' and 'ports' fields on mac address. The
-- same is true for 'IPInfo': vlan should be parent element for 'macs' and
-- 'ports'. In other words, 'MacData' and 'IPData' should contain vlan-indexed
-- maps.
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

instance Semigroup MacData where
  x <> y = MacData
            { macVlan = macVlan y
            , macIPs = macIPs x <> macIPs y
            , macSwPorts = macSwPorts x <> macSwPorts y
            }

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

swPortInfoToMacInfo :: SwPortInfo -> MacInfo
swPortInfoToMacInfo = M.foldrWithKey go M.empty
 where
  go :: SwPort -> PortData -> MacInfo -> MacInfo
  go sp PortData{..} z = M.unionWith (<>) z $
    M.map (\x -> x{macSwPorts = S.singleton sp}) portAddrs

-- TODO: Query Mac using nmap/ip neigh, if not found.[nmap][arp]
resolveMacIPs :: MacIpMap -> MacInfo -> MacInfo
resolveMacIPs macIpMap = M.mapWithKey $ \m d -> d{macIPs = fromMaybe mempty (M.lookup m macIpMap)}

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
  { ipMacAddrs :: S.Set MacAddr
  , ipVlan :: Vlan
  , ipSwPorts :: S.Set SwPort
  --, ipSubnet :: T.Text
  }
 deriving (Show)

instance Semigroup IPData where
  x <> y = IPData
            { ipMacAddrs = ipMacAddrs x <> ipMacAddrs y
            , ipVlan = ipVlan y
            , ipSwPorts = ipSwPorts x <> ipSwPorts y
            }

instance ToJSON IPData where
  toJSON IPData{..} =
    object $
      [ "macs" .= ipMacAddrs
      , "vlan" .= ipVlan
      , "ports" .= ipSwPorts
      ]

instance FromJSON IPData where
  parseJSON = withObject "IPData" $ \v ->
    IPData
      <$> v .: "macs"
      <*> v .: "vlan"
      <*> v .: "ports"

-- Here there're two places, where 'SwPort' may be defined: key of
-- 'SwPortInfo' and 'macSwPorts' record in 'portAddrs :: MacInfo'. And i will
-- explicitly overwrite 'SwPort' obtained from 'portAddrs' with value of
-- current 'SwPortInfo' key.
swPortInfoToIPInfo :: SwPortInfo -> IPInfo
swPortInfoToIPInfo = M.foldrWithKey go M.empty
 where
  go :: SwPort -> PortData -> IPInfo -> IPInfo
  go p PortData{..} = M.unionWith (<>)
    $ M.map (\x -> x{ipSwPorts = S.singleton p})
    $ macInfoToIPInfo portAddrs

macInfoToIPInfo :: MacInfo -> IPInfo
macInfoToIPInfo = M.foldrWithKey go M.empty
 where
  go :: MacAddr -> MacData -> IPInfo -> IPInfo
  go mac MacData{..} =
    let d = IPData{ipMacAddrs = S.singleton mac, ipVlan = macVlan, ipSwPorts = macSwPorts}
    in  M.unionWith (<>) $ foldr (\ip z -> M.insertWith (<>) ip d z) M.empty macIPs

type SwPortInfo = M.Map SwPort PortData
type PortInfo = M.Map PortNum PortData

-- TODO: Read port mode (access/trunk) to 'PortData'.
-- FIXME: Do not use 'MacInfo' inside 'PortData. Just 'MacAddr' will be
-- enough.
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

resolvePortIPs :: MacIpMap -> M.Map a PortData -> M.Map a PortData
resolvePortIPs macIpMap = M.map $ modifyL portAddrsL (resolveMacIPs macIpMap)

