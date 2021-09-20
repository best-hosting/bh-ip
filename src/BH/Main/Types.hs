{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module BH.Main.Types (
  MacIpMap,
  IpMacMap,
  Config(..),
  MacInfo,
  toMacInfo,
  MacData(..),
  resolveMacIPs,
  --macIPsL,
  IPInfo,
  IPData(..),
  SwPortInfo,
  PortInfo,
  PortData(..),
  portAddrsL,
  resolvePortIPs,
  ToSwPortInfo(..),
  macInfoToIPInfo,
)
where

import Data.Aeson
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Maybe
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.List

import BH.Common
import BH.IP
import BH.Switch


type MacIpMap = M.Map MacAddr (S.Set IP)
type IpMacMap = M.Map IP MacAddr

data Config = Config
                { macIpMap :: MacIpMap
                , ipMacMap :: IpMacMap
                , swInfo :: SwInfo
                }
  deriving (Show)

type VlanInfo = M.Map Vlan

-- TODO: Humans works with IPs, not with mac addresses. So, it's more natural
-- to have IP-mac relation, than mac-[IP] .
type MacInfo = M.Map MacAddr MacData
data MacData = MacData
  { macIPs     :: M.Map Vlan (S.Set IP)
  , macSwPorts :: S.Set SwPort
--, macVendor :: T.Text
  }
  deriving (Eq, Show)


-- FIXME: Replace 'MacIpMap' and 'IpMacMap' this with 'MacInfo' and 'IPInfo'.
-- In fact, i may build 'MacInfo' with vlan and ips directly from nmap xml,
-- because when nmap scans network, vlan is already known. Moreover, 'vlan'
-- should be parent element for 'ips' and 'ports' fields on mac address. The
-- same is true for 'IPInfo': vlan should be parent element for 'macs' and
-- 'ports'. In other words, 'MacData' and 'IPData' should contain vlan-indexed
-- maps. [current]
{-data MacData = MacData
  { macIPs :: VlanInfo IP
  , macSwPorts :: S.Set SwPort
  }

data IPData = IPData
  { ipMacs :: VlanInfo MacAddr
  , ipSwPorts :: S.Set SwPort
  }

data PortData = PortData
  { portState ..
  , portAddrs :: M.Map MacAddr (VlanInfo IP)
  }-}
-- FIXME: If i will store all ips and all macs in 'MacInfo' and 'IPInfo'
-- instead of separate 'MacIpMap' and 'IpMacMap', then conversion 'MacInfo' ->
-- 'SwPortInfo' and 'IPInfo' -> 'SwPortInfo' will be lossy (for some ports and
-- IPs i may not know ports (yet)). Thus, db verification should be done in
-- reverse direction only. [verify]

{-macIPsL :: LensC MacData (S.Set IP)
macIPsL g z@MacData{macIPs = x} = (\x' -> z{macIPs = x'}) <$> g x-}

instance Semigroup MacData where
  x <> y = MacData
            { macIPs = M.unionWith (<>) (macIPs x) (macIPs y)
            , macSwPorts = macSwPorts x <> macSwPorts y
            }

instance ToJSON MacData where
  toJSON MacData {..} =
    object $
      [ "ips"   .= macIPs
      , "ports" .= macSwPorts
      ]

instance FromJSON MacData where
  parseJSON = withObject "MacData" $ \v ->
    MacData
      <$> v .: "ips"
      <*> v .: "ports"

-- TODO: Subnets and vlans for IPs.
type IPInfo = M.Map IP IPData

data IPData = IPData
  { ipMacs :: M.Map Vlan (S.Set MacAddr)
  , ipSwPorts :: S.Set SwPort
  --, ipSubnet :: T.Text
  }
 deriving (Show)

instance Semigroup IPData where
  x <> y = IPData
            { ipMacs    = M.unionWith (<>) (ipMacs x) (ipMacs y)
            , ipSwPorts = ipSwPorts x <> ipSwPorts y
            }

instance ToJSON IPData where
  toJSON IPData{..} =
    object $
      [ "macs"  .= ipMacs
      , "ports" .= ipSwPorts
      ]

instance FromJSON IPData where
  parseJSON = withObject "IPData" $ \v ->
    IPData
      <$> v .: "macs"
      <*> v .: "ports"

type SwPortInfo = M.Map SwPort PortData
type PortInfo = M.Map PortNum PortData

-- TODO: Read port mode (access/trunk) to 'PortData'.
-- FIXME: Do not use 'MacInfo' inside 'PortData. Just 'MacAddr' will be
-- enough. Well, not really. Now in all DBs i may find all info: in 'MacInfo'
-- i may find both IP and 'SwPort', in 'IPInfo' i also may find both 'MacAddr'
-- and 'SwPort'. And this should remain. But i just need to remove duplication
-- of 'ports' from 'PortData'.
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

-- TODO: Query Mac using nmap/ip neigh, if not found.[nmap][arp]
-- FIXME: Use 'IPInfo' to resolve mac ips.
resolveMacIPs :: MacIpMap -> MacInfo -> MacInfo
resolveMacIPs macIpMap = M.mapWithKey $ \m d ->
  d{macIPs = M.singleton (Vlan 500) $ fromMaybe S.empty (M.lookup m macIpMap)}

resolvePortIPs :: MacIpMap -> M.Map a PortData -> M.Map a PortData
resolvePortIPs macIpMap = M.map $ modifyL portAddrsL (resolveMacIPs macIpMap)

-- FIXME: Rename to 'HasPorts'.
class ToSwPortInfo a where
  getSwPorts :: a -> [SwPort]
  fromSwPortInfo :: SwPortInfo -> a

instance ToSwPortInfo SwPortInfo where
  getSwPorts = M.keys
  fromSwPortInfo = id

instance ToSwPortInfo MacInfo where
  -- FIXME: In fact, in all queryX functions i need unique items. May be change
  -- type to 'S.Set' to force uniqueness?
  getSwPorts = nub . concatMap (S.toList . macSwPorts) . M.elems
  fromSwPortInfo = M.foldrWithKey go M.empty
   where
    go :: SwPort -> PortData -> MacInfo -> MacInfo
    go sp PortData{..} z = M.unionWith (<>) z $
      M.map (\x -> x{macSwPorts = S.singleton sp}) portAddrs

instance ToSwPortInfo IPInfo where
  getSwPorts = nub . concatMap (S.toList . ipSwPorts) . M.elems
  -- Here there're two places, where 'SwPort' may be defined: key of
  -- 'SwPortInfo' and 'macSwPorts' record in 'portAddrs :: MacInfo'. And i
  -- will explicitly overwrite 'SwPort' obtained from 'portAddrs' with value
  -- of current 'SwPortInfo' key.
  -- FIXME: Do not use 'MacInfo' in 'PortData'. Use just 'S.Set MacAddr'
  -- instead.
  fromSwPortInfo = M.foldrWithKey go M.empty
   where
    go :: SwPort -> PortData -> IPInfo -> IPInfo
    go p PortData{..} = M.unionWith (<>)
      $ M.map (\x -> x{ipSwPorts = S.singleton p})
      $ macInfoToIPInfo portAddrs

macInfoToIPInfo :: MacInfo -> IPInfo
macInfoToIPInfo = M.foldrWithKey goM M.empty
 where
  goM :: MacAddr -> MacData -> IPInfo -> IPInfo
  goM mac MacData{..} = M.unionWith (<>) (M.foldrWithKey goV M.empty macIPs)
   where
    goV :: Vlan -> S.Set IP -> IPInfo -> IPInfo
    goV vl ips =
      let d = IPData{ipMacs = M.singleton vl (S.singleton mac), ipSwPorts = macSwPorts}
      in  M.unionWith (<>) (foldr (\ip -> M.insertWith (<>) ip d) M.empty ips)

toMacInfo :: MacTableEl -> MacInfo
toMacInfo MacTableEl{..} = M.singleton elMac $
  MacData
    { macIPs = M.singleton elVlan S.empty
    -- I don't know 'SwName' here, so i can't define port.
    , macSwPorts = S.empty
    }

toMacInfo' :: SwName -> MacTableEl -> MacInfo
toMacInfo' portSw MacTableEl{..} = M.singleton elMac $
  MacData
    { macIPs = M.singleton elVlan S.empty
    -- I don't know 'SwName' here, so i can't define port.
    , macSwPorts = S.singleton SwPort{portSpec = elPort, ..}
    }

