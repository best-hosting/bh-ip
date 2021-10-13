{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module BH.Main.Types (
  MacIpMap,
  IpMacMap,
  Config(..),
  MacInfo,
  toMacInfo,
  toMacData,
  MacData(..),
  macIPsL,
  --macIPsL,
  IPInfo,
  IPData(..),
  ipMacPortsL,
  IPState(..),
  SwPortInfo,
  PortInfo,
  PortData(..),
  portAddrsL,
  ToSwPortInfo(..),
  C3(..),
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
import Data.Monoid
import Data.Time
import Text.Read

import BH.Common
import BH.IP
import BH.Switch


type MacIpMap = M.Map MacAddr (S.Set IP)
type IpMacMap = M.Map IP MacAddr

data Config = Config
                { macIpMap :: MacIpMap
                , ipMacMap :: IpMacMap
                , swInfo :: SwInfo
                , runVlan :: Vlan
                , cacheTime :: UTCTime
                , updateInterval :: NominalDiffTime
                , timeFile :: FilePath
                , nmapHost :: T.Text
                }
  deriving (Show)

type VlanInfo a = M.Map Vlan a

{-data VlanData a = VlanData
                    { vlanSwPort :: Maybe SwPort
                    , vlanAddrs  :: S.Set a
                    }
  deriving(Eq, Show)

instance Ord a => Semigroup (VlanData a) where
  x <> y = VlanData
            { vlanSwPort = vlanSwPort y
            , vlanAddrs  =
                if vlanSwPort x == vlanSwPort y
                  then vlanAddrs x <> vlanAddrs y
                  else                vlanAddrs y
            }

instance ToJSON a => ToJSON (VlanData a) where
  toJSON VlanData{..} =
    object $
      [ "port"  .= vlanSwPort
      , "addrs" .= vlanAddrs
      ]

instance (Ord a, FromJSON a) => FromJSON (VlanData a) where
  parseJSON = withObject "VlanData" $ \v ->
    VlanData
      <$> v .:? "port"
      <*> v .:  "addrs"-}

-- TODO: Humans works with IPs, not with mac addresses. So, it's more natural
-- to have IP-mac relation, than mac-[IP] .
type MacInfo = M.Map MacAddr MacData

-- Single mac can't be on several ports.
data MacData = MacData
  { macIPs    :: M.Map IP IPState
  , macSwPort :: Last (SwPort, PortState)
--, macVendor :: T.Text
  }
  deriving (Show)

class C3 a where
  type C3Args a
  ipLens :: C3Args a -> LensC a (M.Map IP IPState)

instance C3 MacData where
  type C3Args MacData = ()
  ipLens _ = macIPsL

macIPsL :: LensC MacData (M.Map IP IPState)
macIPsL g z@MacData{macIPs = x} = (\x' -> z{macIPs = x'}) <$> g x

instance Semigroup MacData where
  x <> y = MacData
            { macIPs    = macIPs y    <> macIPs x
            , macSwPort = macSwPort x <> macSwPort y
            }

instance Monoid MacData where
  mempty = MacData{macIPs = M.empty, macSwPort = Last Nothing}

instance ToJSON MacData where
  toJSON MacData{..} =
    if macSwPort == Last Nothing
      then object ["ips" .= macIPs]
      else object ["ips" .= macIPs, "port" .= macSwPort]

instance FromJSON MacData where
  parseJSON = withObject "MacData" $ \v ->
    MacData
      <$> v .:  "ips"
      <*> (Last <$> v .:? "port")

toMacData :: SwName -> MacTableEl -> MacData
toMacData portSw MacTableEl{..} =
  MacData
    { macIPs = M.empty
    , macSwPort = Last (Just (SwPort{portSpec = elPort, ..}, Up))
    }

toMacInfo :: SwName -> MacTableEl -> MacInfo
toMacInfo n x@(MacTableEl{..}) = M.singleton elMac (toMacData n x)

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
-- FIXME: In fact, ports should also be grouped by vlan. Because the same
-- mac/IP may have different ports in different vlans. Or the same. That's
-- completely unrelated and it should be the same in 'xInfo' structure.

{-macIPsL :: LensC MacData (S.Set IP)
macIPsL g z@MacData{macIPs = x} = (\x' -> z{macIPs = x'}) <$> g x-}

-- TODO: Subnets and vlans for IPs.
type IPInfo = M.Map IP IPData

data IPState = Unreachable | Answering
 deriving (Show, Read)

instance ToJSON IPState where
  toJSON = toJSON . show

instance FromJSON IPState where
  parseJSON = withText "IPState" (either fail return . readEither . T.unpack)

-- Single IP can have several macs (though, this is broken network) and, thus,
-- can be on several ports.
data IPData = IPData
  { ipMacPorts :: M.Map MacAddr (Maybe (SwPort, PortState))
  , ipState :: Last IPState
  --, ipSubnet :: T.Text
  }
 deriving (Show)

ipMacPortsL :: LensC IPData (M.Map MacAddr (Maybe (SwPort, PortState)))
ipMacPortsL g z@IPData{ipMacPorts = x} = (\x' -> z{ipMacPorts = x'}) <$> g x

instance Semigroup IPData where
  x <> y = IPData
            -- Map's 'Monoid' instance uses left-biased 'union'.
            { ipMacPorts = ipMacPorts y <> ipMacPorts x
            , ipState = ipState y
            }

instance Monoid IPData where
  mempty = IPData {ipMacPorts = M.empty, ipState = Last Nothing}

instance ToJSON IPData where
  toJSON IPData{..} = object
    [ "macPorts" .= ipMacPorts
    , "state" .= ipState
    ]

instance FromJSON IPData where
  parseJSON = withObject "IPData" $ \v ->
    IPData
      <$> v .: "macPorts"
      <*> v .: "state"

type SwPortInfo = M.Map SwPort PortData
type PortInfo = M.Map PortNum PortData

-- TODO: Read port mode (access/trunk) to 'PortData'.
-- FIXME: Do not use 'MacInfo' inside 'PortData. Just 'MacAddr' will be
-- enough. Well, not really. Now in all DBs i may find all info: in 'MacInfo'
-- i may find both IP and 'SwPort', in 'IPInfo' i also may find both 'MacAddr'
-- and 'SwPort'. And this should remain. But i just need to remove duplication
-- of 'ports' from 'PortData'.
data PortData = PortData
  { portAddrs :: M.Map MacAddr (M.Map IP IPState)
  , portState :: Last PortState
  --, portMode :: PortMode
  }
  deriving (Show)

instance C3 PortData where
  type C3Args PortData = MacAddr
  ipLens mac = portAddrsL . mapL mac . maybeL M.empty

portAddrsL :: LensC PortData (M.Map MacAddr (M.Map IP IPState))
portAddrsL g z@PortData{portAddrs = x} = (\x' -> z{portAddrs = x'}) <$> g x

instance ToJSON PortData where
  toJSON PortData{..} =
    object $
      [ "addrs" .= portAddrs
      , "state" .= portState
      ]

instance FromJSON PortData where
  parseJSON = withObject "PortData" $ \v ->
    PortData
      <$> v .: "addrs"
      <*> v .: "state"

instance Semigroup PortData where
    x <> y = PortData
              { portAddrs = M.unionWith (<>) (portAddrs x) (portAddrs y)
              , portState = portState x <> portState y
              }

instance Monoid PortData where
  mempty = PortData {portAddrs = M.empty, portState = Last Nothing}

-- TODO: Query Mac using nmap/ip neigh, if not found.[nmap][arp]
-- FIXME: Use 'IPInfo' to resolve mac ips. [current]
-- FIXME: vlan should be the topmost level. Not inside 'MacInfo', 'IPInfo',
-- whatever. Every maps should be inside vlan. And vlan should be removed
-- early at start. [current]
{-resolveMacIPs :: MacIpMap -> MacInfo -> MacInfo
resolveMacIPs macIpMap = M.mapWithKey $ \m d ->
  d { macIPs = fromMaybe M.empty
                (M.lookup m macIpMap >>= return . M.fromSet (const Answering))
    }

resolvePortIPs :: MacIpMap -> M.Map a PortData -> M.Map a PortData
resolvePortIPs macIpMap = M.map $ modifyL portAddrsL (resolveMacIPs macIpMap)-}

-- FIXME: Rename to 'HasPorts'.
class ToSwPortInfo a where
  getSwPorts :: a -> [SwPort]
  fromSwPortInfo :: SwPortInfo -> a

instance ToSwPortInfo SwPortInfo where
  getSwPorts = M.keys
  fromSwPortInfo = id

{-instance ToSwPortInfo (VlanInfo a) where
  getSwPorts = nub . mapMaybe vlanSwPort . M.elems-}

instance ToSwPortInfo MacInfo where
  -- FIXME: In fact, in all queryX functions i need unique items. May be change
  -- type to 'S.Set' to force uniqueness? [current]
  -- FIXME: sw port depends on vlan i'm working on. So...? Should i restrict
  -- entire program run to single vlan? [current]
  getSwPorts = nub . mapMaybe (fmap fst . getLast . macSwPort) . M.elems
  fromSwPortInfo = M.foldrWithKey go M.empty
   where
    go :: SwPort -> PortData -> MacInfo -> MacInfo
    go sp PortData{..} z =
      let macSwPort = Last $ maybe Nothing (Just . (sp, )) . getLast $ portState
      in  M.foldrWithKey (\mac macIPs -> M.insertWith (<>) mac MacData{..}) z portAddrs

instance ToSwPortInfo IPInfo where
  getSwPorts = nub . map fst . catMaybes . concatMap (M.elems . ipMacPorts) . M.elems
  -- Here there're two places, where 'SwPort' may be defined: key of
  -- 'SwPortInfo' and 'macSwPorts' record in 'portAddrs :: MacInfo'. And i
  -- will explicitly overwrite 'SwPort' obtained from 'portAddrs' with value
  -- of current 'SwPortInfo' key.
  -- FIXME: Do not use 'MacInfo' in 'PortData'. Use just 'S.Set MacAddr'
  -- instead.
  fromSwPortInfo = M.foldrWithKey go M.empty
   where
    go :: SwPort -> PortData -> IPInfo -> IPInfo
    go port PortData{..} z =
      M.foldrWithKey goMac z portAddrs
     where
      portRef :: Maybe (SwPort, PortState)
      portRef = maybe Nothing (Just . (port, )) . getLast $ portState
      goMac :: MacAddr -> M.Map IP IPState -> IPInfo -> IPInfo
      goMac mac xs z' =
        M.foldrWithKey (\ip st -> M.insertWith (<>) ip
          IPData
            { ipState = Last (Just st)
            , ipMacPorts = M.singleton mac portRef
            }
          ) z' xs

macInfoToIPInfo :: MacInfo -> IPInfo
macInfoToIPInfo = M.foldrWithKey goM M.empty
 where
  goM :: MacAddr -> MacData -> IPInfo -> IPInfo
  goM mac MacData{..} =
    let d = IPData{ipMacPorts = M.singleton mac (getLast macSwPort)}
        y :: IPInfo
        y = M.foldrWithKey (\ip s -> M.insertWith (<>) ip d{ipState = Last (Just s)}) M.empty macIPs
    in  M.unionWith (<>) y

