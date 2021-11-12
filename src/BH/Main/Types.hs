{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module BH.Main.Types (
  Config(..),
  MacInfo,
  MacData(..),
  macIPsL,
  macPortL,
  IPInfo,
  IPData(..),
  ipMacPortsL,
  IPState(..),
  PortInfo,
  PortData(..),
  portAddrsL,
  ModPort(..),
  ModMac(..),
  ModIP(..),
)
where

import Data.Aeson
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Maybe
import Data.Monoid
import Data.Time
import Text.Read

import BH.Common
import BH.IP
import BH.Switch


data Config = Config
                { swInfo :: SwInfo
                , runVlan :: Vlan
                , cacheTime :: UTCTime
                , updateInterval :: NominalDiffTime
                , timeFile :: FilePath
                , nmapHost :: T.Text
                }
  deriving (Show)

type VlanInfo a = M.Map Vlan a

{-data VlanData a = VlanData
                    { vlanPort :: Maybe Port
                    , vlanAddrs  :: S.Set a
                    }
  deriving(Eq, Show)

instance ToJSON a => ToJSON (VlanData a) where
  toJSON VlanData{..} =
    object $
      [ "port"  .= vlanPort
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

-- FIXME: Use type PortRef = First (Port, PortState) instead of type. Really,
-- i don't need separate type. [current]
{-data PortRef = PortRef
                { refPort  :: Port
                , refState :: PortState
                }
  deriving (Show)-}

-- Single mac can't be on several ports.
-- I need 'First IPState' in 'macIPsL' because 'ipState' in 'IPData' has that
-- type and, therefore, if i'll have 'IPData' with 'Nothing' in 'ipState', i
-- won't be able to add it to 'macIPs' (i.e. i won't be able to create a
-- reference to a valid element of 'IPInfo').
data MacData = MacData
  { macIPs    :: M.Map IP (First IPState)
  -- FIXME: Do i really need Semigroup for MacData?
-- FIXME: Do not use pairs, there're lists in yaml. Use 'PortRef' instead.
-- FIXME: Rename 'macPort' to 'macPorts'.
  , macPort :: M.Map Port (First PortState)
--, macVendor :: T.Text
  }
  deriving (Show)

macIPsL :: LensC MacData (M.Map IP (First IPState))
macIPsL g z@MacData{macIPs = x} = (\x' -> z{macIPs = x'}) <$> g x

macPortL :: LensC MacData (M.Map Port (First PortState))
macPortL g z@MacData{macPort = x} = (\x' -> z{macPort = x'}) <$> g x

instance Semigroup MacData where
  x <> y = MacData
            { macIPs = macIPs x <> macIPs y
            , macPort = macPort x <> macPort y
            }

instance Monoid MacData where
  mempty = MacData
            { macIPs  = M.empty
            , macPort = M.empty
            }

instance ToJSON MacData where
  toJSON MacData{..} = object ["ips" .= macIPs, "port" .= macPort]

-- FIXME: Make both fields optional.
instance FromJSON MacData where
  parseJSON = withObject "MacData" $ \v ->
    MacData
      <$> v .:  "ips"
      <*> (fromMaybe M.empty <$> v .:? "port")

-- FIXME: Replace 'MacIpMap' and 'IpMacMap' this with 'MacInfo' and 'IPInfo'.
-- In fact, i may build 'MacInfo' with vlan and ips directly from nmap xml,
-- because when nmap scans network, vlan is already known. Moreover, 'vlan'
-- should be parent element for 'ips' and 'ports' fields on mac address. The
-- same is true for 'IPInfo': vlan should be parent element for 'macs' and
-- 'ports'. In other words, 'MacData' and 'IPData' should contain vlan-indexed
-- maps. [current]
{-data MacData = MacData
  { macIPs :: VlanInfo IP
  , macPorts :: S.Set Port
  }

data IPData = IPData
  { ipMacs :: VlanInfo MacAddr
  , ipPorts :: S.Set Port
  }

data PortData = PortData
  { portState ..
  , portAddrs :: M.Map MacAddr (VlanInfo IP)
  }-}
-- FIXME: In fact, ports should also be grouped by vlan. Because the same
-- mac/IP may have different ports in different vlans. Or the same. That's
-- completely unrelated and it should be the same in 'xInfo' structure.

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
-- I need 'First IPState' here to define 'Monoid' instance (without it,
-- 'mempty' won't be left and right identity).
data IPData = IPData
  { ipMacPorts :: M.Map MacAddr (M.Map Port (First PortState))
  , ipState :: First IPState
  --, ipSubnet :: T.Text
  }
 deriving (Show)

ipMacPortsL :: LensC IPData (M.Map MacAddr (M.Map Port (First PortState)))
ipMacPortsL g z@IPData{ipMacPorts = x} = (\x' -> z{ipMacPorts = x'}) <$> g x

instance Semigroup IPData where
    x <> y = IPData
              { ipMacPorts = ipMacPorts x <> ipMacPorts y
              , ipState = ipState x <> ipState y
              }

instance Monoid IPData where
  mempty = IPData {ipMacPorts = M.empty, ipState = First Nothing}

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

type PortInfo = M.Map Port PortData

-- TODO: Read port mode (access/trunk) to 'PortData'.
data PortData = PortData
  { portAddrs :: M.Map MacAddr (M.Map IP (First IPState))
  , portState :: First PortState
  --, portMode :: PortMode
  }
  deriving (Show)

instance Semigroup PortData where
    x <> y = PortData
            { portAddrs = portAddrs x <> portAddrs y
            , portState = portState x <> portState y
            }

instance Monoid PortData where
    mempty = PortData {portAddrs = M.empty, portState = pure Up}

portAddrsL :: LensC PortData (M.Map MacAddr (M.Map IP (First IPState)))
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

-- TODO: Query Mac using nmap/ip neigh, if not found.[nmap][arp]
-- FIXME: vlan should be the topmost level. Not inside 'MacInfo', 'IPInfo',
-- whatever. Every maps should be inside vlan. And vlan should be removed
-- early at start.

-- FIXME: In fact, in all queryX functions i need unique items. May be change
-- type to 'S.Set' to force uniqueness? [current]
-- FIXME: sw port depends on vlan i'm working on. So...? Should i restrict
-- entire program run to single vlan?

class ModPort a where
  setPortState :: PortState -> Port -> a -> a
  delPort :: S.Set MacAddr -> Port -> a -> a
  addPort :: PortData -> Port -> a -> a

instance ModPort PortInfo where
  setPortState s = M.adjust (\d -> d{portState = pure s})
  delPort macs port swPortInfo = fromMaybe swPortInfo $ do
    PortData{..} <- M.lookup port swPortInfo
    let f | S.size macs /= S.size (M.keysSet portAddrs) =
              M.adjust (modifyL portAddrsL (\z -> foldr M.delete z macs))
          | otherwise = M.delete
    return (f port swPortInfo)
  addPort pd p = M.insert p pd

-- FIXME: Or i may write instance for 'M.Map MacAddr (Maybe (Port,
-- PortState)) instead, because i send selection (S.Set MacAddr) in any case..
-- That's depends on does 'MacAddr -- Nothing' has sense or not. And it seems
-- it does.
instance ModPort (M.Map Port (First PortState)) where
  setPortState s = M.adjust (const (pure s))
  delPort _ = M.delete
  addPort d port = M.insert port (portState d)

class ModMac a where
  delMac :: (S.Set IP, S.Set Port) -> MacAddr -> a -> a
  addMac :: MacData -> MacAddr -> a -> a

-- For 'PortData'.
instance ModMac (M.Map MacAddr (M.Map IP (First IPState))) where
  addMac d mac = M.insert mac (macIPs d)
  delMac _ = M.delete

-- For 'IPData'.
instance ModMac (M.Map MacAddr (M.Map Port (First PortState))) where
  addMac d mac = M.insert mac (macPort d)
  delMac _ = M.delete

instance ModMac MacInfo where
  addMac = flip M.insert
  delMac (ips, ports) mac macInfo = M.adjust go mac macInfo
   where
    go :: MacData -> MacData
    go = modifyL macIPsL (\z -> foldr M.delete z ips)
          . modifyL macPortL (\z -> foldr M.delete z ports)

class ModIP a where
  setIPState :: IPState -> IP -> a -> a
  delIP :: S.Set MacAddr -> IP -> a -> a
  addIP :: IPData -> IP -> a -> a

instance ModIP IPInfo where
  setIPState s = M.adjust (\d -> d{ipState = pure s})
  delIP macs ip ipInfo = fromMaybe ipInfo $ do
    IPData{..} <- M.lookup ip ipInfo
    let f | macs /= M.keysSet ipMacPorts =
              --M.adjust (modifyL ipMacPortsL (M.filterWithKey (\k _ -> k `S.notMember` macs)))
              M.adjust (modifyL ipMacPortsL (\x -> foldr M.delete x macs))
          | otherwise = M.delete
    return (f ip ipInfo)
  addIP d ip = M.insert ip d

instance ModIP (M.Map IP (First IPState)) where
  setIPState s = M.adjust (const (pure s))
  delIP _ = M.delete
  addIP d ip = M.insert ip (ipState d)

