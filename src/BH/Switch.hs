{-# LANGUAGE OverloadedStrings #-}

module BH.Switch
    ( SwName (..)
    , SwInfo (..)
    , parseSwInfo
    , SwPort (..)
    , PortId (..)
    , TelnetState (..)
    , PortMacMap (..)
    , TelnetRef (..)
    , MacIpMap (..)
    , PortMap (..)
    )
  where

import           Network.Simple.TCP
import qualified Data.Text as T
import qualified Data.Map as M

import BH.IP


newtype SwName      = SwName T.Text
 deriving (Eq, Ord, Show)

data SwInfo         = SwInfo
                        { swName   :: SwName
                        , hostName :: HostName
                        , userName :: T.Text
                        , password :: T.Text
                        , enablePasword :: T.Text
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
                            , enablePasword = enpw
                            , defaultPortSpec = ds
                            }
                )

data SwPort         = SwPort Int
  deriving (Eq, Ord, Show)

data PortId         = PortId {portSw :: SwName, port :: SwPort}
  deriving (Eq, Ord, Show)

data TelnetState    = Unauth
                    | AuthUsername
                    | Password
                    | Logged
                    | EnableRequested
                    | EnablePassword
                    | Enabled
                    | ShowMacAddressTable PortId
                    | Exit
  deriving (Eq, Show)

type PortMacMap     = M.Map PortId (Maybe [MacAddr])
data TelnetRef      = TelnetRef
                        { switchInfo :: SwInfo
                        , telnetState :: TelnetState
                        , macMap :: PortMacMap
                        }
  deriving (Show)

type MacIpMap       = M.Map MacAddr [IP]

type PortMap        = M.Map PortId [(MacAddr, [IP])]

