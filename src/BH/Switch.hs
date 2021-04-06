{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

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
    , TState2 (..)
    , TStateCmd (..)
    , TelnetRefC (..)
    )
  where

import           Network.Simple.TCP
import qualified Data.Text as T
import qualified Data.Map as M
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.Trans.Cont

import qualified Network.Telnet.LibTelnet as TL
import qualified Data.ByteString.Char8 as B8

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

data SwPort         = SwPort Int
  deriving (Eq, Ord, Show)

data PortId         = PortId {portSw :: SwName, port :: SwPort}
  deriving (Eq, Ord, Show)

data TelnetState a  = Unauth
                    | AuthUsername
                    | Password
                    | Logged
                    | EnableRequested
                    | EnablePassword
                    | Enabled
                    | Command a
                    | Exit
  deriving (Eq, Show)

class TStateCmd a where
    getCmd :: TState2 -> a

data TState2   = Unauth2 | forall a. (Eq a, Show a) => Command2 a

class Eq a => TelnetRefC t a where
    userNameC :: t a -> T.Text
    passwordC :: t a -> T.Text
    enablePasswordC :: t a -> T.Text
    telnetStateC :: t a -> TelnetState a
    setTelnetStateC :: TelnetState a -> t a -> t a
    conC :: TL.HasTelnetPtr c => t a -> c

data TelnetRef a    = TelnetRef
                        { switchInfo :: SwInfo
                        , telnetState :: TelnetState a
                        , macMap :: PortMacMap
                        , telnetCon :: forall c. TL.HasTelnetPtr c => c
                        }

instance Eq a  => TelnetRefC TelnetRef a where
    userNameC = userName . switchInfo
    passwordC = password . switchInfo
    enablePasswordC = enablePassword . switchInfo
    telnetStateC = telnetState
    setTelnetStateC s r = r{telnetState = s}
    conC = telnetCon

type PortMacMap     = M.Map PortId (Maybe [MacAddr])

type MacIpMap       = M.Map MacAddr [IP]

type PortMap        = M.Map PortId [(MacAddr, [IP])]

