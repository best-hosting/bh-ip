{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

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
    , telnetLoginC
    )
  where

import           Network.Simple.TCP
import qualified Data.Text as T
import qualified Data.Map as M
import Data.IORef
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

data TelnetRef a    = TelnetRef
                        { switchInfo :: SwInfo
                        , telnetState :: TelnetState a
                        , macMap :: PortMacMap
                        }
  deriving (Show)

instance Eq a => TelnetRefC TelnetRef a where
    userNameC = userName . switchInfo
    passwordC = password . switchInfo
    enablePasswordC = enablePassword . switchInfo
    telnetStateC = telnetState
    setTelnetStateC s r = r{telnetState = s}

type PortMacMap     = M.Map PortId (Maybe [MacAddr])

type MacIpMap       = M.Map MacAddr [IP]

type PortMap        = M.Map PortId [(MacAddr, [IP])]

telnetLoginC :: (TelnetRefC t a, TL.HasTelnetPtr con) => IORef (t a) -> con -> T.Text -> ContT () IO (con, T.Text)
telnetLoginC tRef con ts = shiftT $ \k -> liftIO $ do
    r <- readIORef tRef
    let s0 = telnetStateC r
    ms' <- runMaybeT . runReaderT (go s0) $ r
    case ms' of
      Just s'
        | s' == Enabled -> atomicWriteIORef tRef (setTelnetStateC s' r) >> k (con, ts)
        | otherwise     -> atomicWriteIORef tRef (setTelnetStateC s' r)
      Nothing           -> k (con, ts)
  where
    -- | Return 'Nothing' if i don't understand state. And 'Just state'
    -- otherwise.
    go :: TelnetRefC t a => (TelnetState a) -> ReaderT (t a) (MaybeT IO) (TelnetState a)
    go Unauth = go AuthUsername
    go s@AuthUsername
        | "Username" `T.isInfixOf` ts || "User Name" `T.isInfixOf` ts = do
            user <- asks userNameC
            liftIO $ TL.telnetSend con . B8.pack $ T.unpack user ++ "\n"
            return s
        | "Password" `T.isInfixOf` ts = go Password
        | otherwise                 = return s
    go s@Password
        | "Password" `T.isInfixOf` ts = do
            pw <- asks passwordC
            liftIO $ TL.telnetSend con . B8.pack $ T.unpack pw ++ "\n"
            return s
        | ">" `T.isSuffixOf` ts       = go Logged
        | "#" `T.isSuffixOf` ts       = return Enabled
        | otherwise                 = return s
    go s@Logged
        | ">" `T.isSuffixOf` ts       = do
            liftIO $ TL.telnetSend con . B8.pack $ "enable\n"
            return s
        | "Password" `T.isInfixOf` ts = go EnablePassword
        | otherwise                 = return s
    go s@EnablePassword
        | "Password" `T.isInfixOf` ts = do
            enpw <- asks enablePasswordC
            liftIO $ TL.telnetSend con . B8.pack $ T.unpack enpw ++ "\n"
            return s
        | "#" `T.isSuffixOf` ts       = return Enabled
        | otherwise                 = return s
    go _ = fail "Unknown state"

