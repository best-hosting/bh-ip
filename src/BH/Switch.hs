{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module BH.Switch
    ( SwName (..)
    , SwInfo (..)
    , parseSwInfo
    , SwPort (..)
    , PortId (..)
    , TelnetState (..)
    , PortMacMap (..)
    , MacPortMap (..)
    , SwConfig (..)
    , TelnetRef (..)
    , TelnetRef2 (..)
    , TelnetRef3 (..)
    , telnetRef3
    , TelnetRef4 (..)
    , TelnetEnd
    , TelnetCmd
    , TelnetRes
    , telnetRef4
    , CmdReader (..)
    , MacIpMap (..)
    , PortMap (..)
    , TelnetRefClass (..)
    , TelnetCtx
    , TelnetCtx3
    , telnetLogin
    , TelnetOpClass (..)
    )
  where

import           Network.Simple.TCP
import qualified Data.Text as T
import qualified Data.Map as M
import Data.IORef
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.Trans.Cont
import System.IO.Unsafe

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

-- FIXME: Rename to PortNum .
data SwPort         = SwPort Int
  deriving (Eq, Ord, Show)

-- FIXME: Rename to SwPort .
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

-- FIXME: Passwords should be 'Just T.Text', because if no password is given,
-- i'll not perform specified auth type.
class Eq a => TelnetRefClass t a where
    userNameC :: t a -> T.Text
    passwordC :: t a -> T.Text
    enablePasswordC :: t a -> T.Text
    --swNameC :: t a -> SwName
    --defPortSpecC :: t a -> T.Text
    telnetStateC :: t a -> TelnetState a
    setTelnetStateC :: TelnetState a -> t a -> t a
    -- FIXME: macMap may have different types depending on pass type. If i
    -- query by interface it'll be one. If i query be mac, it'll be another.
    --macMapC :: t a -> PortMacMap

data TelnetRef a    = TelnetRef
                        { switchInfo :: SwInfo
                        , telnetState :: TelnetState a
                        , macMap :: PortMacMap
                        }
  deriving (Show)

instance Eq a => TelnetRefClass TelnetRef a where
    userNameC = userName . switchInfo
    passwordC = password . switchInfo
    enablePasswordC = enablePassword . switchInfo
    --swNameC = swName . switchInfo
    --defPortSpecC = defaultPortSpec . switchInfo
    telnetStateC = telnetState
    setTelnetStateC s r = r{telnetState = s}
    --macMapC = macMap

data TelnetRef2 a   = TelnetRef2
                        { switchInfo2 :: SwInfo
                        , telnetState2 :: TelnetState a
                        , swConfs :: M.Map SwName T.Text
                        }
  deriving (Show)

data TelnetRef3 a   = TelnetRef3
                        { switchInfo3 :: SwInfo
                        , telnetState3 :: TelnetState a
                        , telnetRes :: TelnetOpRes a
                        , telnetCont :: Maybe (ReaderT (IORef (TelnetRef3 a)) IO ())
                        }

class TelnetOpClass a where
    data TelnetOpRes a :: *
    telnetResDef :: TelnetOpRes a
    telnetRef3 :: TelnetOpClass a => IORef (TelnetRef3 a)
    {-# NOINLINE telnetRef3 #-}
    telnetRef3  = unsafePerformIO . newIORef
                    $ TelnetRef3 { telnetState3 = Unauth
                                 , switchInfo3 = undefined
                                 , telnetCont = Nothing
                                 , telnetRes = telnetResDef
                                 }

type TelnetEnd a = ()     -> ReaderT (CmdReader a) IO ()
type TelnetRes a = T.Text -> ReaderT (CmdReader a) IO ()

type TelnetCmd a = T.Text -> ContT () (ReaderT (CmdReader a) IO) ()
data TelnetRef4 a   = TelnetRef4
                        { tFinal  :: Maybe a
                        , tResume :: Maybe (TelnetRes a)
                        , tInt :: Int
                        }

telnetRef4 :: IORef (TelnetRef4 a)
{-# NOINLINE telnetRef4 #-}
telnetRef4  = unsafePerformIO . newIORef
                $ TelnetRef4 { tResume = Nothing
                             , tFinal = Nothing
                             , tInt = 0
                             }

-- FIXME: Query, result and program (telnet commands) are all bound together.
-- I.e. if i query by IP, i definitely need 'findPort' and result will be
-- switch port. Etc. That means, i may group this triple: query type, result
-- type and actual function to execute after login into single class instance.
--
-- That been said, the 'config save' operation is different: it does not have
-- a query. Though, if i consider all this not as queries/response, but just
-- like input/program/output, then..
data CmdReader a = CmdReader { switchInfo4 :: SwInfo
                             , tCon :: TL.TelnetPtr
                             , findMac :: MacAddr
                             , telRef :: IORef (TelnetRef4 a)
                             }

-- FIXME: TelnetRef payload (macMap, saveSws) depends on 'a'.. data families?
instance Eq a => TelnetRefClass TelnetRef2 a where
    userNameC = userName . switchInfo2
    passwordC = password . switchInfo2
    enablePasswordC = enablePassword . switchInfo2
    telnetStateC = telnetState2
    setTelnetStateC s r = r{telnetState2 = s}

type PortMacMap     = M.Map PortId (Maybe [MacAddr])

type MacPortMap     = M.Map MacAddr (Maybe [PortId])

type MacIpMap       = M.Map MacAddr [IP]

type PortMap        = M.Map PortId [(MacAddr, [IP])]

type SwConfig       = M.Map SwName T.Text

-- FIXME: Include TelnetRef into TelnetCtx ?
type TelnetCtx t a  = ContT () (ReaderT (IORef (t a)) IO)

type TelnetCtx3 a  = ContT () (ReaderT (IORef (TelnetRef3 a)) IO)

withReaderTM :: Monad m => (r' -> m r) -> ReaderT r m a -> ReaderT r' m a
withReaderTM f m = ask >>= lift . f >>= lift . runReaderT m

telnetLogin :: (TelnetRefClass t a, TL.HasTelnetPtr con) => con -> T.Text -> TelnetCtx t a (con, T.Text)
telnetLogin con ts = shiftT $ \k -> lift $ do
    tRef <- ask
    r <- liftIO (readIORef tRef)
    let s0 = telnetStateC r
    ms' <- withReaderT (const r) (go s0)
    case ms' of
      Right s'
        | s' == Enabled -> liftIO (atomicWriteIORef tRef (setTelnetStateC s' r)) >> k (con, ts)
        | otherwise     -> liftIO (atomicWriteIORef tRef (setTelnetStateC s' r))
      Left _            -> k (con, ts)
  where
    -- | Return 'Nothing' if i don't understand state. And 'Just state'
    -- otherwise.
    go :: TelnetRefClass t a => (TelnetState a) -> ReaderT (t a) IO (Either String (TelnetState a))
    go Unauth = go AuthUsername
    go s@AuthUsername
        | "Username" `T.isInfixOf` ts || "User Name" `T.isInfixOf` ts = do
            user <- asks userNameC
            liftIO $ TL.telnetSend con . B8.pack $ T.unpack user ++ "\n"
            return (Right s)
        | "Password" `T.isInfixOf` ts = go Password
        -- FIXME: May be drop unexpected state?
        | otherwise                 = return (Right s)
    go s@Password
        | "Password" `T.isInfixOf` ts = do
            pw <- asks passwordC
            liftIO $ TL.telnetSend con . B8.pack $ T.unpack pw ++ "\n"
            return (Right s)
        | ">" `T.isSuffixOf` ts       = go Logged
        | "#" `T.isSuffixOf` ts       = return (Right Enabled)
        | otherwise                 = return (Right s)
    go s@Logged
        | ">" `T.isSuffixOf` ts       = do
            liftIO $ TL.telnetSend con . B8.pack $ "enable\n"
            return (Right s)
        | "Password" `T.isInfixOf` ts = go EnablePassword
        | otherwise                 = return (Right s)
    go s@EnablePassword
        | "Password" `T.isInfixOf` ts = do
            enpw <- asks enablePasswordC
            liftIO $ TL.telnetSend con . B8.pack $ T.unpack enpw ++ "\n"
            return (Right s)
        | "#" `T.isSuffixOf` ts       = return (Right Enabled)
        | otherwise                 = return (Right s)
    go _ = return (Left "Unknown state")

