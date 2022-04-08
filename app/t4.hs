{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Map as M
import Data.Maybe
import Data.Yaml
import Data.Monoid
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Aeson.Types as J
import qualified Data.Aeson.Encoding as JE
import Data.List

import BH.Common

main :: IO ()
main = showMap testA

newtype MacAddr = MacAddr String
  deriving (Show, Eq, Ord)

instance ToJSON MacAddr where
    toJSON (MacAddr str) = String (T.pack str)

instance J.ToJSONKey MacAddr where
    toJSONKey = J.ToJSONKeyText (\(MacAddr str) -> T.pack str) (\(MacAddr str) -> JE.text . T.pack $ str)

data AddrState = Unreachable | Answering
  deriving (Show)

instance ToJSON AddrState where
    toJSON Unreachable = String "Unreachable"
    toJSON Answering   = String "Answering"

newtype Port = Port String
  deriving (Show, Eq, Ord)

data PortState = Up | NotConnected | Disabled
  deriving (Show)

instance ToJSON PortState where
    toJSON Up = String "Up"
    toJSON NotConnected = String "NotConnected"
    toJSON Disabled = String "Disabled"

instance ToJSON Port where
    toJSON (Port str) = String (T.pack str)

instance J.ToJSONKey Port where
    toJSONKey = J.ToJSONKeyText (\(Port str) -> T.pack str) (\(Port str) -> JE.text . T.pack $ str)

data MacData = MacData
                { macPort   :: First Port
                , macServer :: First String
                , macState  :: First AddrState
                }
  deriving (Show)

macPortL :: LensC MacData (First Port)
macPortL g z@MacData{macPort = x} = (\x' -> z{macPort = x'}) <$> g x

instance Semigroup MacData where
    x <> y = MacData
                { macPort   = macPort x <> macPort y
                , macServer = macServer x <> macServer y
                , macState  = macState  x <> macState y
                }

instance Monoid MacData where
    mempty = MacData {macPort = mempty, macServer = mempty, macState = mempty}

instance ToJSON MacData where
    toJSON MacData {..} =
        object . mapMaybe id $
          [ ("port"   .=) <$> getFirst macPort
          , ("server" .=) <$> getFirst macServer
          , ("state"  .=) <$> getFirst macState
          ]

data PortData = PortData
                  { portMacs :: First [MacAddr]
                  , portName :: First String
                  , portState :: First PortState
                  }
  deriving (Show)

portMacsL :: LensC PortData (First [MacAddr])
portMacsL g z@PortData{portMacs = x} = (\x' -> z{portMacs = x'}) <$> g x

instance Semigroup PortData where
    x <> y = PortData
                { portMacs = portMacs x <> portMacs y
                , portName = portName x <> portName y
                , portState = portState x <> portState y
                }

instance Monoid PortData where
    mempty = PortData
                { portMacs = mempty
                , portName = mempty
                , portState = mempty
                }

instance ToJSON PortData where
    toJSON PortData {..} =
        object . mapMaybe id $
              [ ("macs" .=) <$> getFirst portMacs
              , ("name" .=) <$> getFirst portName
              , ("state" .=) <$> getFirst portState
              ]

newtype IP = IP String
  deriving (Show, Eq, Ord)

instance ToJSON IP where
    toJSON (IP str) = String (T.pack str)

instance J.ToJSONKey IP where
    toJSONKey = J.ToJSONKeyText (\(IP str) -> T.pack str) (\(IP str) -> JE.text . T.pack $ str)

data IPData = IPData {ipMac :: MacAddr, ipState :: AddrState}
  deriving (Show)

instance ToJSON IPData where
    toJSON IPData{..} =
        object $
          [ "mac" .= ipMac
          , "state" .= ipState
          ]

data MacPortRef = MacPortRef {refMac :: Maybe MacAddr, refPort :: Maybe Port}
  deriving (Show)

type MacMap = M.Map MacAddr MacData

testA :: MacMap
testA = M.fromList
    [ (MacAddr "1:1:1", MacData {macPort = pure (Port "sw/1"), macServer = pure "bh1", macState = pure Answering})
    , (MacAddr "1:1:2", MacData {macPort = pure (Port "sw/1"), macServer = pure "bh1", macState = pure Answering})
    , (MacAddr "2:2:2" , MacData {macPort = pure (Port "sw/1"), macServer = pure "bh2", macState = pure Answering})
    , (MacAddr "3:3:3" , MacData {macPort = pure (Port "sw/2"), macServer = pure "bh3", macState = pure Answering})
    ]

type PortMap = M.Map Port PortData

testB :: PortMap
testB = M.fromList
    [ (Port "sw/1", PortData {portMacs = pure [MacAddr "1:1:1", MacAddr "1:1:2", MacAddr "2:2:2"], portName = pure "p1", portState = pure Up})
    , (Port "sw/2", PortData {portMacs = pure [MacAddr "3:3:3"], portName = pure "p2", portState = pure Up})
    , (Port "sw/3", PortData {portMacs = pure [], portName = pure "p3", portState = pure Up})
    ]





-- extend ?
macUpdateRef2 :: MacPortRef -> MacPortRef -> MacMap -> MacMap
macUpdateRef2 (MacPortRef oldMac oldPort) (MacPortRef newMac newPort) mm =
    maybe id (\m ->
        M.insertWith
          (\_ old ->
              old{macPort = First . setPort newPort $ getFirst (macPort old)})
        m mempty{macPort = First newPort})
      newMac
    . maybe id (\m ->
        M.adjust
          (\d -> d{macPort = First . setPort Nothing $ getFirst (macPort d)})
          m)
      oldMac
    $ mm

setPort :: Maybe Port -> Maybe Port -> Maybe Port
setPort Nothing = const Nothing
setPort (Just new) = const (Just new)

portUpdateRef2 :: MacPortRef -> MacPortRef -> PortMap -> PortMap
-- M.delete may be wrong choice here, because 'delete' does not "break
-- binding", it completely deletes element. This may be not what i really
-- want.
portUpdateRef2 (MacPortRef oldMac oldPort) (MacPortRef newMac newPort) pm =
    maybe id (\p ->
        M.insertWith
          (\_ old ->
              old{portMacs = pure
                      . addMac Nothing newMac
                      $ (fromMaybe [] $ getFirst (portMacs old))})
        p mempty{portMacs = pure $ addMac oldMac newMac []})
      newPort
    . maybe id (\p ->
        M.adjust
          (\d ->
              d{portMacs = pure
                      -- If i trust consistency of references, i should not
                      -- delete new mac on old port, because that was /not/
                      -- explicitly requested.
                      -- . addMac (Just newMac) Nothing
                      . addMac oldMac Nothing
                      $ (fromMaybe [] $ getFirst (portMacs d))
               }
          )
          p)
      oldPort
    $ pm


-- This function may be the most generic 'update' function, which should be
-- instantiated according to some type-class instances.
addMac :: (Eq a) => Maybe a -> Maybe a -> [a] -> [a]
addMac Nothing    Nothing macs  = macs
addMac (Just old) Nothing macs  = filter (/= old) macs
addMac Nothing    (Just m) macs = m : macs
addMac (Just old) (Just new) macs = addMac Nothing (Just new) . addMac (Just old) Nothing $ macs

updateMacsByMac6 :: (MacAddr, Maybe Port) -> (MacMap, PortMap) -> (MacMap, PortMap)
updateMacsByMac6 (mac, newPort) (mm0, pm0) =
    let refs =
          ( MacPortRef{refMac = const mac <$> M.lookup mac mm0, refPort = M.lookup mac mm0 >>= getFirst . macPort}
          , MacPortRef{refMac = Just mac, refPort = newPort}
          )
    in  updateRef2 refs (mm0, pm0)

updatePortsByPort8 :: (Port, [MacAddr]) -> (MacMap, PortMap) -> (MacMap, PortMap)
updatePortsByPort8 (newPort, newMacs) (mm0, pm0) =
    let oldMacs = fromMaybe [] $ M.lookup newPort pm0 >>= getFirst . portMacs

        refAddPort =
          ( MacPortRef{refMac = Nothing, refPort = const newPort <$> M.lookup newPort pm0}
          , MacPortRef{refMac = Nothing, refPort = Just newPort}
          )

        -- For macs, which are no longer present, remove mac-port binding,
        -- but do /not/ delete mac and port itself. Thus, i need to use slightly
        -- different refs for MacMap and PortMap.
        -- FIXME: Do i need this now? 'portUpdateRef2' does not delete any
        -- keys.
        (mm1, pm1) =
          foldr
            (\mac (mm, pm) ->
              let refs =
                    ( MacPortRef {refMac = Just mac, refPort = const newPort <$> M.lookup newPort pm0}
                    , MacPortRef {refMac = Nothing, refPort = Just newPort}
                    )
              in  updateRef2 refs (mm, pm)
            )
            (mm0, pm0)
            (oldMacs \\ newMacs)

        refsAdd = map (\mac ->
                      ( MacPortRef {refMac = const mac <$> M.lookup mac mm0, refPort = M.lookup mac mm0 >>= getFirst . macPort}
                      , MacPortRef {refMac = Just mac, refPort = Just newPort}
                      )
                   )
                   (newMacs \\ oldMacs)

    in  foldr updateRef2 (updateRef2 refAddPort (mm1, pm1)) refsAdd

updateRef2 :: (MacPortRef, MacPortRef) -> (MacMap, PortMap) -> (MacMap, PortMap)
updateRef2 ref (mm, pm) =
    ( uncurry macUpdateRef2 ref mm
    , uncurry portUpdateRef2 ref pm
    )



showMap :: (J.ToJSONKey a, ToJSON b) => M.Map a b -> IO ()
showMap mm = B.putStr (encode mm)

showAll :: (MacMap, PortMap) -> IO ()
showAll (mm, pm) = showMap mm >> showMap pm

