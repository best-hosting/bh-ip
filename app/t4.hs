{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import qualified Data.Map as M
import Data.Maybe
import Data.Yaml
import Data.Monoid
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Aeson.Types as J
import qualified Data.Aeson.Encoding as JE

--import BH.Common

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

class MacPortCollection ce where
    type ColKey ce
    update :: MacPortRef -> MacPortRef -> ce -> ce
    buildRef :: Maybe (ColKey ce) -> ce -> [MacPortRef]

{-class Collection2 c g where
    type ColKey2 c
    --update2 :: MacPortRef -> MacPortRef -> c -> c
    buildRef2 :: Maybe (ColKey2 c) -> c -> [MacPortRef]-}

{-updateRef1t1 :: LensC r (Maybe k) -> r -> r -> M.Map k v -> M.Map k v
updateRef1t1 l old new mm =
  | isNothing (getL l new) = fromMaybe mm $ M.delete <$> mx <*> pure mm
  | isNothing (getL l 
  where
    mOld :: Maybe k
    mOld = 
    mNew :: Maybe k
    mNew = 

updateRef1t1' :: Monoid v => Maybe k -> Maybe k -> M.Map k v -> M.Map k v
updateRef1t1' _       Nothing  mm = fromMaybe mm $ M.delete <$> mx <*> pure mm
updateRef1t1' Nothing (Just x) mm = M.insert x (setL vl jkmempty  MacData {macPort = fromMaybe (Port "") mp, macServer = ""}) mm
  | isNothing (getL l 
  where
    mOld :: Maybe k
    mOld = 
    mNew :: Maybe k
    mNew = 

updateRef1t1 (MacPortRef Nothing _) (MacPortRef (Just x) mp) mm =
    M.insert x (MacData {macPort = fromMaybe (Port "") mp, macServer = ""}) mm
updateRef1t1 (MacPortRef (Just x) _) (MacPortRef (Just y) mp) mm
  | x == y  =
    M.insertWith (\new old -> new{macServer = macServer old}) x (MacData {macPort = fromMaybe (Port "") mp, macServer = ""}) mm
  | otherwise   = error "Impossible"-}

setMacRef :: MacPortRef -> MacData
setMacRef (MacPortRef _ mp) = mempty{macPort = maybe mempty pure mp}

insertMacRef :: MacPortRef -> MacMap -> MacMap
insertMacRef (MacPortRef Nothing  mp) = id
insertMacRef r@(MacPortRef (Just x) mp) = M.insertWith (<>) x (setMacRef r)

-- extend ?
macUpdateRef :: MacPortRef -> MacPortRef -> MacMap -> MacMap
macUpdateRef (MacPortRef mx _) (MacPortRef Nothing _) mm =
    fromMaybe mm $ M.delete <$> mx <*> pure mm
macUpdateRef (MacPortRef Nothing _) r@(MacPortRef (Just x) mp) mm =
    --M.insert x (mempty{macPort = maybe mempty (First . Just) mp}) mm
    --M.insertWith (<>) x (setMacRef r) mm
    insertMacRef r mm
macUpdateRef (MacPortRef (Just x) _) r@(MacPortRef (Just y) mp) mm
  | x == y  =
    --M.insertWith (<>) x (setMacRef r) mm
    insertMacRef r mm
  | otherwise   = error "Impossible"

-- extract ?
macBuildRef :: Maybe MacAddr -> MacMap -> MacPortRef
macBuildRef Nothing  _  = MacPortRef {refMac = Nothing, refPort = Nothing}
macBuildRef (Just m) mm =
    maybe (macBuildRef Nothing mm)
          (\MacData{macPort = First mp} -> MacPortRef{refMac = Just m, refPort = mp})
          $ M.lookup m mm

instance MacPortCollection (M.Map MacAddr MacData) where
    type ColKey (M.Map MacAddr MacData) = MacAddr
    update = macUpdateRef
    buildRef k = (: []) . macBuildRef k

setPortRef :: [MacPortRef] -> PortData
setPortRef refs = PortData {portMacs = pure $ mapMaybe refMac refs}

--insertPortRef :: MacPortRef 
portUpdateRef :: MacPortRef -> MacPortRef -> PortMap -> PortMap
portUpdateRef (MacPortRef _ mx) (MacPortRef _ Nothing) pm =
    fromMaybe pm $ M.delete <$> mx <*> pure pm
portUpdateRef (MacPortRef _ Nothing) r@(MacPortRef mm (Just x)) pm =
    M.insert x (setPortRef [r]) pm
portUpdateRef (MacPortRef mo (Just x)) r@(MacPortRef mm (Just y)) pm
  | x == y =
        let addMac Nothing macs  = maybe macs (\m -> filter (/= m) macs) mo
            addMac (Just m) macs = m : macs
        --in  M.insertWith (\new old -> PortData{portName = portName old, portMacs = addMac mm (portMacs old)}) x (PortData {portMacs = [], portName = ""}) pm
        --in  M.insertWith (\new old -> PortData{portName = portName old, portMacs = addMac mm (portMacs old)}) x mempty pm
        in  M.insertWith (\new old -> new{portMacs = pure $ addMac mm (fromMaybe [] $ getFirst (portMacs old))} <> old) x mempty pm
  | otherwise = error "Impossible"

portBuildRef :: Maybe Port -> PortMap -> [MacPortRef]
portBuildRef Nothing _ = []
portBuildRef (Just p) pm =
    maybe (portBuildRef Nothing pm)
          (\PortData{..} -> map (\m -> MacPortRef {refMac = Just m, refPort = Just p}) . fromMaybe [] $ getFirst portMacs)
          $ M.lookup p pm

instance MacPortCollection (M.Map Port PortData) where
    type ColKey (M.Map Port PortData) = Port
    update = portUpdateRef
    buildRef = portBuildRef

updateMac :: Maybe MacAddr -> MacPortRef -> MacMap -> MacMap
updateMac m r mm = foldr (\x z -> update x r z) mm (buildRef m mm)

updateByMac :: (MacAddr, Maybe Port) -> (MacMap, PortMap) -> (MacMap, PortMap)
updateByMac k (mm, pm) =
    let pm' = updatePortsByMac k mm pm
        mm' = updateMacsByMac k mm
    in  (mm', pm')

updateMacsByMac :: (MacAddr, Maybe Port) -> MacMap -> MacMap
updateMacsByMac (mac, mport) mm =
    let [ref0] = buildRef (Just mac) mm
        ref1 = MacPortRef {refMac = Just mac, refPort = mport}
    in  update ref0 ref1 mm

updatePortsByMac :: (MacAddr, Maybe Port) -> MacMap -> PortMap -> PortMap
updatePortsByMac (_, Nothing) _ pm = pm
updatePortsByMac (mac, Just port) mm pm =
    let oldMacs = mapMaybe refMac $ buildRef (Just port) pm
    in  updatePortsByPort (port, mac : oldMacs) mm pm

updateByPort :: (Port, [MacAddr]) -> (MacMap, PortMap) -> (MacMap, PortMap)
updateByPort k (mm, pm) =
    let pm' = updatePortsByPort k mm pm
        mm' = updateMacsByPort k mm
    in  (mm', pm')

updatePortsByPort :: (Port, [MacAddr]) -> MacMap -> PortMap -> PortMap
updatePortsByPort (newPort, newMacs) mm pm0 =
    let refs0 = buildRef (Just newPort) pm0
        oldMacs = mapMaybe refMac refs0
    in  refsAdd oldMacs . refsRemoveOld oldMacs $ pm0
  where
    refsRemoveNew :: MacAddr -> PortMap -> PortMap
    refsRemoveNew mac pm = flip (maybe pm) (M.lookup mac mm >>= getFirst . macPort) $ \oldPort ->
        if oldPort /= newPort
          then update MacPortRef{refMac = Just mac, refPort = Just oldPort}
                      MacPortRef{refMac = Nothing , refPort = Just oldPort}
                      pm
          else pm
    refsAdd :: [MacAddr] -> PortMap -> PortMap
    refsAdd oldMacs pm = foldr
        (\mac pz ->
            if mac `notElem` oldMacs
              then update ref0 MacPortRef{refMac = Just mac, refPort = Just newPort}
                    . refsRemoveNew mac
                    $ pz
              else pz
        ) pm newMacs
      where ref0 = MacPortRef{refMac = Nothing, refPort = Just newPort}
    refsRemoveOld :: [MacAddr] -> PortMap -> PortMap
    refsRemoveOld oldMacs pm = foldr
        (\mac pz ->
            if mac `notElem` newMacs
              then update MacPortRef{refMac = Just mac, refPort = Just newPort} ref1 pz
              else pz
        ) pm oldMacs
      where ref1 = MacPortRef {refMac = Nothing, refPort = Just newPort}

updateMacsByPort :: (Port, [MacAddr]) -> MacMap -> MacMap
updateMacsByPort (port, macs) mm =
    foldr (\mac mz -> updateMacsByMac (mac, Just port) mz) mm macs

showMap :: (J.ToJSONKey a, ToJSON b) => M.Map a b -> IO ()
showMap mm = B.putStr (encode mm)

showAll :: (MacMap, PortMap) -> IO ()
showAll (mm, pm) = showMap mm >> showMap pm

