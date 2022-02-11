{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as M
import Data.Maybe
import Data.Yaml
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Aeson.Types as J
import qualified Data.Aeson.Encoding as JE

main :: IO ()
main = showMap testA

newtype MacAddr = MacAddr String
  deriving (Show, Eq, Ord)

instance ToJSON MacAddr where
    toJSON (MacAddr str) = String (T.pack str)

instance J.ToJSONKey MacAddr where
    toJSONKey = J.ToJSONKeyText (\(MacAddr str) -> T.pack str) (\(MacAddr str) -> JE.text . T.pack $ str)

newtype Port = Port String
  deriving (Show, Eq, Ord)

instance ToJSON Port where
    toJSON (Port str) = String (T.pack str)

instance J.ToJSONKey Port where
    toJSONKey = J.ToJSONKeyText (\(Port str) -> T.pack str) (\(Port str) -> JE.text . T.pack $ str)

data MacData = MacData {macPort :: Port, macServer :: String}
  deriving (Show)

instance ToJSON MacData where
    toJSON MacData {..} =
        object $
          [ "port" .= macPort
          , "server" .= macServer
          ]

data PortData = PortData {portMacs :: [MacAddr], portName :: String}
  deriving (Show)

instance ToJSON PortData where
    toJSON PortData {..} =
        object
          [ "macs" .= portMacs
          , "name" .= portName
          ]

data GenRef = GenRef {refMac :: Maybe MacAddr, refPort :: Maybe Port}
  deriving (Show)

type MacMap = M.Map MacAddr MacData

testA :: MacMap
testA = M.fromList
    [ (MacAddr "1:1:1", MacData {macPort = Port "sw/1", macServer = "bh1"})
    , (MacAddr "1:1:2", MacData {macPort = Port "sw/1", macServer = "bh1"})
    , (MacAddr "2:2:2" , MacData {macPort = Port "sw/1", macServer = "bh2"})
    , (MacAddr "3:3:3" , MacData {macPort = Port "sw/2", macServer = "bh3"})
    ]

type PortMap = M.Map Port PortData

testB :: PortMap
testB = M.fromList
    [ (Port "sw/1", PortData {portMacs = [MacAddr "1:1:1", MacAddr "1:1:2", MacAddr "2:2:2"], portName = "p1"})
    , (Port "sw/2", PortData {portMacs = [MacAddr "3:3:3"], portName = "p2"})
    , (Port "sw/3", PortData {portMacs = [], portName = "p3"})
    ]

class Collection ce where
    type ColKey ce
    update :: GenRef -> GenRef -> ce -> ce
    buildRef :: Maybe (ColKey ce) -> ce -> [GenRef]

macGenRef :: GenRef -> GenRef -> MacMap -> MacMap
macGenRef (GenRef mx _) (GenRef Nothing _) mm =
    maybe mm (flip M.delete mm) mx
macGenRef (GenRef Nothing _) (GenRef (Just x) mp) mm =
    M.insert x (MacData {macPort = fromMaybe (Port "") mp, macServer = ""}) mm
macGenRef (GenRef (Just x) _) (GenRef (Just y) mp) mm
  | x == y  =
    M.insertWith (\new old -> new{macServer = macServer old}) x (MacData {macPort = fromMaybe (Port "") mp, macServer = ""}) mm
  | otherwise   = error "Impossible"

macBuildGenRef :: Maybe MacAddr -> MacMap -> GenRef
macBuildGenRef Nothing  _  = GenRef {refMac = Nothing, refPort = Nothing}
macBuildGenRef (Just m) mm =
    maybe (macBuildGenRef Nothing mm)
          (\MacData{macPort = p} -> GenRef{refMac = Just m, refPort = Just p})
          $ M.lookup m mm

instance Collection (M.Map MacAddr MacData) where
    type ColKey (M.Map MacAddr MacData) = MacAddr
    update = macGenRef
    buildRef k = (: []) . macBuildGenRef k

portGenRef :: GenRef -> GenRef -> PortMap -> PortMap
portGenRef (GenRef _ mx) (GenRef _ Nothing) pm =
    maybe pm (flip M.delete pm) mx
portGenRef (GenRef _ Nothing) (GenRef mm (Just x)) pm =
    M.insert x (PortData {portMacs = maybe [] (: []) mm, portName = ""}) pm
portGenRef (GenRef mo (Just x)) (GenRef mm (Just y)) pm
  | x == y =
        let addMac Nothing macs  = maybe macs (\m -> filter (/= m) macs) mo
            addMac (Just m) macs = m : macs
        in  M.insertWith (\_ old -> PortData{portName = portName old, portMacs = addMac mm (portMacs old)}) x (PortData {portMacs = [], portName = ""}) pm
  | otherwise = error "Impossible"

portBuildGenRef :: Maybe Port -> PortMap -> [GenRef]
portBuildGenRef Nothing _ = [GenRef {refMac = Nothing, refPort = Nothing}]
portBuildGenRef (Just p) pm =
    maybe (portBuildGenRef Nothing pm)
          (\PortData {portMacs = macs} -> map (\m -> GenRef {refMac = Just m, refPort = Just p}) macs)
          $ M.lookup p pm

instance Collection (M.Map Port PortData) where
    type ColKey (M.Map Port PortData) = Port
    update = portGenRef
    buildRef = portBuildGenRef

updateMac :: Maybe MacAddr -> GenRef -> MacMap -> MacMap
updateMac m r mm = foldr (\x z -> update x r z) mm (buildRef m mm)

updateMacsByMac :: (MacAddr, Maybe Port) -> MacMap -> MacMap
updateMacsByMac (mac, mport) mm =
    let [ref0] = buildRef (Just mac) mm
        ref1 = GenRef {refMac = Just mac, refPort = mport}
    in  update ref0 ref1 mm

updatePortsByMac :: (MacAddr, Maybe Port) -> MacMap -> PortMap -> PortMap
updatePortsByMac (_, Nothing) _ pm = pm
updatePortsByMac (mac, Just port) mm pm =
    let oldMacs = mapMaybe refMac $ buildRef (Just port) pm
    in  updatePortsByPort (port, mac : oldMacs) mm pm

updatePortsByPort :: (Port, [MacAddr]) -> MacMap -> PortMap -> PortMap
updatePortsByPort (newPort, newMacs) mm pm0 =
    let refs0 = buildRef (Just newPort) pm0
        oldMacs = mapMaybe refMac refs0
    in  refsAdd oldMacs . refsRemoveOld oldMacs $ pm0
  where
    refsRemoveNew :: MacAddr -> PortMap -> PortMap
    refsRemoveNew mac pm = flip (maybe pm) (macPort <$> M.lookup mac mm) $ \oldPort ->
        if oldPort /= newPort
          then update GenRef{refMac = Just mac, refPort = Just oldPort}
                      GenRef{refMac = Nothing , refPort = Just oldPort}
                      pm
          else pm
    refsAdd :: [MacAddr] -> PortMap -> PortMap
    refsAdd oldMacs pm = foldr
        (\mac pz ->
            if mac `notElem` oldMacs
              then update ref0 GenRef{refMac = Just mac, refPort = Just newPort}
                    . refsRemoveNew mac
                    $ pz
              else pz
        ) pm newMacs
      where ref0 = GenRef{refMac = Nothing, refPort = Just newPort}
    refsRemoveOld :: [MacAddr] -> PortMap -> PortMap
    refsRemoveOld oldMacs pm = foldr
        (\mac pz ->
            if mac `notElem` newMacs
              then update GenRef{refMac = Just mac, refPort = Just newPort} ref1 pz
              else pz
        ) pm oldMacs
      where ref1 = GenRef {refMac = Nothing, refPort = Just newPort}

showMap :: (J.ToJSONKey a, ToJSON b) => M.Map a b -> IO ()
showMap mm = B.putStr (encode mm)

