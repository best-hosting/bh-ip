{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

import qualified Data.Map as M
import Data.Maybe

main :: IO ()
main = print "ura"

newtype MacAddr = MacAddr String
  deriving (Show, Eq, Ord)

newtype Port = Port String
  deriving (Show, Eq, Ord)

data MacData = MacData {macPort :: Port, macServer :: String}
  deriving (Show)

data PortData = PortData {portMacs :: [MacAddr], portName :: String}
  deriving (Show)

data GenRef = GenRef {refMac :: Maybe MacAddr, refPort :: Maybe Port}
  deriving (Show)

type MacMap = M.Map MacAddr MacData

testA :: MacMap
testA = M.fromList
    [ (MacAddr "11", MacData {macPort = Port "1", macServer = "bh1"})
    , (MacAddr "12", MacData {macPort = Port "1", macServer = "bh1"})
    , (MacAddr "2" , MacData {macPort = Port "1", macServer = "bh2"})
    , (MacAddr "3" , MacData {macPort = Port "2", macServer = "bh3"})
    ]

type PortMap = M.Map Port PortData

testB :: PortMap
testB = M.fromList
    [ (Port "1", PortData {portMacs = [MacAddr "11", MacAddr "12", MacAddr "2"], portName = "p1"})
    , (Port "2", PortData {portMacs = [MacAddr "3"], portName = "p2"})
    , (Port "3", PortData {portMacs = [], portName = "p3"})
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

