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
import Control.Monad

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

data FirstL a = FirstL [a]
  deriving (Show)

instance Semigroup (FirstL a) where
    FirstL [] <> y = y
    x         <> _ = x

instance Monoid (FirstL a) where
    mempty = FirstL []

data PortData = PortData
                  { portMacs :: First [MacAddr]
                  , portServers :: First String
                  , portState :: First PortState
                  }
  deriving (Show)

portMacsL :: LensC PortData (First [MacAddr])
portMacsL g z@PortData{portMacs = x} = (\x' -> z{portMacs = x'}) <$> g x

instance Semigroup PortData where
    x <> y = PortData
                { portMacs = portMacs x <> portMacs y
                , portServers = portServers x <> portServers y
                , portState = portState x <> portState y
                }

instance Monoid PortData where
    mempty = PortData
                { portMacs = mempty
                , portServers = mempty
                , portState = mempty
                }

instance ToJSON PortData where
    toJSON PortData {..} =
        object . mapMaybe id $
              [ ("macs" .=) <$> getFirst portMacs
              , ("name" .=) <$> getFirst portServers
              , ("state" .=) <$> getFirst portState
              ]

data PortData2 = PortData2
                  { unknownMacs2 :: First [MacAddr]
                  , portServers2 :: First [M.Map String [MacAddr]]
                  , portState2 :: First PortState
                  }
  deriving (Show)

instance Semigroup PortData2 where
    x <> y = PortData2
                { unknownMacs2 = unknownMacs2 x <> unknownMacs2 y
                , portServers2 = portServers2 x <> portServers2 y
                , portState2 = portState2 x <> portState2 y
                }

instance Monoid PortData2 where
    mempty = PortData2
                { unknownMacs2 = mempty
                , portServers2 = mempty
                , portState2 = mempty
                }

instance ToJSON PortData2 where
    toJSON PortData2 {..} =
        object . mapMaybe id $
              [ ("macs" .=) <$> getFirst unknownMacs2
              , ("name" .=) <$> getFirst portServers2
              , ("state" .=) <$> getFirst portState2
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
    , (MacAddr "2:2:2" , MacData {macPort = pure (Port "sw/1"), macServer = pure "bh1", macState = pure Answering})
    , (MacAddr "3:3:3" , MacData {macPort = pure (Port "sw/2"), macServer = pure "bh3", macState = pure Answering})
    ]

type PortMap = M.Map Port PortData
type PortMap2 = M.Map Port PortData2

-- FIXME: Port may have a list of names.. huh?
-- FIXME: Define type for all fields to avoid (pure []) and the like
-- constructs.
testB :: PortMap
testB = M.fromList
    [ (Port "sw/1", PortData {portMacs = pure [MacAddr "1:1:1", MacAddr "1:1:2", MacAddr "2:2:2"], portServers = pure "bh1", portState = pure Up})
    , (Port "sw/2", PortData {portMacs = pure [MacAddr "3:3:3"], portServers = pure "bh3", portState = pure Up})
    , (Port "sw/3", PortData {portMacs = pure [], portServers = pure "", portState = pure Up})
    ]

testA2 :: MacMap
testA2 = M.fromList
    [ ( MacAddr "1:1:1"
      , MacData
        { macPort = pure (Port "sw/1")
        , macServer = pure "bh1"
        , macState = pure Answering
        }
      )
    , ( MacAddr "1:1:2"
      , MacData
        { macPort = pure (Port "sw/1")
        , macServer = pure "bh1"
        , macState = pure Answering
        }
      )
    , ( MacAddr "2:2:2"
      , MacData
        { macPort = pure (Port "sw/1")
        , macServer = pure "bh2"
        , macState = pure Answering
        }
      )
    , ( MacAddr "3:3:3" 
      , MacData
        { macPort = pure (Port "sw/2")
        , macServer = pure "bh3"
        , macState = pure Answering
        }
      )
    ]

{-testB2 :: PortMap2
testB2 = M.fromList
    [ ( Port "sw/1"
      , PortData2
        { unknownMacs2 = pure
            [ MacAddr "1:1:1"
            , MacAddr "1:1:2"
            , MacAddr "2:2:2"
            ]
        , portServers2 = pure
            [ "bh1"
            , "bh2"
            ]
        , portState2 = pure Up
        }
      )
    , ( Port "sw/2"
      , PortData2
        { unknownMacs2 = pure [MacAddr "3:3:3"]
        , portServers2 = pure ["bh3"]
        , portState2 = pure Up
        }
      )
    , ( Port "sw/3"
      , PortData2
        { unknownMacs2 = pure []
        , portServers2 = pure []
        , portState2 = pure Up
        }
      )
    ]-}





-- extend ?
{-macUpdateRef :: MacPortRef -> MacPortRef -> MacMap -> MacMap
macUpdateRef (MacPortRef oldMac oldPort) (MacPortRef newMac newPort) mm =
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
    $ mm-}

macUpdateRef2 :: NameRef -> NameRef -> MacMap -> MacMap
macUpdateRef2 (NameRef oldMac oldPort oldName) (NameRef newMac newPort newName) mm =
    maybe id (\m ->
        M.insertWith
          (\_ old ->
              old{ macPort = First . setPort newPort $ getFirst (macPort old)
                 , macServer = First newName
                 })
        m mempty{macPort = First newPort, macServer = First newName})
      newMac
    . maybe id (\m ->
        M.adjust
          (\d -> d{ macPort = First Nothing
                  , macServer = First Nothing
                  })
          m)
      oldMac
    $ mm

setPort :: Maybe Port -> Maybe Port -> Maybe Port
setPort Nothing = const Nothing
setPort (Just new) = const (Just new)

{-portUpdateRef :: MacPortRef -> MacPortRef -> PortMap -> PortMap
-- M.delete may be wrong choice here, because 'delete' does not "break
-- binding", it completely deletes element. This may be not what i really
-- want.
portUpdateRef (MacPortRef oldMac oldPort) (MacPortRef newMac newPort) pm =
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
    $ pm-}

-- FIXME: Should i check 'Just'/Nothing for non-list fields and check equality
-- before deleting, like i do for 'addMac'? Or just delete non-list fields
-- without any checks?
--
-- This is effectively (setL new . setL mempty) for all fields. Maybe a class
-- parametrizing /lenses/ over data type may allow to merge all these into one
-- function.
portUpdateRef2 :: NameRef -> NameRef -> PortMap -> PortMap
portUpdateRef2 (NameRef oldMac oldPort oldName) (NameRef newMac newPort newName) pm =
    maybe id (\p ->
        M.insertWith
          (\_ old ->
              old{ portMacs = pure
                      . addMac Nothing newMac
                      $ (fromMaybe [] $ getFirst (portMacs old))
                 , portServers = First newName
                 })
        p mempty{portMacs = First $ (: []) <$> newMac, portServers = First newName})
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
               , portServers = mempty
               }
          )
          p)
      oldPort
    $ pm

portUpdateRef3 :: NameRef -> NameRef -> PortMap2 -> PortMap2
portUpdateRef3 (NameRef oldMac oldPort oldName) (NameRef newMac newPort newName) pm =
      maybe id (\p ->
          M.insertWith
            (\_ old ->
                old{ unknownMacs2 = maybe (unknownMacs2 old) pure
                        $ (:) <$> newMac <*> getFirst (unknownMacs2 old)
                   , portServers2 = maybe (portServers2 old) pure
                        $ (:) <$> newName <*> getFirst (portServers2 old)
                   })
          p mempty
              { unknownMacs2 = First $ (: []) <$> newMac
              , portServers2 = First $ (: []) <$> newName
              }
        )
        newPort
    . maybe id (\p ->
        M.adjust
          (\d ->
              d{unknownMacs2 = maybe (unknownMacs2 d) pure
                      -- If i trust consistency of references, i should not
                      -- delete new mac on old port, because that was /not/
                      -- explicitly requested.
                    $ getFirst (unknownMacs2 d) >>= filterM (\x -> (x /=) <$> oldMac)
               , portServers2 = maybe (portServers2 d) pure
                    $ getFirst (portServers2 d) >>= filterM (\x -> (x /=) <$> oldName)
               }
          )
          p
        )
        oldPort
    $ pm

-- This function may be the most generic 'update' function, which should be
-- instantiated according to some type-class instances.
addMac :: (Eq a) => Maybe a -> Maybe a -> [a] -> [a]
addMac Nothing    Nothing macs  = macs
addMac (Just old) Nothing macs  = filter (/= old) macs
addMac Nothing    (Just m) macs = m : macs
addMac (Just old) (Just new) macs = addMac Nothing (Just new) . addMac (Just old) Nothing $ macs

{-updateRef :: (MacPortRef, MacPortRef) -> (MacMap, PortMap) -> (MacMap, PortMap)
updateRef ref (mm, pm) =
    ( uncurry macUpdateRef ref mm
    , uncurry portUpdateRef ref pm
    )-}

updateRef2 :: (NameRef, NameRef) -> (MacMap, PortMap) -> (MacMap, PortMap)
updateRef2 ref (mm, pm) =
    ( uncurry macUpdateRef2 ref mm
    , uncurry portUpdateRef2 ref pm
    )

{-updateRef3 :: (NameRef, NameRef) -> (MacMap, PortMap2) -> (MacMap, PortMap2)
updateRef3 ref (mm, pm) =
    ( uncurry macUpdateRef2 ref mm
    , uncurry portUpdateRef3 ref pm
    )-}

{-updateMacPort :: (MacAddr, Maybe Port) -> (MacMap, PortMap) -> (MacMap, PortMap)
updateMacPort (mac, newPort) (mm0, pm0) =
    let refs =
          ( MacPortRef{refMac = const mac <$> M.lookup mac mm0, refPort = M.lookup mac mm0 >>= getFirst . macPort}
          , MacPortRef{refMac = Just mac, refPort = newPort}
          )
    in  updateRef refs (mm0, pm0)-}

{-updatePortMacs :: (Port, [MacAddr]) -> (MacMap, PortMap) -> (MacMap, PortMap)
updatePortMacs (port, newMacs) (mm0, pm0) =
    let oldMacs = fromMaybe [] $ M.lookup port pm0 >>= getFirst . portMacs

        -- For adding port i don't even need to check whether it's already
        -- there or not, just assume, it's not there.
        refAddPort =
          ( MacPortRef{refMac = Nothing, refPort = Nothing}
          , MacPortRef{refMac = Nothing, refPort = Just port}
          )

        -- For macs, which are no longer present, remove mac-port binding,
        -- but do /not/ delete mac and port itself. Thus, i need to use slightly
        -- different refs for MacMap and PortMap.
        -- FIXME: Do i need this now? 'portUpdateRef' does not delete any
        -- keys.
        -- If there's no port yet, 'oldMacs' will be empty. So i may safely
        -- assume, that if 'foldr' run, port was already there.
        (mm1, pm1) =
          foldr
            (\mac (mm, pm) ->
              let refs =
                    ( MacPortRef {refMac = Just mac, refPort = Just port}
                    , MacPortRef {refMac = Nothing , refPort = Just port}
                    )
              in  updateRef refs (mm, pm)
            )
            (mm0, pm0)
            (oldMacs \\ newMacs)

        refsAdd =
          foldr
            (\mac z ->
                  ( MacPortRef {refMac = const mac <$> M.lookup mac mm0, refPort = M.lookup mac mm0 >>= getFirst . macPort}
                  , MacPortRef {refMac = Just mac, refPort = Just port}
                  )
                  : z
             )
             [refAddPort]
             (newMacs \\ oldMacs)

    in  foldr updateRef (mm1, pm1) refsAdd -}

showMap :: (J.ToJSONKey a, ToJSON b) => M.Map a b -> IO ()
showMap mm = B.putStr (encode mm)

showAll :: (MacMap, PortMap) -> IO ()
showAll (mm, pm) = showMap mm >> showMap pm

showAll2 :: (MacMap, PortMap2) -> IO ()
showAll2 (mm, pm) = showMap mm >> showMap pm

data NameRef = NameRef  { nrMac  :: Maybe MacAddr
                        , nrPort :: Maybe Port
                        , nrName :: Maybe String
                        }
  deriving (Show)

updatePortName :: (Port, String) -> (MacMap, PortMap) -> (MacMap, PortMap)
updatePortName (port, newName) (mm0, pm0) =
    let oldMacs = fromMaybe [] $ M.lookup port pm0 >>= getFirst . portMacs
        oldName = M.lookup port pm0 >>= getFirst . portServers
        (mm1, pm1) =
          foldr
            (\mac (mm, pm) ->
              let refs =
                    ( NameRef
                        { nrMac = Just mac
                        , nrPort = Just port
                        , nrName = oldName
                        }
                    , NameRef
                        { nrMac  = Just mac
                        , nrPort = Nothing
                        , nrName = oldName
                        }
                    )
              in  updateRef2 refs (mm, pm)
            )
            (mm0, pm0)
            oldMacs
        refChangeName =
            ( NameRef
                { nrMac = Nothing
                , nrPort = Just port
                , nrName = oldName
                }
            , NameRef
                { nrMac = Nothing
                , nrPort = Just port
                , nrName = Just newName
                }
            )
    in updateRef2 refChangeName (mm1, pm1)

updateMacName :: (MacAddr, String) -> (MacMap, PortMap) -> (MacMap, PortMap)
updateMacName (mac, newName) (mm0, pm0) =
    let oldName = M.lookup mac mm0 >>= getFirst . macServer
        oldPort = M.lookup mac mm0 >>= getFirst . macPort
        refSplit =  ( NameRef
                        { nrMac  = Just mac
                        , nrPort = oldPort
                        , nrName = oldName
                        }
                    , NameRef
                        { nrMac  = Nothing
                        , nrPort = oldPort
                        , nrName = oldName
                        }
                    )
        refChangeName = ( NameRef
                            { nrMac = Just mac
                            , nrPort = Nothing
                            , nrName = oldName
                            }
                        , NameRef
                            { nrMac = Just mac
                            , nrPort = Nothing
                            , nrName = Just newName
                            }
                        )
    in  updateRef2 refChangeName . updateRef2 refSplit $ (mm0, pm0)

updatePortMacs2 :: (Port, [MacAddr]) -> (MacMap, PortMap) -> (MacMap, PortMap)
updatePortMacs2 (port, newMacs) (mm0, pm0) =
    let oldMacs = fromMaybe [] $ M.lookup port pm0 >>= getFirst . portMacs
        oldName = M.lookup port pm0 >>= getFirst . portServers

        -- For adding port i don't even need to check whether it's already
        -- there or not, just assume, it's not there.
        refAddPort =
          ( NameRef {nrMac = Nothing, nrPort = Nothing, nrName = Nothing}
          , NameRef {nrMac = Nothing, nrPort = Just port, nrName = oldName}
          )

        -- For macs, which are no longer present, remove mac-port binding,
        -- but do /not/ delete mac and port itself. Thus, i need to use slightly
        -- different refs for MacMap and PortMap.
        -- FIXME: Do i need this now? 'portUpdateRef' does not delete any
        -- keys.
        -- If there's no port yet, 'oldMacs' will be empty. So i may safely
        -- assume, that if 'foldr' run, port was already there.
        -- These 'NameRef'-s will delete both port and name from mac. Well, i
        -- assume, that the one update holds correct info, and if mac looses
        -- port, than name will remain with port.
        (mm1, pm1) =
          foldr
            (\mac (mm, pm) ->
              let refSplit =
                    ( NameRef {nrMac = Just mac, nrPort = Just port, nrName = oldName}
                    , NameRef {nrMac = Nothing, nrPort = Just port, nrName = oldName}
                    )
              in  updateRef2 refSplit (mm, pm)
            )
            (mm0, pm0)
            (oldMacs \\ newMacs)

        refsAdd =
          foldr
            (\mac z ->
                let oldPort = M.lookup mac mm0 >>= getFirst . macPort
                    refSplit =
                      ( NameRef
                          { nrMac = const mac <$> M.lookup mac mm0
                          , nrPort = oldPort
                          , nrName = oldName
                          }
                      , NameRef
                          { nrMac = Nothing
                          , nrPort = oldPort
                          , nrName = oldName
                          }
                      )
                    refAdd =
                      ( NameRef
                          { nrMac = const mac <$> M.lookup mac mm0
                          , nrPort = Nothing
                          , nrName = Nothing
                          }
                      , NameRef
                          { nrMac  = Just mac
                          , nrPort = Just port
                          , nrName = oldName
                          }
                      )
                in  refAdd : refSplit : z
             )
             [refAddPort]
             (newMacs \\ oldMacs)

    in  foldr updateRef2 (mm1, pm1) refsAdd

updateMacPort2 :: (MacAddr, Maybe Port) -> (MacMap, PortMap) -> (MacMap, PortMap)
updateMacPort2 (mac, newPort) (mm0, pm0) =
    let oldName = M.lookup mac mm0 >>= getFirst . macServer
        oldPort = M.lookup mac mm0 >>= getFirst . macPort
        refChange =
          ( NameRef
              { nrMac = Just mac
              , nrPort = oldPort
              , nrName = oldName
              }
          , NameRef
              { nrMac = Just mac
              , nrPort = newPort
              , nrName = oldName
              }
          )
    in  updateRef2 refChange (mm0, pm0)





{-updatePortName2 :: (Port, String) -> (MacMap, PortMap2) -> (MacMap, PortMap2)
updatePortName2 (port, newName) (mm0, pm0) =
    let oldMacs = fromMaybe [] $ M.lookup port pm0 >>= getFirst . unknownMacs2
        (mm1, pm1) =
          foldr
            (\mac (mm, pm) ->
              let oldName = M.lookup mac mm0 >>= getFirst . macServer
                  refSplit =
                    ( NameRef
                        { nrMac = Just mac
                        , nrPort = Just port
                        , nrName = oldName
                        }
                    , NameRef
                        { nrMac  = Just mac
                        , nrPort = Nothing
                        , nrName = oldName
                        }
                    )
              in  updateRef3 refSplit (mm, pm)
            )
            (mm0, pm0)
            oldMacs
        refChangeName =
            ( NameRef
                { nrMac = Nothing
                , nrPort = Just port
                , nrName = Nothing
                }
            , NameRef
                { nrMac = Nothing
                , nrPort = Just port
                , nrName = Just newName
                }
            )
    in updateRef3 refChangeName (mm1, pm1)
-}
