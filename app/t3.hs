{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}

import qualified Options.Applicative as O
import Control.Monad.Trans.Cont
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Text as T
import Data.IORef
import System.IO.Unsafe
import qualified Data.Attoparsec.Text as A
import Data.Typeable
import Data.Monoid
import Data.Maybe
import Data.Type.Equality
import qualified Data.Map as M
import qualified Data.Yaml as Y
import Data.Aeson
import qualified Data.Aeson.Encoding as JE
import qualified Data.ByteString as B
import Data.Void

import BH.Common

data Option a = Option
  { optName :: String
  , optParser :: String -> Maybe a -- ^ How to parse option argument.
  }

instance Functor Option where
  fmap f (Option name p) = Option name (fmap f . p)

optMatches :: Option a -> String -> Bool
optMatches opt s = s == '-' : '-' : optName opt

data Parser a where
  NilP :: a -> Parser a
  ConsP :: Option (a -> b)
        -> Parser a -> Parser b

instance Functor Parser where
  fmap f (NilP x) = NilP (f x)
  fmap f (ConsP opt rest) = ConsP (fmap (f.) opt) rest

instance Applicative Parser where
  pure = NilP
  NilP f <*> p = fmap f p
  ConsP opt rest <*> p =
    ConsP (fmap uncurry opt) ((,) <$> rest <*> p)

-- | Create Parser for option.
option :: String -> (String -> Maybe a) -> Parser a
option name p = ConsP (fmap const (Option name p)) (NilP ())

-- | Create Parser for option using 'read' as argument parser.
optionR :: Read a => String -> Parser a
optionR name = option name p
  where
    p arg = case reads arg of
      [(r, "")] -> Just r
      _       -> Nothing

data User = User
  { userName :: String
  , userId :: Integer
  } deriving Show

parser :: Parser User
parser = User <$> option "name" Just <*> optionR "id"

runParser :: Parser a -> [String] -> Maybe (a, [String])
runParser (NilP x) args = Just (x, args)
runParser (ConsP _ _) [] = Nothing
runParser p (arg : args) =
  case stepParser p arg args of
    Nothing -> Nothing
    Just (p', args') -> runParser p' args'

stepParser :: Parser a -> String -> [String] -> Maybe (Parser a, [String])
stepParser p arg args = case p of
  NilP _ -> Nothing
  ConsP opt rest
    | optMatches opt arg -> case args of
        [] -> Nothing
        (value : args') -> do
          f <- optParser opt value
          return (fmap f rest, args')
    | otherwise -> do
        (rest', args') <- stepParser rest arg args
        return (ConsP opt rest', args')

main :: IO ()
main = undefined

{-data TState = TState {tResume :: forall c. Maybe (T.Text -> StateT c IO ()), tInt :: Int}

type T c d = ContT () (StateT c IO) d-}

shiftW :: Monad m => ((a -> m r, b) -> ContT r m r) -> b -> ContT r m a
shiftW f x = shiftT (\k -> f (k, x))

data TState = TState
                { tResume :: Maybe (T.Text -> StateT [Int] (ReaderT (IORef TState) IO) ())
                , tInt :: Int
                }

data TState2 = forall c. Typeable c => TState2
                { tResume2 :: Maybe (() -> StateT T.Text (ReaderT (IORef TState2) IO) ())
                , tResult2 :: Maybe (A.Result c)
                , tInt2 :: Int
                }

data TState3 c = TState3
                { tResume3 :: Maybe (() -> StateT T.Text (ReaderT (IORef (TState3 c)) IO) ())
                , tResult3 :: c
                , tInt3 :: Int
                }

tRef :: IORef TState
{-# NOINLINE tRef #-}
tRef  = unsafePerformIO (newIORef TState{tResume = Nothing, tInt = 0})

g0 :: IO ((), [Int])
g0 = do
  ref <- newIORef TState{tResume = Nothing, tInt = 0}
  flip runReaderT ref . flip runStateT [] . evalContT $ (gRunner "text1") -- 1
  flip runReaderT ref . flip runStateT [] . evalContT $ (gRunner "text2") -- 2
  flip runReaderT ref . flip runStateT [] . evalContT $ (gRunner "text3") -- 3
  flip runReaderT ref . flip runStateT [] . evalContT $ (gRunner "text4") -- 4

prog :: T.Text -> ContT () (StateT [Int] (ReaderT (IORef TState) IO)) ()
prog ts = shiftW g1 ts >>= shiftW g2 >>= shiftW g3 >>= liftIO . print

gRunner :: T.Text -> ContT () (StateT [Int] (ReaderT (IORef TState) IO)) ()
gRunner ts = do
    tRef <- ask
    st <- liftIO (readIORef tRef)
    let mCont = tResume st
        n = tInt st
    liftIO $ print $ "Runner: " <> show n
    case mCont of
      Nothing -> liftIO (putStrLn "Start cmd.")  >> prog ts
      Just c  -> liftIO (putStrLn "Continue cmd.") >> lift (c ts)


g1 :: (T.Text -> StateT [Int] (ReaderT (IORef TState) IO) (), T.Text)
      -> ContT () (StateT [Int] (ReaderT (IORef TState) IO)) ()
g1 (k, ts) = do
  let i = 1
  s <- get
  liftIO $ print $ "a: " <> show ts <> ", " <> show s
  modify (i :)
  ref <- ask
  liftIO $ atomicWriteIORef ref (TState{tResume = Just k, tInt = i})

g2 :: (T.Text -> StateT [Int] (ReaderT (IORef TState) IO) (), T.Text)
      -> ContT () (StateT [Int] (ReaderT (IORef TState) IO)) ()
g2 (k, ts) = do
  let i = 2
  s <- get
  liftIO $ print $ "b: " <> show ts <> ", " <> show s
  modify (i :)
  ref <- ask
  liftIO $ atomicWriteIORef ref (TState{tResume = Just k, tInt = i})

g3 :: (T.Text -> StateT [Int] (ReaderT (IORef TState) IO) (), T.Text)
      -> ContT () (StateT [Int] (ReaderT (IORef TState) IO)) ()
g3 (k, ts) = do
  let i = 3
  s <- get
  liftIO $ print $ "c: " <> show ts <> ", " <> show s
  modify (i :)
  ref <- ask
  liftIO $ atomicWriteIORef ref (TState{tResume = Just k, tInt = i})

{-f0 :: IO ((), T.Text)
f0 = do
  ref <- newIORef TState2{tResume2 = Nothing, tResult2 = Just $ A.parse (A.string "") "", tInt2 = 0}
  flip runReaderT ref . flip runStateT "abc" . evalContT $ fRunner -- 1
  flip runReaderT ref . flip runStateT "def" . evalContT $ fRunner -- 2
  flip runReaderT ref . flip runStateT "ghi" . evalContT $ fRunner -- 3
  flip runReaderT ref . flip runStateT "klm" . evalContT $ fRunner -- 4-}

f0 :: IO ((), T.Text)
f0 = do
  ref <- newIORef TState2{tResume2 = Nothing, tResult2 = (Nothing :: Maybe (A.Result Int)), tInt2 = 0}
  flip runReaderT ref . flip runStateT "abc" . evalContT $ fRunner -- 1
  flip runReaderT ref . flip runStateT "abc" . evalContT $ fRunner -- 2
  flip runReaderT ref . flip runStateT "def" . evalContT $ fRunner -- 2
  flip runReaderT ref . flip runStateT "def" . evalContT $ fRunner -- 2
{-  TState2{..} <- liftIO (readIORef ref)
  let res = (maybe (error "Netu parsera") A.feed (join $ cast tResult2) "def") :: A.Result T.Text
  liftIO . print $ A.feed res ""-}
  return ((), "")
{-  flip runReaderT ref . flip runStateT "abc" . evalContT $ fRunner -- 2
  flip runReaderT ref . flip runStateT "def" . evalContT $ fRunner -- 3-}

fRunner :: ContT () (StateT T.Text (ReaderT (IORef TState2) IO)) ()
fRunner = do
    tRef <- ask
    st <- liftIO (readIORef tRef)
    let n = tInt2 st
    liftIO $ print $ "Runner: " <> show n
    case st of
      TState2 Nothing  _ _ -> liftIO (putStrLn "Start cmd.")  >> prog3
      TState2 (Just c) _ _ -> liftIO (putStrLn "Continue cmd.") >> lift (c ())

{-prog2 :: ContT () (StateT T.Text (ReaderT (IORef TState2) IO)) ()
prog2 = shiftT f1 >> shiftT f2 >> shiftT f3 >>= liftIO . print-}

prog3 :: ContT () (StateT T.Text (ReaderT (IORef TState2) IO)) ()
prog3 = fParse (Just <$> A.string "abcdef") >>= \x -> liftIO (print $ "final result " <> show x)

f1 :: (() -> StateT T.Text (ReaderT (IORef TState2) IO) ())
      -> ContT () (StateT T.Text (ReaderT (IORef TState2) IO)) ()
f1 k = do
  let i = 1
  ts <- get
  liftIO $ print $ "a: " <> show ts
  --put (T.tail ts)
  ref <- ask
  liftIO $ atomicModifyIORef' ref (\s -> (s{tResume2 = Just k, tInt2 = i}, ()))

f2 :: (() -> StateT T.Text (ReaderT (IORef TState2) IO) ())
      -> ContT () (StateT T.Text (ReaderT (IORef TState2) IO)) ()
f2 k = do
  let i = 2
  ts <- get
  liftIO $ print $ "b: " <> show ts
  --put (T.tail ts)
  ref <- ask
  liftIO $ atomicModifyIORef' ref (\s -> (s{tResume2 = Just k, tInt2 = i}, ()))

{-f3 :: (() -> StateT T.Text (ReaderT (IORef TState2) IO) ())
      -> ContT () (StateT T.Text (ReaderT (IORef TState2) IO)) ()
f3 k = do
  let i = 3
  ts <- get
  liftIO $ print $ "c: " <> show ts
  --put (T.tail ts)
  ref <- ask
  liftIO $ atomicModifyIORef' ref (\s -> (s{tResume2 = Just k, tInt2 = i}, ()))-}

fParse :: (Typeable c, Show c) => A.Parser c -> ContT () (StateT T.Text (ReaderT (IORef TState2) IO)) c
fParse p = shiftT $ \k -> do
  shiftT f1
  ts <- get
  liftIO $ print $ "x: " <> show ts
  ref <- ask
  TState2{..} <- liftIO (readIORef ref)
  let res = maybe (A.parse p) A.feed (tResult2 >>= cast) ts
  --liftIO . print $ A.feed res ""
  liftIO $ atomicModifyIORef' ref (\TState2{..} -> (TState2{tResult2 = Just res, tInt2 = tInt2 + 1, ..}, ()))
  --liftIO $ atomicModifyIORef' ref (\s -> (s{tResult2 = Just res, tInt2 = i}, ()))
  case res of
    A.Partial _ -> liftIO $ putStrLn "Partial result.."
    A.Fail i xs err -> error $ "Naebnulos vse: " <> T.unpack i <> concat xs <> err
    A.Done unparsedTxt r -> do
      liftIO $ putStrLn "Finished output parsing"
      liftIO $ print $ "Unparsed text left: '" <> unparsedTxt <> "'"
      shiftT f2
      lift (k r)

data TResult3 = TResult3 { tString :: First (A.Result T.Text), tMaybe :: First (A.Result Int)}
  deriving (Show)

tResult3Str :: LensC TResult3 (Maybe (A.Result T.Text))
tResult3Str g z@TResult3{..} = (\x' -> z{tString = First x'}) <$> g (getFirst tString)

tResult3M :: LensC TResult3 (Maybe (A.Result Int))
tResult3M g z@TResult3{..} = (\x' -> z{tMaybe = First x'}) <$> g (getFirst tMaybe)

instance Semigroup TResult3 where
  x <> y = x{tString = tString x <> tString y, tMaybe = tMaybe x <> tMaybe y}

instance Monoid TResult3 where
  mempty = TResult3{tString = First Nothing, tMaybe = First Nothing}
  mappend = (<>)

-- FIXME: Write 'any type' result holder for use with cast, like in prog3.

f0' :: IO ((), T.Text)
f0' = do
  ref <- newIORef TState3{tResume3 = Nothing, tResult3 = mempty, tInt3 = 0}
  flip runReaderT ref . flip runStateT "abc" . evalContT $ fRunner' -- f3
  flip runReaderT ref . flip runStateT "abc" . evalContT $ fRunner' -- parse str
  flip runReaderT ref . flip runStateT "def" . evalContT $ fRunner' -- finish parse
  flip runReaderT ref . flip runStateT "xyz" . evalContT $ fRunner' -- f3
  flip runReaderT ref . flip runStateT "1234" . evalContT $ fRunner' -- parse int
  flip runReaderT ref . flip runStateT " " . evalContT $ fRunner' -- finish parse
  flip runReaderT ref . flip runStateT "huynya" . evalContT $ fRunner' -- f3
  return ((), "")

fRunner' :: ContT () (StateT T.Text (ReaderT (IORef (TState3 TResult3)) IO)) ()
fRunner' = do
    tRef <- ask
    st <- liftIO (readIORef tRef)
    let n = tInt3 st
    liftIO $ print $ "Runner: " <> show n
    case st of
      TState3 Nothing  _ _ -> liftIO (putStrLn "Start cmd.")  >> prog3'
      TState3 (Just c) _ _ -> liftIO (putStrLn "Continue cmd.") >> lift (c ())

prog3' :: ContT () (StateT T.Text (ReaderT (IORef (TState3 TResult3)) IO)) ()
prog3' = do
  x <- fParse3 tResult3Str (A.string "abcdef")
  liftIO (print $ "final string " <> show x)
  x <- fParse3 tResult3M (A.decimal)
  liftIO (print $ "final number " <> show x)

f3 :: (() -> StateT T.Text (ReaderT (IORef (TState3 c)) IO) ())
      -> ContT () (StateT T.Text (ReaderT (IORef (TState3 c)) IO)) ()
f3 k = do
  let i = 1
  ts <- get
  liftIO $ print $ "f3: " <> show ts
  ref <- ask
  liftIO $ atomicModifyIORef' ref (\s -> (s{tResume3 = Just k, tInt3 = i}, ()))

fParse3 ::
  (Monoid c, Show d) =>
  LensC c (Maybe (A.Result d))
  -> A.Parser d
  -> ContT () (StateT T.Text (ReaderT (IORef (TState3 c)) IO)) d
fParse3 l p = shiftT $ \k -> do
  shiftT f3
  ts <- get
  liftIO $ print $ "x3: " <> show ts
  ref <- ask
  TState3{..} <- liftIO (readIORef ref)
  let res = maybe (A.parse p) (A.feed) (getL l tResult3) ts
  --liftIO . print $ A.feed res ""
  liftIO $ atomicModifyIORef' ref (\TState3{..} ->
    (TState3
      { tResult3 = setL l (Just res) tResult3
      , tInt3 = tInt3 + 1
      , ..}
    , ()
    )
   )
  case res of
    A.Partial _ -> liftIO $ putStrLn "Partial result.."
    A.Fail i xs err -> error $ "Naebnulos vse: " <> show res
    A.Done unparsedTxt r -> do
      liftIO $ putStrLn "Finished output parsing"
      liftIO $ print $ "Unparsed text left: '" <> unparsedTxt <> "'"
      shiftT f3
      lift (k r)

data TResult3T = forall c. TResult3T {tRes3T :: Maybe (A.Result c)}
  deriving (Typeable)

instance Semigroup TResult3T where
  z@TResult3T{tRes3T = Just x} <> _ = z
  TResult3T{tRes3T = Nothing} <> z = z

instance Monoid TResult3T where
  mempty = TResult3T{tRes3T = Nothing}
  mappend = (<>)

f0'T :: IO ((), T.Text)
f0'T = do
  ref <- newIORef TState3{tResume3 = Nothing, tResult3 = mempty, tInt3 = 0}
  flip runReaderT ref . flip runStateT "abc" . evalContT $ fRunner'T -- f3
  flip runReaderT ref . flip runStateT "abc" . evalContT $ fRunner'T -- parse str
  flip runReaderT ref . flip runStateT "def" . evalContT $ fRunner'T -- finish parse
  return ((), "")

fRunner'T :: ContT () (StateT T.Text (ReaderT (IORef (TState3 TResult3T)) IO)) ()
fRunner'T = do
    tRef <- ask
    st <- liftIO (readIORef tRef)
    let n = tInt3 st
    liftIO $ print $ "Runner: " <> show n
    case st of
      TState3 Nothing  _ _ -> liftIO (putStrLn "Start cmd.")  >> prog3'T
      TState3 (Just c) _ _ -> liftIO (putStrLn "Continue cmd.") >> lift (c ())

prog3'T :: ContT () (StateT T.Text (ReaderT (IORef (TState3 TResult3T)) IO)) ()
prog3'T = do
  x <- fParse3T (A.string "abcdef")
  liftIO (print $ "final string " <> show x)
  x <- fParse3T (A.decimal)
  liftIO (print $ "final number " <> show (x :: Int))

fParse3T ::
  (Monoid c, Typeable c, Show d, Typeable d) =>
  A.Parser d
  -> ContT () (StateT T.Text (ReaderT (IORef (TState3 c)) IO)) d
fParse3T p = shiftT $ \k -> do
  shiftT f3
  ts <- get
  liftIO $ print $ "x3: " <> show ts
  ref <- ask
  TState3{..} <- liftIO (readIORef ref)
  let res = maybe (A.parse p) (A.feed) (join $ cast tResult3) ts
  --liftIO . print $ A.feed res ""
  liftIO $ atomicModifyIORef' ref (\TState3{..} ->
    (TState3
      { tResult3 = fromMaybe mempty (cast (Just res))
      , tInt3 = tInt3 + 1
      , ..}
    , ()
    )
   )
  case res of
    A.Partial _ -> liftIO $ putStrLn "Partial result.."
    A.Fail i xs err -> error $ "Naebnulos vse: " <> show res
    A.Done unparsedTxt r -> do
      liftIO $ putStrLn "Finished output parsing"
      liftIO $ print $ "Unparsed text left: '" <> unparsedTxt <> "'"
      shiftT f3
      lift (k r)

newtype IP = IP String
  deriving (Show, Eq, Ord)

instance ToJSON IP where
    toJSON (IP x) = toJSON x
instance ToJSONKey IP where
    toJSONKey = ToJSONKeyText (\(IP x) -> T.pack x) (\(IP x) -> JE.text . T.pack $ x)

newtype Mac = Mac String
  deriving (Show, Eq, Ord)

instance ToJSON Mac where
    toJSON (Mac x) = toJSON x
instance ToJSONKey Mac where
    toJSONKey = ToJSONKeyText (\(Mac x) -> T.pack x) (\(Mac x) -> JE.text . T.pack $ x)

newtype Port = Port String
  deriving (Show, Eq, Ord)

instance ToJSON Port where
    toJSON (Port x) = toJSON x
instance ToJSONKey Port where
    toJSONKey = ToJSONKeyText (\(Port x) -> T.pack x) (\(Port x) -> JE.text . T.pack $ x)

tIP :: Col IP
tIP = IPtoMac
  $ M.fromList
      [ (IP "ip1", (First (Just (Mac "mac1")), First (Just (Port "port1"))))
      , (IP "ip2", (First (Just (Mac "mac2")), First (Just (Port "port2"))))
      ]

tMac :: Col Mac
tMac = MacToIP
  $ M.fromList
      [ (Mac "mac1", (First (Just (IP "ip1")), First (Just (Port "port1"))))
      , (Mac "mac2", (First (Just (IP "ip2")), First (Just (Port "port2"))))
      ]

tPort :: Col Port
tPort = PortToMac
  $ M.fromList
      [ (Port "port1", (First (Just (Mac "mac1")), First (Just (IP "ip1"))))
      , (Port "port2", (First (Just (Mac "mac2")), First (Just (IP "ip2"))))
      ]


p :: StateT (Col IP, Col Mac, Col Port) IO ()
p = do
  let tIP1 = updateIP (IP "ip3", Mac "mac3") tMac tIP
  liftIO $ showT tIP1
  liftIO $ showT tMac
  liftIO $ showT tPort

{-updateAll :: (IP, Mac) -> (Col IP, Col Mac, Col Port) -> (Col IP, Col Mac, Col Port) 
updateAll (im, mm, pm) =-}

data N = S N | Z

data CK

class ColKey k where
  data Col k
  type ColRef k
  --type ColRef2 k :: * -> *
  updateIP :: (IP, Mac) -> ColRef k -> Col k -> Col k
  --updateIP2 :: (IP, Mac) -> Col k -> ColRef k
  removeIP :: IP -> Col IP -> Col k -> Col k
  updatePort :: (Port, Mac) -> Col Mac -> Col k -> Col k

instance (ColKey IP, ColKey Mac) => ColKey (IP, Mac) where
  data Col (IP, Mac) = IPM (Col IP, Col Mac)
  type ColRef (IP, Mac) = Void
  updateIP (ip, mac) _ (IPM (im, mm)) =
    IPM (updateIP (ip, mac) mm im, updateIP (ip, mac) (undefined :: Void) mm)

instance (ColKey IP, ColKey Mac, ColKey Port) => ColKey (IP, Mac, Port) where
  data Col (IP, Mac, Port) = IPMP (Col IP, Col Mac, Col Port)
  type ColRef (IP, Mac, Port) = Void
  updateIP (ip, mac) _ (IPMP (im, mm, pm)) =
    let mm' = updateIP (ip, mac) (undefined :: Void) mm
    in  IPMP
          ( updateIP (ip, mac) mm' im
          , mm'
          , updateIP (ip, mac) mm' pm
          )

instance ColKey IP where
  data Col IP = IPtoMac (M.Map IP (First Mac, First Port))
  type ColRef IP = Col Mac
  updateIP (ip, mac) (MacToIP mm) (IPtoMac im) =
    IPtoMac $
      M.insertWith (<>) ip
        (First (Just mac), First $ M.lookup mac mm >>= getFirst . snd)
        im
  --updateIP' (ip, mac) (MacToIP mm) (IPtoMac im) = undefined

  removeIP ip _ (IPtoMac im) = IPtoMac (M.delete ip im)

  updatePort (port, mac) (MacToIP mm) (IPtoMac im) =
    IPtoMac . fromMaybe im $ do
      ip <- M.lookup mac mm >>= getFirst . fst
      return $ M.insertWith (<>) ip
        (First (Just mac), First (Just port))
        im

deriving instance Show (Col IP)
instance ToJSON (Col IP) where
  toJSON (IPtoMac x) = toJSON x

instance ColKey Mac where
  data Col Mac = MacToIP (M.Map Mac (First IP, First Port))
  type ColRef Mac = Void
  updateIP (ip, mac) _ (MacToIP mm) =
    MacToIP $
      M.insertWith (<>) mac
        (First (Just ip), mempty)
        mm
  --updateIP' (ip, mac) (IPtoMac mm) (MacToIP im) = undefined

  removeIP ip (IPtoMac im) (MacToIP mm) =
    MacToIP . fromMaybe mm $ do
      mac <- M.lookup ip im >>= getFirst . fst
      return $ M.delete mac mm

  updatePort (port, mac) _ (MacToIP mm) =
    MacToIP $
      M.insertWith (<>) mac
        (mempty, First (Just port))
        mm

deriving instance Show (Col Mac)
instance ToJSON (Col Mac) where
  toJSON (MacToIP x) = toJSON x

instance ColKey Port where
  data Col Port = PortToMac (M.Map Port (First Mac, First IP))
  type ColRef Port = Col Mac
  updateIP (ip, mac) (MacToIP mm) (PortToMac pm) =
    PortToMac . fromMaybe pm $ do
      port <- M.lookup mac mm >>= getFirst . snd
      return $ M.insertWith (<>) port
        (First (Just mac), First (Just ip))
        pm
  --updateIP' (ip, mac) (IPtoMac mm) (PortToMac im) = undefined

  removeIP ip (IPtoMac im) (PortToMac pm) =
    PortToMac . fromMaybe pm $ do
      port <- M.lookup ip im >>= getFirst . snd
      return $ M.delete port pm

  updatePort (port, mac) (MacToIP mm) (PortToMac pm) =
    PortToMac $
      M.insertWith (<>) port
        (First (Just mac), First $ M.lookup mac mm >>= getFirst . fst)
        pm

deriving instance Show (Col Port)
instance ToJSON (Col Port) where
  toJSON (PortToMac x) = toJSON x

showT :: ToJSON a => a -> IO ()
showT = B.putStr . Y.encode

data Key  = KeyPair Int String
          | KeyInt Int
          | KeyStr String
  deriving(Show)

instance Eq Key where
  KeyPair x s == KeyPair y p = x == y && s == p
  KeyInt x    == KeyInt y = x == y
  KeyStr s == KeyStr p = s == p
  KeyInt _ == KeyStr _ = False
  KeyStr _ == KeyInt _ = False
  KeyPair x _ == KeyInt y = x == y
  KeyInt x == KeyPair y _ = x == y
  KeyPair _ s == KeyStr p = s == p
  KeyStr s == KeyPair _ p = s == p

instance Ord Key where
  KeyPair x s `compare` KeyPair y p =
    if x `compare` y == EQ
      then s `compare` p
      else x `compare` y
  KeyInt x `compare` KeyInt y = x `compare` y
  KeyStr s `compare` KeyStr p = s `compare` p
  KeyInt _ `compare` KeyStr _ = GT
  KeyStr _ `compare` KeyInt _ = LT
  KeyPair x _ `compare` KeyInt y = x `compare` y
  KeyInt x `compare` KeyPair y _ = x `compare` y
  KeyPair _ s `compare` KeyStr p = s `compare` p
  KeyStr s `compare` KeyPair _ p = s `compare` p

tk :: M.Map Key String
tk = M.fromList
  [ (KeyPair 1 "one", "1111")
  , (KeyPair 2 "two", "2222")
  , (KeyPair 3 "three", "3333")
  , (KeyPair 4 "four", "4444")
  ]
