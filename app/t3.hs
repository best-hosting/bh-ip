{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Options.Applicative as O
import Control.Monad.Trans.Cont
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Text as T
import Data.IORef
import System.IO.Unsafe
import qualified Data.Attoparsec.Text as A
import Data.Typeable

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

prog2 :: ContT () (StateT T.Text (ReaderT (IORef TState2) IO)) ()
prog2 = shiftT f1 >> shiftT f2 >> shiftT f3 >>= liftIO . print

prog3 :: ContT () (StateT T.Text (ReaderT (IORef TState2) IO)) ()
prog3 = fParse (A.string "abcdef") >>= \x -> liftIO (print $ "final result " <> show x)

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

f3 :: (() -> StateT T.Text (ReaderT (IORef TState2) IO) ())
      -> ContT () (StateT T.Text (ReaderT (IORef TState2) IO)) ()
f3 k = do
  let i = 3
  ts <- get
  liftIO $ print $ "c: " <> show ts
  --put (T.tail ts)
  ref <- ask
  liftIO $ atomicModifyIORef' ref (\s -> (s{tResume2 = Just k, tInt2 = i}, ()))

fParse :: (Typeable c, Show c) => A.Parser c -> ContT () (StateT T.Text (ReaderT (IORef TState2) IO)) c
fParse p = shiftT $ \k -> do
  shiftT f1
  ts <- get
  liftIO $ print $ "x: " <> show ts
  ref <- ask
  TState2{..} <- liftIO (readIORef ref)
  let res = maybe (A.parse p) A.feed (join $ cast tResult2) ts
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

