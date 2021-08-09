{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Options.Applicative as O
import Control.Monad.Trans.Cont
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Text as T
import Data.IORef
import System.IO.Unsafe

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

tRef :: IORef TState
{-# NOINLINE tRef #-}
tRef  = unsafePerformIO (newIORef TState{tResume = Nothing, tInt = 0})

g0 :: IO ((), [Int])
g0 = do
  ref <- newIORef TState{tResume = Nothing, tInt = 1}
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

