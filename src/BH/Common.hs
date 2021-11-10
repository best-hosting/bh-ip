{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module BH.Common
    ( LensC
    , getL
    , modifyL
    , setL
    , idL
    , maybeL
    , nothingL
    , mapL
    , fstL
    , sndL
    , lexemeA
    , symbolA
    , maybeErr
    , (<<>>)
    , anyP
    , allP
    , isWords
    , insertAdjust
    , insertAdjust2
    )
  where

import qualified Data.Text as T
import Data.Functor.Const
import Data.Functor.Identity
import qualified Data.Attoparsec.Text as A
import Control.Monad.Except
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import Data.Maybe

-- See https://www.twanvl.nl/blog/haskell/cps-functional-references .
type LensC a b   = forall f. Functor f => (b -> f b) -> a -> f a

getL :: LensC a b -> a -> b
getL l = getConst . l Const

modifyL :: LensC a b -> (b -> b) -> a -> a
modifyL l g = runIdentity . l (Identity . g)

setL :: LensC a b -> b -> a -> a
setL l x = modifyL l (const x)

maybeL :: a -> LensC (Maybe a) a
maybeL d g Nothing  = const Nothing <$> g d
maybeL _ g (Just x) = Just <$> g x

-- | Always get and set 'Nothing' (value is not changed on 'set').
nothingL :: LensC a (Maybe b)
nothingL g x = const x <$> g Nothing

toMaybeL :: LensC a (Maybe a)
toMaybeL g x  = fromMaybe undefined <$> g (Just x)

idL :: LensC a a
idL g x = g x

-- | Lens version of Map's lookup.
mapL :: Ord k => k -> LensC (M.Map k b) (Maybe b)
mapL k g x = (maybe x (\y' -> M.insert k y' x)) <$> g (M.lookup k x)

fstL :: LensC (a, b) a
fstL g (x, y) = (\x' -> (x', y)) <$> g x

sndL :: LensC (a, b) b
sndL g (x, y) = (\y' -> (x, y')) <$> g y

symbolA :: T.Text -> A.Parser T.Text
symbolA   = lexemeA . A.string

lexemeA :: A.Parser a -> A.Parser a
lexemeA p = p <* A.takeWhile A.isHorizontalSpace

maybeErr :: MonadError String m => String -> Maybe a -> m a
maybeErr err = maybe (throwError err) return

infixr 4 <<>>
(<<>>) :: (Applicative f, Monoid a) => f a -> f a -> f a
(<<>>) = liftA2 (<>)

anyP :: [a -> Bool] -> a -> Bool
anyP ps x = any ($ x) ps

allP :: [a -> Bool] -> a -> Bool
allP ps x = all ($ x) ps

isWords :: Char -> Bool
isWords = anyP [isAlpha, A.isHorizontalSpace]

-- | 'insert' /or/ 'adjust', but /not/ 'insertWith'.
insertAdjust :: Ord a => (b -> b) -> b -> a -> M.Map a b -> M.Map a b
-- FIXME: Try to build db with intentionally wrong 'insertAdjust', which
-- inserts default /without/ modifying it by function 'f'.
insertAdjust f def x = M.insertWith (const f) x (f def)

-- Wrong insertAdjust for testing.
insertAdjust2 :: Ord a => (b -> b) -> b -> a -> M.Map a b -> M.Map a b
-- FIXME: Try to build db with intentionally wrong 'insertAdjust', which
-- inserts default /without/ modifying it by function 'f'.
insertAdjust2 f def x = M.insertWith (const f) x def

