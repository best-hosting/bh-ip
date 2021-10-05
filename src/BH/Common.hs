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
    , lexemeA
    , symbolA
    , maybeErr
    , (<<>>)
    , anyP
    , allP
    , isWords
    )
  where

import qualified Data.Text as T
import Data.Functor.Const
import Data.Functor.Identity
import qualified Data.Attoparsec.Text as A
import Control.Monad.Except
import Control.Applicative
import Data.Char

-- See https://www.twanvl.nl/blog/haskell/cps-functional-references .
type LensC a b   = forall f. Functor f => (b -> f b) -> a -> f a

getL :: LensC a b -> a -> b
getL l = getConst . l Const

modifyL :: LensC a b -> (b -> b) -> a -> a
modifyL l g = runIdentity . l (Identity . g)

setL :: LensC a b -> b -> a -> a
setL l x = modifyL l (const x)

maybeL :: LensC (Maybe a) a
maybeL g Nothing  = Just <$> g undefined
maybeL g (Just x) = Just <$> g x

idL :: LensC a a
idL g x = g x

-- | Always get and set 'Nothing' (value is not changed on 'set').
nothingL :: LensC a (Maybe b)
nothingL g x = (const x) <$> g Nothing

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
