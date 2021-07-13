{-# LANGUAGE RankNTypes #-}

module BH.Common
    ( LensC
    , getL
    , modifyL
    , setL
    , lexemeA
    , symbolA
    )
  where

import qualified Data.Text as T
import Data.Functor.Const
import Data.Functor.Identity
import qualified Data.Attoparsec.Text as A

-- See https://www.twanvl.nl/blog/haskell/cps-functional-references .
type LensC a b   = forall f. Functor f => (b -> f b) -> a -> f a

getL :: LensC a b -> a -> b
getL l = getConst . l Const

modifyL :: LensC a b -> (b -> b) -> a -> a
modifyL l g = runIdentity . l (Identity . g)

setL :: LensC a b -> b -> a -> a
setL l x = modifyL l (const x)

symbolA :: T.Text -> A.Parser T.Text
symbolA   = lexemeA . A.string

lexemeA :: A.Parser a -> A.Parser a
lexemeA p = p <* A.takeWhile A.isHorizontalSpace

