{-# LANGUAGE RankNTypes #-}

module BH.Common
    ( LensC
    , getL
    , modifyL
    , setL
    )
  where

import Data.Functor.Const
import Data.Functor.Identity

-- See https://www.twanvl.nl/blog/haskell/cps-functional-references .
type LensC a b   = forall f. Functor f => (b -> f b) -> a -> f a

getL :: LensC a b -> a -> b
getL l = getConst . l Const

modifyL :: LensC a b -> (b -> b) -> a -> a
modifyL l g = runIdentity . l (Identity . g)

setL :: LensC a b -> b -> a -> a
setL l x = modifyL l (const x)

