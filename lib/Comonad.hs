module Comonad ( Comonad(..)
               , ComonadStore(..)
               ) where

import Prelude hiding ( Functor(..)
                      , Foldable(..)
                      , Applicative(..)
                      )
import qualified Prelude as P

import Category
import Functor



class Functor f m m => Comonad f m where
    extract :: FunCat m a => m (f a) a
    extend :: (FunCat m a, FunCat m b) => m (f a) b -> m (f a) (f b)
    -- duplicate :: FunCat m a => f a -> f (f a)
    -- duplicate = extend FIdentity

class Comonad f m => ComonadStore s f m | f -> s where
    pos :: FunCat m a => f a -> s
    peek :: FunCat m a => s -> f a -> a

    -- peeks :: FunCat m a => (s -> s) -> f a -> a
    -- peeks f w = peek (f (pos w)) w

    -- seek :: FunCat m a => s -> f a -> f a
    -- seek s = peek s . duplicate

    -- seeks :: FunCat m a => (s -> s) -> f a -> f a
    -- seeks f = peeks f . duplicate

    -- experiment :: (Functor g m n, FunCat m a) => (s -> g s) -> f a -> g a
    -- experiment f w = fmap (`peek` w) (f (pos w))
