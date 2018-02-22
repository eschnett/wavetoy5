module Monad ( Monad(..)
             -- , (>>=)
             ) where

import Prelude hiding ( Functor(..)
                      , Foldable(..)
                      , Applicative(..)
                      , Monad(..)
                      , (=<<)
                      )
import qualified Prelude as P

import Category
import Functor



class Applicative f m m => Monad f m where
--     return :: FunCat m a => m a (f a)
--     return = pure
--     infixr 1 <=<
--     (<=<) :: (FunCat m a, FunCat m b, FunCat m c)
--              => m b (f c) -> m a (f b) -> m a (f c)
--     infixr 1 =<<
--     (=<<) :: forall f m a b. (FunCat m a, FunCat m b) => m a (f b) -> f a -> f b
--     f =<< x = chase (f <=< (FConst x :: FConst (FunCat m) () (f a))) ()
--     join :: FunCat m a => m (f (f a)) (f a)
-- 
-- infixr 1 >>=
-- (>>=) :: (Monad f m, FunCat m a, FunCat m b) => f a -> m a (f b) -> f b
-- (>>=) = flip (=<<)
