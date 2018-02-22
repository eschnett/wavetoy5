{-# LANGUAGE UndecidableInstances #-}

module Unboxed ( Unboxed
               , type (-#>)(..)
               -- , funny
               , runUFun2
               ) where

import qualified Data.Vector.Unboxed as U

import Category


    
-- This is a subcategory of Numeric
class (RealFloat a, U.Unbox a) => Unboxed a
instance (RealFloat a, U.Unbox a) => Unboxed a
instance Category Unboxed

newtype (-#>) a b = UFun { runUFun :: a -> b }
instance Function (-#>) where
    type FunCat (-#>) = Unboxed
    chase = runUFun
instance Discretization (-#>) where
    discretize = UFun

funny :: ((a -> b) -> f a -> f b) -> (a -#> b) -> f a -#> f b
funny f = UFun . f . runUFun

runUFun2 :: (a -#> (b -#> c)) -> a -> b -> c
runUFun2 f x y = runUFun (runUFun f x) y
