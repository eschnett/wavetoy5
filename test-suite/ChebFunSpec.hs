{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module ChebFunSpec where

import qualified Data.Vector.Unboxed as U
import Test.QuickCheck

import Category
import ChebFun



type CFun = ChebFun U.Vector

prop_ChebFun_assoc ::
    CFun Double Double -> CFun Double Double -> CFun Double Double -> Double ->
    Property
prop_ChebFun_assoc f g h x =
    chase ((f `FCompose` g) `FCompose` h) x ===
    chase (f `FCompose` (g `FCompose` h)) x

prop_ChebFun_embed :: CFun Double Double -> Double -> Property
prop_ChebFun_embed f x =
    -- ((discretize @ CFun) . chase) f === f
    chase (((discretize @ CFun) . chase) f) x ~~~ chase f x

prop_ChebFun_project :: Fun Double Double -> Double -> Property
prop_ChebFun_project (Fn f) x =
    (chase . (discretize @ CFun) . chase . (discretize @ CFun)) f x ~~~
    (chase . (discretize @ CFun)) f x



-- TODO: Move this into its own module
-- | Like '==', but allowing for round-off error
-- TODO: Use IEEE to avoid arbitrary constant
infix 4 ~~
(~~) :: (Fractional a, Ord a) => a -> a -> Bool
x ~~ y = abs (x - y) < 1.0e-13 * (1 `max` abs x `max` abs y)

-- | Like '===', but allowing for round-off error
infix 4 ~~~
(~~~) :: (Fractional a, Ord a, Show a) => a -> a -> Property
x ~~~ y =
    counterexample (show x ++ interpret res ++ show y) res
    where
      res = x ~~ y
      interpret True  = " == "
      interpret False = " /= "
