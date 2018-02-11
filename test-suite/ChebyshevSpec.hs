{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module ChebyshevSpec where

import Data.Fixed
import qualified Data.Vector.Unboxed as U
import Test.QuickCheck

import Chebyshev



prop_Chebyshev_zero :: Double -> Property
prop_Chebyshev_zero x =
    cpoly x ~~~ poly x
    where poly _ = 0
          coeffs = chebyshevApprox 0 poly :: U.Vector Double
          cpoly = \y -> chebyshev y coeffs

prop_Chebyshev_polynomial ::
    Positive Int -> Double -> NonNegative Int -> Double -> Property
prop_Chebyshev_polynomial (Positive n') a' (NonNegative i') x' =
    cpoly x ~~~ poly x
    where n = mod (n' - 1) 10 + 1
          a = mod' a' 1
          i = mod i' n
          x = mod' x' 1.01
          poly y = a * y ^ i
          coeffs = chebyshevApprox n poly :: U.Vector Double
          cpoly = \y -> chebyshev y coeffs



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
