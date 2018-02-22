-- | Adapted from [math-functions-0.2.1.0] Numeric.Polynomial.Chebyshev
module Chebyshev (chebyshev, chebyshevApprox, chebyshevApprox') where

import qualified Data.Vector.Generic as G



-- $chebyshev
--
-- A Chebyshev polynomial of the first kind is defined by the
-- following recurrence:
--
-- \[\begin{aligned}
-- T_0(x)     &= 1 \\
-- T_1(x)     &= x \\
-- T_{n+1}(x) &= 2xT_n(x) - T_{n-1}(x) \\
-- \end{aligned}
-- \]

-- TODO: change (a -> a) to (a -> b), allowing b to be a vector space

data C a = C !a !a

-- | Evaluate a Chebyshev polynomial of the first kind, using
-- Clenshaw's algorithm.
chebyshev ::
    (Fractional a, G.Vector v a)
    => a                        -- ^ Parameter of each function.
    -> v a -- ^ Coefficients of each polynomial term, in increasing order.
    -> a
chebyshev x cs =
    if G.null cs then 0 else fini . G.foldr' step (C 0 0) . G.tail $ cs
    where step k (C b0 b1) = C (k + x2 * b0 - b1) b0
          fini   (C b0 b1) = 0.5 * G.head cs + x * b0 - b1
          x2               = 2 * x
{-# INLINE chebyshev #-}




-- | See <http://mathworld.wolfram.com/ChebyshevApproximationFormula.html>

chebyshevApprox :: (G.Vector v a, Floating a) => Int -> (a -> a) -> v a
chebyshevApprox n = chebyshevApprox' (2 * n) n

chebyshevApprox' :: (G.Vector v a, Floating a) => Int -> Int -> (a -> a) -> v a
chebyshevApprox' n nc f = G.generate nc coeff
    where coeff j = 2 / fi n * sum [f (x k) * cheb j k | k <- [0..n-1]]
          x k = cos (pi * (fi k + 0.5) / fi n)
          cheb j k = cos (pi * fi j * (fi k + 0.5) / fi n)
          fi = fromIntegral
