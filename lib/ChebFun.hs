{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module ChebFun(ChebFun(..)) where

import qualified Data.Vector.Generic as G
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Instances ()
import Text.Read

import Category
import Chebyshev



-- TODO: Turn this into a type
ncoeffs :: Int
ncoeffs = 5

-- TODO: Store as b
newtype ChebFun v a b = ChebFun (v Double)
instance G.Vector v Double => Eq (ChebFun v a b) where
    ChebFun cs1 == ChebFun cs2 = G.eq cs1 cs2
instance G.Vector v Double => Ord (ChebFun v a b) where
    compare (ChebFun cs1) (ChebFun cs2) = G.cmp cs1 cs2
instance G.Vector v Double => Read (ChebFun v a b) where
    readPrec = ChebFun <$> G.readPrec
instance G.Vector v Double => Show (ChebFun v a b) where
    showsPrec d (ChebFun cs) = G.showsPrec d cs

instance (G.Vector v Double, QC.Arbitrary (v Double))
        => QC.Arbitrary (ChebFun v a b) where
    -- arbitrary = ChebFun <$> QC.arbitrary
    -- shrink (ChebFun cs) = ChebFun <$> QC.shrink cs
    arbitrary = ChebFun <$> G.generateM ncoeffs (const QC.arbitrary)
    shrink (ChebFun cs) = []

eval :: (G.Vector v Double, Numeric a, Numeric b) => ChebFun v a b -> a -> b
eval (ChebFun cs) x = realToFrac (chebyshev (realToFrac (compactify x)) cs)

approx :: (G.Vector v Double, Numeric a, Numeric b)
          => Int -> (a -> b) -> ChebFun v a b
approx n f =
    ChebFun (chebyshevApprox n (realToFrac . f . uncompactify . realToFrac))

compactify :: Floating a => a -> a
compactify x = (2 / pi) * atan x

uncompactify :: Floating a => a -> a
uncompactify x = tan ((pi / 2) * x)

instance G.Vector v Double => Function (ChebFun v) where
    type FunCat (ChebFun v) = Numeric
    chase = eval

instance G.Vector v Double => Discretization (ChebFun v) where
    discretize = approx ncoeffs
