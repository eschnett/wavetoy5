-- {-# LANGUAGE UndecidableInstances #-}

module ChebFun (ChebFun(..)) where

import Prelude hiding (Functor(..), (<$>))
import qualified Prelude as P

import Data.Functor.Identity
import Data.Proxy

import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

import qualified Test.QuickCheck as QC
import Test.QuickCheck.Instances ()

import Category
import Chebyshev
import Functor
import Unboxed



-- TODO: Turn this into a type
ncoeffs :: Int
ncoeffs = 5



newtype ChebFun v a b = ChebFun (v b)
    deriving (Eq, Ord, Read, Show)

instance (G.Vector v b, QC.Arbitrary b) => QC.Arbitrary (ChebFun v a b) where
    -- arbitrary = ChebFun <$> QC.arbitrary
    -- shrink (ChebFun cs) = ChebFun <$> QC.shrink cs
    arbitrary = ChebFun P.<$> G.generateM ncoeffs (const QC.arbitrary)
    shrink (ChebFun cs) = []

eval :: (G.Vector v b, Numeric a, Numeric b) => ChebFun v a b -> a -> b
eval (ChebFun cs) x = chebyshev (realToFrac (compactify x)) cs

approx :: (G.Vector v b, Numeric a, Numeric b)
          => Int -> (a -> b) -> ChebFun v a b
approx n f = ChebFun (chebyshevApprox n (f . uncompactify . realToFrac))

compactify :: Floating a => a -> a
compactify x = (2 / pi) * atan x

uncompactify :: Floating a => a -> a
uncompactify x = tan ((pi / 2) * x)



instance Function (ChebFun V.Vector) where
    type FunCat (ChebFun V.Vector) = Numeric
    chase = eval

instance Discretization (ChebFun V.Vector) where
    discretize = approx ncoeffs



instance Function (ChebFun U.Vector) where
    type FunCat (ChebFun U.Vector) = Unboxed
    chase = eval

instance Discretization (ChebFun U.Vector) where
    discretize = approx ncoeffs



type ChebFunV = ChebFun V.Vector

instance Functor (ChebFunV a) (->) (->) where
    fmap f = fmap (chase f)

instance Functor Proxy ChebFunV (->) where
    fmap f = fmap (chase f)

instance Functor Identity ChebFunV (->) where
    fmap f = fmap (chase f)

instance Functor Maybe ChebFunV (->) where
    fmap f = fmap (chase f)

instance Functor (Either a) ChebFunV (->) where
    fmap f = fmap (chase f)

instance Functor ((,) a) ChebFunV (->) where
    fmap f = fmap (chase f)

instance Functor [] ChebFunV (->) where
    fmap f = fmap (chase f)

instance Functor ((->) a) ChebFunV (->) where
    fmap f = fmap (chase f)

instance Functor (ChebFunV a) ChebFunV (->) where
    fmap f = fmap (chase f)



type ChebFunU = ChebFun U.Vector

instance Functor (ChebFunU a) (->) (->) where
    fmap f = fmap (chase f)

instance Functor Proxy ChebFunU (->) where
    fmap f = fmap (chase f)

instance Functor Identity ChebFunU (->) where
    fmap f = fmap (chase f)

instance Functor Maybe ChebFunU (->) where
    fmap f = fmap (chase f)

instance Functor (Either a) ChebFunU (->) where
    fmap f = fmap (chase f)

instance Functor ((,) a) ChebFunU (->) where
    fmap f = fmap (chase f)

instance Functor [] ChebFunU (->) where
    fmap f = fmap (chase f)

instance Functor ((->) a) ChebFunU (->) where
    fmap f = fmap (chase f)

instance Functor (ChebFunU a) ChebFunU (->) where
    fmap f = fmap (chase f)
