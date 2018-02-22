module Vector ( Vector(..)
              , UVector(..)
              , NVector(..)
              , NUVector(..)
              ) where

import Prelude hiding ( Functor(..)
                      , Foldable(..)
                      , Applicative(..)
                      , Traversable(..))
import qualified Prelude as P

import Data.Proxy
import Data.Validity
import Data.Validity.Vector ()
import GHC.TypeLits

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Test.QuickCheck as QC

import Category
import Functor
import Unboxed



newtype Vector a = Vector (V.Vector a)
    deriving (Eq, Ord, Read, Show)

instance Functor Vector (->) (->) where
    fmap f (Vector xs) = Vector (V.map f xs)

instance Foldable Vector (->) (->) where
    foldMap f (Vector xs) = V.foldl (\r x -> r `mappend` f x) mempty xs

instance Apply Vector (->) (->) where
    -- liftF2 f (Vector xs, Vector ys) = Vector (V.zipWith (curry f) xs ys)
    liftF2 f (Vector xs) (Vector ys) = Vector (V.zipWith f xs ys)



newtype UVector a = UVector (U.Vector a)
    deriving (Eq, Ord, Read, Show)

instance Functor UVector (-#>) (->) where
    fmap f (UVector xs) = UVector (U.map (runUFun f) xs)

instance Foldable UVector (-#>) (->) where
    foldMap f (UVector xs) = U.foldl (\r x -> r `mappend` chase f x) mempty xs

instance Apply UVector (-#>) (->) where
    -- liftF2 f (UVector xs, UVector ys) =
    --     UVector (U.zipWith (curry (runUFun f)) xs ys)
    liftF2 f (UVector xs) (UVector ys) = UVector (U.zipWith (runUFun2 f) xs ys)



newtype NVector (n :: Nat) a = NVector (V.Vector a)
    deriving (Eq, Ord, Read, Show)

instance (KnownNat n, QC.Arbitrary a) => QC.Arbitrary (NVector n a) where
    arbitrary = NVector P.<$> V.generateM n' (const QC.arbitrary)
        where n' = fromInteger (natVal (Proxy @n))
    shrink (NVector xs) =
        NVector P.<$> filter ((== n') . V.length) (P.traverse QC.shrink xs)
        where n' = fromInteger (natVal (Proxy @n))

instance (KnownNat n, Validity a) => Validity (NVector n a) where
    validate (NVector xs) =
        mconcat [ V.length xs == n' <?@> "vector has correct length"
                , xs <?!> "vector"]
        where n' = fromInteger (natVal (Proxy @n))
    isValid = isValidByValidating

instance Functor (NVector n) (->) (->) where
    fmap f (NVector xs) = NVector (V.map f xs)

instance Foldable (NVector n) (->) (->) where
    foldMap f (NVector xs) = V.foldl (\r x -> r `mappend` f x) mempty xs

instance Apply (NVector n) (->) (->) where
    -- liftF2 f (NVector xs, NVector ys) = NVector (V.zipWith (curry f) xs ys)
    liftF2 f (NVector xs) (NVector ys) = NVector (V.zipWith f xs ys)

instance KnownNat n => Applicative (NVector n) (->) (->) where
    pure _ a = NVector (V.replicate n' a)
        where n' = fromInteger (natVal (Proxy @n))

-- This will work once List is Traversable
-- instance Traversable (NVector n) (->) where
--     mapTraverse f g (NVector xs) =
--         (f . NVector . V.fromList) <$> traverse g (V.toList xs)

instance KnownNat n => Distributive (NVector n) (->) where
    cotraverseMap f g xs = NVector (V.generate n' gen)
        where gen i = f (fmap (elt i . g) xs)
              elt i (NVector ys) = ys V.! i
              n' = fromInteger (natVal (Proxy @n))



newtype NUVector (n :: Nat) a = NUVector (U.Vector a)
    deriving (Eq, Ord, Read, Show)

-- instance (KnownNat n, QC.Arbitrary a, U.Unbox a)
--         => QC.Arbitrary (NUVector n a) where
--     arbitrary = NUVector <$> U.generateM n' (const QC.arbitrary)
--         where n' = fromInteger (natVal (Proxy @n))
--     shrink xs =
--         NUVector <$> filter ((== n') . V.length) (P.traverse QC.shrink xs)
--         where n' = fromInteger (natVal (Proxy @n))

-- instance (KnownNat n, Validity a, U.Unbox a) => Validity (NUVector n a) where
--     validate (NUVector xs) =
--         mconcat [ U.length x == n' <?@> "vector has correct length"
--                 , x <?!> "vector"]
--         where n' = fromInteger (natVal (Proxy @n))
--     isValid = isValidByValidating

instance Functor (NUVector n) (-#>) (->) where
    fmap f (NUVector xs) = NUVector (U.map (chase f) xs)

instance Foldable (NUVector n) (-#>) (->) where
    foldMap f (NUVector xs) = U.foldl (\r x -> r `mappend` chase f x) mempty xs

-- instance Apply (NUVector n) (-#>) (->) where
--     liftF2 f (NUVector xs, NUVector y) =
--         NUVector (U.zipWith (curry (chase f)) x y)

-- instance KnownNat n => Applicative (NUVector n) (-#>) (->) where
--     pure a = NUVector (U.replicate n' a)
--         where n' = fromInteger (natVal (Proxy @n))
