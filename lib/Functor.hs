module Functor ( FEndo(..)
               , Functor(..)
               , Foldable(..)
               , Apply(..)
               , Applicative(..)
               , (<$>)
               , Traversable(..)
               , Distributive(..)
               ) where

import Prelude hiding ( Functor(..)
                      , Foldable(..)
                      , Applicative(..)
                      , (<$>)
                      , Traversable(..)
                      )
import qualified Prelude as P

import Data.Functor.Const
import Data.Functor.Identity
import Data.Proxy
import Data.Semigroup

import Category



newtype FEndo m a = FEndo { appFEndo :: m a a }
instance (Morphism m, FunCat m a) => Semigroup (FEndo m a) where
    FEndo f <> FEndo g = FEndo (fcompose f g)
instance (Morphism m, FunCat m a) => Monoid (FEndo m a) where
    mempty = FEndo fid
    mappend = (<>)



class (Function m, Function n) => Functor f m n where
    fmap :: (FunCat m a, FunCat m b) => m a b -> n (f a) (f b)

class Functor f m n => Foldable f m n where
    -- foldMap :: (FunCat m a, FunCat m b, Monoid b) => m a b -> f a -> b
    foldMap :: (FunCat m a, Monoid b) => (a -> b) -> f a -> b
    --TODO fold :: (Monoid a) => f a -> a
    --TODO default fold ::
    --TODO     forall a. (Foldable f (FIdentity (FunCat m)), FunCat m a, Monoid a)
    --TODO                => f a -> a
    --TODO fold = foldMap (FIdentity :: FIdentity (FunCat m) a a)
    --TODO default fold :: (FunCat m a, Monoid a) => f a -> a
    --TODO fold = foldMap id
--TODO foldl :: Foldable f m n => (b -> a -> b) -> b -> f a -> b
--TODO foldl f z xs = appEndo (getDual (foldMap (Dual . Endo . flip f) xs)) z
--TODO foldr :: Foldable f (->) => (a -> b -> b) -> b -> f a -> b
--TODO foldr f z xs = appEndo (foldMap (Endo . f) xs) z

class Functor f m n => Apply f m n where
    -- liftF2 :: (FunCat m a, FunCat m b, FunCat m c)
    --           => m (a, b) c -> n (f a, f b) (f c)
    liftF2 :: (FunCat m a, FunCat m b, FunCat m c)
              => m a (m b c) -> n (f a) (n (f b) (f c))

class Apply f m n => Applicative f m n where
    -- pure :: FunCat m a => m () a -> n () (f a)
    pure :: FunCat m a => (Proxy m, Proxy n) -> a -> f a
    liftA2 :: (FunCat m a, FunCat m b, FunCat m c)
              => m a (m b c) -> n (f a) (n (f b) (f c))
    liftA2 = liftF2

class Functor f m m => Traversable f m where
    mapTraverse :: Applicative g m m => m (f b) c -> m a (g b) -> f a -> g c
    -- traverse :: Applicative g m m => (a -> g b) -> f a -> g (f b)
    -- traverse = mapTraverse FIdentity
    -- sequenceA :: Applicative g m m => f (g a) -> g (f a)
    -- sequenceA = mapTraverse FIdentity FIdentity
infixl 4 <$>
(<$>) :: (Functor f m n, FunCat m a, FunCat m b) => m a b -> n (f a) (f b)
(<$>) = fmap

class Functor f m m => Distributive f m where
    cotraverseMap :: Foldable g m m => (g b -> c) -> (a -> f b) -> g a -> f c
    -- collect :: Functor g m m => (a -> f b) -> g a -> f (g b)
    -- collect = cotraverseMap FIdentity
    -- distribute :: Functor g m m => g (f a) -> f (g a)
    -- distribute = cotraverseMap FIdentity FIdentity



-- Proxy

instance Functor Proxy (->) (->) where
    fmap = P.fmap
    -- fmap _ = \Proxy -> Proxy

instance Foldable Proxy (->) (->) where
    foldMap = P.foldMap

instance Apply Proxy (->) (->) where
    liftF2 f Proxy Proxy = Proxy

instance Applicative Proxy (->) (->) where
    pure _ x = Proxy

instance Traversable Proxy (->) where
    mapTraverse f g Proxy = f <$> pure (Proxy @(->), Proxy @(->)) Proxy

instance Distributive Proxy (->) where
    cotraverseMap f g _ = Proxy

-- Const a

instance Functor (Const a) (->) (->) where
    fmap = P.fmap

instance Foldable (Const a) (->) (->) where
    foldMap = P.foldMap

instance Semigroup a => Apply (Const a) (->) (->) where
    liftF2 f (Const a) (Const b) = Const (a <> b)

instance (Monoid a, Semigroup a) => Applicative (Const a) (->) (->) where
    pure _ x = Const mempty

instance Traversable (Const a) (->) where
    mapTraverse f g (Const a) = f <$> pure (Proxy @(->), Proxy @(->)) (Const a)

-- Identity

instance Functor Identity (->) (->) where
    fmap = P.fmap
    -- fmap f = \(Identity x) -> Identity (f x)

instance Foldable Identity (->) (->) where
    foldMap = P.foldMap

instance Apply Identity (->) (->) where
    liftF2 f (Identity x) (Identity y) = Identity (f x y)

instance Applicative Identity (->) (->) where
    pure _ x = Identity x

instance Traversable Identity (->) where
    mapTraverse f g (Identity x) = f . Identity <$> g x

instance Distributive Identity (->) where
    cotraverseMap f g xs = (Identity . f) ((runIdentity . g) <$> xs)

-- Maybe

instance Functor Maybe (->) (->) where
    fmap = P.fmap
    -- fmap f = \case
    --          Nothing -> Nothing
    --          Just x -> Just (f x)

instance Foldable Maybe (->) (->) where
    foldMap = P.foldMap

instance Apply Maybe (->) (->) where
    liftF2 f Nothing _ = Nothing
    liftF2 f _ Nothing = Nothing
    liftF2 f (Just x) (Just y) = Just (f x y)

instance Applicative Maybe (->) (->) where
    pure _ x = Just x

instance Traversable Maybe (->) where
    mapTraverse f g Nothing = f <$> pure (Proxy @(->), Proxy @(->)) Nothing
    mapTraverse f g (Just x) = f . Just <$> g x

-- Either a

instance Functor (Either a) (->) (->) where
    fmap = P.fmap
    -- fmap f = \case
    --          Left a -> Left a
    --          Right x -> Right (f x)

instance Foldable (Either a) (->) (->) where
    foldMap = P.foldMap

instance Apply (Either a) (->) (->) where
    liftF2 f (Left a) _ = Left a
    liftF2 f _ (Left b) = Left b
    liftF2 f (Right x) (Right y) = Right (f x y)

instance Applicative (Either a) (->) (->) where
    pure _ x = Right x

instance Traversable (Either a) (->) where
    mapTraverse f g (Left a) = f <$> pure (Proxy @(->), Proxy @(->)) (Left a)
    mapTraverse f g (Right x) = f . Right <$> g x

-- (,) a

instance Functor ((,) a) (->) (->) where
    fmap = P.fmap
    -- fmap f = \(t, x) -> (t, f x)

instance Foldable ((,) a) (->) (->) where
    foldMap = P.foldMap

instance Semigroup a => Apply ((,) a) (->) (->) where
    liftF2 f (a, x) (b, y) = (a <> b, f x y)

instance (Monoid a, Semigroup a) => Applicative ((,) a) (->) (->) where
    pure _ x = (mempty, x)

instance Traversable ((,) a) (->) where
    mapTraverse f g (a, x) = f <$> (,) a <$> g x

-- []

instance Functor [] (->) (->) where
    fmap = P.fmap
    -- fmap f = \case
    --          [] -> []
    --          x : xs -> f x : fmap f xs

instance Foldable [] (->) (->) where
    foldMap = P.foldMap

-- TODO: This is ZipList, use List instead
instance Apply [] (->) (->) where
    liftF2 f (x:xs) (y:ys) = f x y : liftF2 f xs ys
    liftF2 f _ _ = []

instance Applicative [] (->) (->) where
    pure _ x = repeat x

--TODO instance Traversable [] (->) where
--TODO     mapTraverse f g = f <$> foldr cons_f (pure [])
--TODO         where cons_f x ys = liftA2 (:) (f x) ys

-- (->) a

instance Functor ((->) a) (->) (->) where
    fmap = P.fmap
    -- fmap f = \ff -> f . ff
