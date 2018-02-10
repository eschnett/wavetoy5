{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Category
    ( CatKind
    , Category(..)
    , FunKind
    , Function(..)
    , Discretization(..)
    , FIdentity(..)
    , FCompose(..)
    , Hask
    ) where

import Data.Kind



-- | The kind of a category. A category represents a constraint on types.
type CatKind = Type -> Constraint
-- | The kind of a function. A function maps between two types.
type FunKind = Type -> Type -> Type



-- | A category. We currently restrict categories to be subcategories
-- of Hask. We represent categories via the constraint defining its
-- objects. Different from the standard Haskell 'Category' class,
-- morphisms (functions) are not a type, but a set of types satisfying
-- a constraint 'Function'.
class Category (k :: CatKind)

-- | A function, i.e. a morphism in a category.
class Category (FunCat f) => Function (f :: FunKind) where
    -- | The category in which this function is a morphism
    type FunCat f :: CatKind
    -- | "Chase" the morphism, i.e. apply the function. This can also
    -- be interpreted as embedding the category into Hask, and
    -- converting the function to (->).
    chase :: (FunCat f a, FunCat f b) => f a b -> a -> b

-- | A function that can approximate a function in Hask.
class Function f => Discretization f where
    -- | Convert (approximate) a Hask function to this representation
    discretize :: (FunCat f a, FunCat f b) => (a -> b) -> f a b

-- | A generic identity function
data FIdentity (k :: CatKind) a b where
    FIdentity :: k a => FIdentity k a a
instance Eq (FIdentity k a b) where
    _ == _ = True
instance Ord (FIdentity k a b) where
    compare _ _ = EQ
instance k a => Read (FIdentity k a a) where
    readsPrec d r = [(FIdentity, s) | ("FIdentity", s) <- lex r]
instance Show (FIdentity k a b) where
    showsPrec d FIdentity = showString "FIdentity"
instance Category k => Function (FIdentity k) where
    type FunCat (FIdentity k) = k
    chase FIdentity = id

-- | Compose two functions. Both functions must be morphisms in the
-- same category, and their composition lives in the same category.
data FCompose f g b a c = FCompose (f b c) (g a b)
                          deriving (Eq, Ord, Read, Show)
instance (Function f, Function g, FunCat f ~ FunCat g, FunCat f b)
        => Function (FCompose f g b) where
    type FunCat (FCompose f g b) = FunCat f
    chase (FCompose f g) = chase f . chase g



-- | Hask is the category of all Haskell types.
class Hask a
instance Hask a
instance Category Hask

-- | Haskell functions live in Hask.
instance Function (->) where
    type FunCat (->) = Hask
    chase = id
-- | Haskell functions live in Hask.
instance Discretization (->) where
    discretize = id
