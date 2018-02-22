{-# LANGUAGE UndecidableInstances #-}

module Category
    ( CatKind
    , Category
    , FunKind
    , Function(..)
    , Discretization(..)
    , Morphism(..)
    , FId(..)
    , FCompose(..)
    , FConst(..)
    , FHask(..)
    , FUnit(..)
    , FCurry(..)
    , FUncurry(..)
    , Hask
    , Numeric
    ) where

-- import Prelude hiding (id, (.))
-- import qualified Prelude as P

import Data.Kind
import Text.Read



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

-- | Functions can be Morphisms by themselves
class Function f => Morphism f where
    fid :: FunCat f a => f a a
    fcompose :: (FunCat f a, FunCat f b, FunCat f c) => f b c -> f a b -> f a c

-- | The identity function
-- TODO: Rename this to 'FunId' and 'FunCompose'?
data FId (k :: CatKind) a b where
    FId :: k a => FId k a a
instance Eq (FId k a b) where
    _ == _ = True
instance Ord (FId k a b) where
    compare _ _ = EQ
instance k a => Read (FId k a a) where
    -- readsPrec d r = [(FId, s) | ("FId", s) <- lex r]
    readPrec = do Ident "FId" <- lexP
                  return FId
    readListPrec = readListPrecDefault
instance Show (FId k a b) where
    showsPrec d FId = showString "FId"
instance Category k => Function (FId k) where
    type FunCat (FId k) = k
    chase FId = id

-- | Compose two functions. Both functions must be morphisms in the
-- same category, and their composition lives in the same category.
data FCompose f g b a c = FCompose (f b c) (g a b)
                          deriving (Eq, Ord, Read, Show)
instance (Function f, Function g, FunCat f ~ FunCat g, FunCat f b)
        => Function (FCompose f g b) where
    type FunCat (FCompose f g b) = FunCat f
    chase (FCompose f g) = chase f . chase g

-- | The constant function
newtype FConst (k :: CatKind) a b = FConst b
    deriving (Eq, Ord, Read, Show)
instance Category k => Function (FConst k) where
    type FunCat (FConst k) = k
    chase (FConst x) = const x

-- | A Haskell function
newtype FHask (k :: CatKind) a b = FHask (a -> b)
instance Category k => Function (FHask k) where
    type FunCat (FHask k) = k
    chase (FHask f) = f

-- | Unit function
data FUnit (k :: CatKind) a b where
    FUnit :: b -> FUnit k () b
instance Eq b => Eq (FUnit k a b) where
    FUnit x == FUnit y = x == y
instance Ord b => Ord (FUnit k a b) where
    compare (FUnit x) (FUnit y) = compare x y
-- TODO: instance Read b => Read (FUnit k a b) where
-- TODO: Correct precedence
instance Show b => Show (FUnit k a b) where
    showsPrec d (FUnit x) = showString "FUnit " . showsPrec d x
instance Category k => Function (FUnit k) where
    type FunCat (FUnit k) = k
    chase (FUnit x) () = x

-- | Currying
-- Is FCurry a Comonad? An indexed Comonad?
--     f () a -> a
--     f (a, b) c -> f a (f b c)
data FCurry (k :: CatKind) f b c a d where
    FCurry :: f (a, b) c -> FCurry k f b c a (FCurry' k f a b c)
instance Eq (f (a, b) c) => Eq (FCurry k f b c a d) where
    FCurry f == FCurry g = f == g
instance Ord (f (a, b) c) => Ord (FCurry k f b c a d) where
    compare (FCurry f) (FCurry g) = compare f g
-- TODO: instance Read (f (a, b) c) => Read (FCurry k f b c a d) where
-- TODO: Correct precedence
instance Show (f (a, b) c) => Show (FCurry k f b c a d) where
    showsPrec d (FCurry f) = showString "FCurry " . showsPrec d f
instance Function f => Function (FCurry k f b c) where
    type FunCat (FCurry k f b c) = FunCat f
    chase (FCurry f) a = FCurry' f a

data FCurry' (k :: CatKind) (f :: FunKind) a b c = FCurry' (f (a, b) c) a
instance (Eq (f (a, b) c), Eq a) => Eq (FCurry' k f a b c) where
    FCurry' f x == FCurry' g y = f == g && x == y
instance (Ord (f (a, b) c), Ord a) => Ord (FCurry' k f a b c) where
    compare (FCurry' f x) (FCurry' g y) = compare (f, x) (g, y)
-- TODO: Read, Show
--TODO instance Function f => Function (FCurry' k f a) where
--TODO     type FunCat (FCurry' k f a) = FunCat f
--TODO     chase (FCurry' f a) b = chase f (a, b)

-- | Uncurry
-- Is FUncurry a Monad? An indexed Monad?
-- a -> f () a
-- f a (f b c) -> f (a, b) c
data FUncurry (k :: CatKind) f g a b d c where
    FUncurry :: f a (g b c) -> FUncurry k f g a b (a, b) c
-- TODO: Eq, Ord, Read, Show
--TODO instance (Function f, Function g, FunCat f ~ FunCat g)
--TODO         => Function (FUncurry k f g a b) where
--TODO     type FunCat (FUncurry k f g a b) = FunCat f
--TODO     chase (FUncurry f) (x, y) = chase (chase f x) y



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
-- | Haskell functions are morphisms.
instance Morphism (->) where
    fid = id
    fcompose = (.)



class RealFloat a => Numeric a
instance RealFloat a => Numeric a
instance Category Numeric
