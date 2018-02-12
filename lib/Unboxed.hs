{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Unboxed (Unboxed) where

import qualified Data.Vector.Unboxed as U

import Category


    
-- This is a subcategory of Numeric
class (RealFloat a, U.Unbox a) => Unboxed a
instance (RealFloat a, U.Unbox a) => Unboxed a
instance Category Unboxed
