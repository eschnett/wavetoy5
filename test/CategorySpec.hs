{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module CategorySpec where

import Test.QuickCheck
import Test.QuickCheck.Poly

import Category



prop_FId_id :: A -> Property
prop_FId_id x = chase (FId @Hask) x === x

prop_FCompose_compose :: Fun B C -> Fun A B -> A -> Property
prop_FCompose_compose (Fn f) (Fn g) x = chase (FCompose f g) x === (f . g) x

prop_FCompose_left_id :: Fun A B -> A -> Property
prop_FCompose_left_id (Fn f) x = chase (FId `FCompose` f) x === chase f x

prop_FCompose_right_id :: Fun A B -> A -> Property
prop_FCompose_right_id (Fn f) x = chase (f `FCompose` FId) x === chase f x

prop_FCompose_assoc :: Fun C A -> Fun B C -> Fun A B -> A -> Property
prop_FCompose_assoc (Fn f) (Fn g) (Fn h) x =
    chase ((f `FCompose` g) `FCompose` h) x ===
    chase (f `FCompose` (g `FCompose` h)) x

prop_Hask_embed :: Fun A B -> A -> Property
prop_Hask_embed (Fn f) x = (discretize . chase) f x === f x

prop_Hask_project :: Fun A B -> A -> Property
prop_Hask_project (Fn f) x =
    (chase . discretize @(->) . chase . discretize @(->)) f x ===
    (chase . discretize @(->)) f x
