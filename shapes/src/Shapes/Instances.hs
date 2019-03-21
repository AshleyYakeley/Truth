{-# OPTIONS -fno-warn-orphans #-}

module Shapes.Instances
    (
    ) where

import Data.IORef
import Shapes.Import
import Unsafe.Coerce

unsafeRefl :: forall a b. a :~: b
unsafeRefl = unsafeCoerce Refl

instance TestEquality IORef where
    testEquality ta tb =
        if ta == unsafeCoerce tb
            then Just unsafeRefl
            else Nothing

instance TestEquality TVar where
    testEquality ta tb =
        if ta == unsafeCoerce tb
            then Just unsafeRefl
            else Nothing
