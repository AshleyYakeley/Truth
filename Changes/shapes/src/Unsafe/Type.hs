module Unsafe.Type
    ( module Unsafe.Refl
    , module Unsafe.Type
    ) where

import Data.Isomorphism
import Shapes.Import
import Unsafe.Refl

unsafeIsomorphism ::
       forall k (cat :: k -> k -> Type) (a :: k) (b :: k). Category cat
    => Isomorphism cat a b
unsafeIsomorphism = MkIsomorphism (unsafeId @k @cat @a @b) (unsafeId @k @cat @b @a)

unsafeGetIsomorphism ::
       forall k (cat :: k -> k -> Type) (a :: k) (b :: k) (m :: Type -> Type). (Category cat, Applicative m)
    => m (Isomorphism cat a b)
unsafeGetIsomorphism = pure $ unsafeIsomorphism @k @cat @a @b
