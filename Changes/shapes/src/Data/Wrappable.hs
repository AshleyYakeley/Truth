module Data.Wrappable where

import Data.Coerce.Coercion
import Shapes.Import

class Wrappable (k :: Type) where
    data Wrapper k :: k -> k
    wrapperCoercion :: forall (a :: k). Coercion a (Wrapper k a)

instance Wrappable Type where
    newtype Wrapper Type (f :: Type) = MkWrapper0 f
    wrapperCoercion = MkCoercion

instance Wrappable (ka -> Type) where
    newtype Wrapper (ka -> Type) (f :: ka -> Type)
          (a :: ka) = MkWrapper1 (f a)
    wrapperCoercion = MkCoercion

instance Wrappable (ka -> kb -> Type) where
    newtype Wrapper (ka -> kb -> Type) (f :: ka -> kb -> Type)
          (a :: ka)
          (b :: kb) = MkWrapper2 (f a b)
    wrapperCoercion = MkCoercion

instance Wrappable (ka -> kb -> kc -> Type) where
    newtype Wrapper (ka -> kb -> kc -> Type)
          (f :: ka -> kb -> kc -> Type)
          (a :: ka)
          (b :: kb)
          (c :: kc) = MkWrapper3 (f a b c)
    wrapperCoercion = MkCoercion
