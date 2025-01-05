module Data.Givable where

import Data.Coerce.Coercion
import Data.Wrappable
import Shapes.Import
import Unsafe.Refl (unsafeDerive1)

class Wrappable k => Givable (t :: k -> Type) where
    type GivableConstraint t :: k -> Constraint
    giveWrapperConstraint :: forall (a :: k) r. t a -> (GivableConstraint t (Wrapper k a) => r) -> r

giveWrapperConstraintDict ::
       forall k (t :: k -> Type) (a :: k). Givable t
    => t a
    -> Dict (GivableConstraint t (Wrapper k a))
giveWrapperConstraintDict ta = giveWrapperConstraint ta Dict

giveConstraintDict ::
       forall k (t :: k -> Type) (a :: k). Givable t
    => t a
    -> Dict (GivableConstraint t a)
giveConstraintDict ta =
    case wrapperCoercion @k @a of
        MkCoercion -> unsafeDerive1 $ giveWrapperConstraintDict ta

giveConstraint ::
       forall k (t :: k -> Type) (a :: k) (r :: Type). Givable t
    => t a
    -> (GivableConstraint t a => r)
    -> r
giveConstraint tk call =
    case giveConstraintDict tk of
        Dict -> call
