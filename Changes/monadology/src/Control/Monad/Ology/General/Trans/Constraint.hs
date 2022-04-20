module Control.Monad.Ology.General.Trans.Constraint where

import Import

type TransKind = (Type -> Type) -> (Type -> Type)

type TransConstraint :: ((Type -> Type) -> Constraint) -> TransKind -> Constraint
class TransConstraint c t where
    hasTransConstraint ::
           forall (m :: Type -> Type). c m
        => Dict (c (t m))

transConstraintDict ::
       forall c t m. TransConstraint c t
    => Dict (c m)
    -> Dict (c (t m))
transConstraintDict Dict = hasTransConstraint @c @t @m

withTransConstraintTM ::
       forall c t m a. (TransConstraint c t, c m)
    => (c (t m) => t m a)
    -> t m a
withTransConstraintTM tma =
    case hasTransConstraint @c @t @m of
        Dict -> tma

withTransConstraintTM' ::
       forall c t' t m a. (TransConstraint c t, c m)
    => (c (t m) => t' (t m) a)
    -> t' (t m) a
withTransConstraintTM' tma =
    case hasTransConstraint @c @t @m of
        Dict -> tma

withTransConstraintDict ::
       forall c t m c'. (TransConstraint c t, c m)
    => (c (t m) => Dict (c' (t m)))
    -> Dict (c' (t m))
withTransConstraintDict dict =
    case hasTransConstraint @c @t @m of
        Dict -> dict
