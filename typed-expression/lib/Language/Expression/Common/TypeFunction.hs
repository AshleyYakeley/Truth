-- | the "U" is for "unsafe". This entire module is built on lies.
module Language.Expression.Common.TypeFunction where

import Shapes

data TF kp kq
    = TFConstant kq
    | TFConstructor (kp -> kq)
    | TFOther

type Apply :: TF kp kq -> kp -> kq
type family Apply s t where
    Apply ('TFConstant a) t = a
    Apply ('TFConstructor f) t = f t

type ApplyFunctor :: TF Type Type -> Type
newtype ApplyFunctor tf = MkApplyFunctor
    { unApplyFunctor :: forall a b. (a -> b) -> Apply tf a -> Apply tf b
    }

bijApplyFunctor ::
       forall (tf :: TF Type Type) (a :: Type) (b :: Type).
       ApplyFunctor tf
    -> Bijection a b
    -> Bijection (Apply tf a) (Apply tf b)
bijApplyFunctor (MkApplyFunctor afmap) (MkIsomorphism ab ba) = MkIsomorphism (afmap ab) (afmap ba)
