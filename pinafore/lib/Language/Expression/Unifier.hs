module Language.Expression.Unifier where

import Language.Expression.Named
import Shapes

class (Monad (UnifierMonad unifier), Applicative unifier) => Unifier unifier where
    type UnifierMonad unifier :: Type -> Type
    type UnifierNegWitness unifier :: Type -> Type
    type UnifierPosWitness unifier :: Type -> Type
    unifyNegWitnesses ::
           UnifierNegWitness unifier a
        -> UnifierNegWitness unifier b
        -> (forall ab. UnifierNegWitness unifier ab -> unifier (ab -> a, ab -> b) -> UnifierMonad unifier r)
        -> UnifierMonad unifier r
    unifyPosWitnesses ::
           UnifierPosWitness unifier a
        -> UnifierPosWitness unifier b
        -> (forall ab. UnifierPosWitness unifier ab -> unifier (a -> ab, b -> ab) -> UnifierMonad unifier r)
        -> UnifierMonad unifier r
    unifyPosNegWitnesses ::
           UnifierPosWitness unifier a -> UnifierNegWitness unifier b -> UnifierMonad unifier (unifier (a -> b))
    solveUnifier ::
           UnifierPosWitness unifier t
        -> unifier a
        -> (forall t'. UnifierPosWitness unifier t' -> (t -> t') -> a -> UnifierMonad unifier r)
        -> UnifierMonad unifier r

data UnifyExpression name unifier a =
    forall conv. MkUnifyExpression (unifier conv)
                                   (NamedExpression name (UnifierNegWitness unifier) (conv -> a))

instance Functor (UnifyExpression name unifier) where
    fmap ab (MkUnifyExpression uconv expr) = MkUnifyExpression uconv $ fmap (fmap ab) expr

instance Applicative unifier => Applicative (UnifyExpression name unifier) where
    pure a = MkUnifyExpression (pure ()) $ pure $ \_ -> a
    liftA2 abc (MkUnifyExpression uconva expra) (MkUnifyExpression uconvb exprb) =
        MkUnifyExpression (liftA2 (,) uconva uconvb) $ liftA2 (\caa cbb (ca, cb) -> abc (caa ca) (cbb cb)) expra exprb

unifyExpression ::
       Applicative unifier => NamedExpression name (UnifierNegWitness unifier) a -> UnifyExpression name unifier a
unifyExpression expr = MkUnifyExpression (pure ()) $ fmap (\a _ -> a) expr

unifierExpression ::
       Functor unifier => UnifyExpression name unifier a -> unifier (NamedExpression name (UnifierNegWitness unifier) a)
unifierExpression (MkUnifyExpression uconv expr) = fmap (\conv -> fmap (\conva -> conva conv) expr) uconv
