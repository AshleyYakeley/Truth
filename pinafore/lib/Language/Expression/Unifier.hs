module Language.Expression.Unifier where

import Language.Expression.Expression
import Language.Expression.Named
import Language.Expression.Sealed
import Shapes

class (Monad (UnifierMonad unifier), Applicative unifier) => Unifier unifier where
    type UnifierMonad unifier :: Type -> Type
    type UnifierNegWitness unifier :: Type -> Type
    type UnifierPosWitness unifier :: Type -> Type
    type UnifierSubstitutions unifier :: Type
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
           UnifierPosWitness unifier a -> UnifierNegWitness unifier b -> Compose (UnifierMonad unifier) unifier (a -> b)
    solveUnifier :: unifier a -> UnifierMonad unifier (a, UnifierSubstitutions unifier)
    unifierPosSubstitute ::
           UnifierSubstitutions unifier
        -> UnifierPosWitness unifier t
        -> (forall t'. UnifierPosWitness unifier t' -> (t -> t') -> r)
        -> r
    unifierNegSubstitute ::
           UnifierSubstitutions unifier
        -> UnifierNegWitness unifier t
        -> (forall t'. UnifierNegWitness unifier t' -> (t' -> t) -> r)
        -> r
    simplifyExpressionType ::
           SealedExpression name (UnifierNegWitness unifier) (UnifierPosWitness unifier)
        -> SealedExpression name (UnifierNegWitness unifier) (UnifierPosWitness unifier)

liftUnifier :: Monad (UnifierMonad unifier) => unifier a -> Compose (UnifierMonad unifier) unifier a
liftUnifier ua = Compose $ return ua

solveUnifyPosNegWitnesses ::
       forall unifier a b. Unifier unifier
    => UnifierPosWitness unifier a
    -> UnifierNegWitness unifier b
    -> UnifierMonad unifier (a -> b)
solveUnifyPosNegWitnesses wa wb = do
    uab <- getCompose $ unifyPosNegWitnesses @unifier wa wb
    (ab, _) <- solveUnifier uab
    return ab

unifierExpressionSubstitute ::
       forall unifier name a. Unifier unifier
    => UnifierSubstitutions unifier
    -> NamedExpression name (UnifierNegWitness unifier) a
    -> NamedExpression name (UnifierNegWitness unifier) a
unifierExpressionSubstitute _ (ClosedExpression a) = ClosedExpression a
unifierExpressionSubstitute subs (OpenExpression (MkNameWitness name tw) expr) =
    unifierNegSubstitute @unifier subs tw $ \tw' conv ->
        OpenExpression (MkNameWitness name tw') $
        fmap (\ta -> ta . conv) $ unifierExpressionSubstitute @unifier subs expr

unifierExpressionSubstituteAndSimplify ::
       forall unifier name a. Unifier unifier
    => UnifierSubstitutions unifier
    -> UnifierPosWitness unifier a
    -> NamedExpression name (UnifierNegWitness unifier) a
    -> SealedExpression name (UnifierNegWitness unifier) (UnifierPosWitness unifier)
unifierExpressionSubstituteAndSimplify subs twt expr =
    simplifyExpressionType @unifier $ MkSealedExpression twt $ unifierExpressionSubstitute @unifier subs expr

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
