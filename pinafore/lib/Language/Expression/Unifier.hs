module Language.Expression.Unifier where

import Language.Expression.Expression
import Language.Expression.Named
import Language.Expression.Sealed
import Shapes

class (Monad (UnifierMonad unifier), Applicative unifier, Eq (UnifierName unifier)) => Unifier unifier where
    type UnifierName unifier :: Type
    type UnifierMonad unifier :: Type -> Type
    type UnifierTSNegWitness unifier :: Type -> Type
    type UnifierTSPosWitness unifier :: Type -> Type
    type UnifierSubstitutions unifier :: Type
    unifyTSNegWitnesses ::
           UnifierTSNegWitness unifier a
        -> UnifierTSNegWitness unifier b
        -> (forall ab. UnifierTSNegWitness unifier ab -> unifier (ab -> a, ab -> b) -> UnifierMonad unifier r)
        -> UnifierMonad unifier r
    unifyTSPosWitnesses ::
           UnifierTSPosWitness unifier a
        -> UnifierTSPosWitness unifier b
        -> (forall ab. UnifierTSPosWitness unifier ab -> unifier (a -> ab, b -> ab) -> UnifierMonad unifier r)
        -> UnifierMonad unifier r
    unifyPosTSNegWitnesses ::
           UnifierTSPosWitness unifier a -> UnifierTSNegWitness unifier b -> UnifierMonad unifier (unifier (a -> b))
    solveUnifier :: unifier a -> UnifierMonad unifier (a, UnifierSubstitutions unifier)
    unifierPosSubstitute ::
           UnifierSubstitutions unifier
        -> UnifierTSPosWitness unifier t
        -> (forall t'. UnifierTSPosWitness unifier t' -> (t -> t') -> r)
        -> r
    unifierNegSubstitute ::
           UnifierSubstitutions unifier
        -> UnifierTSNegWitness unifier t
        -> (forall t'. UnifierTSNegWitness unifier t' -> (t' -> t) -> r)
        -> r
    simplifyExpressionType ::
           SealedExpression (UnifierName unifier) (UnifierTSNegWitness unifier) (UnifierTSPosWitness unifier)
        -> UnifierMonad unifier (SealedExpression (UnifierName unifier) (UnifierTSNegWitness unifier) (UnifierTSPosWitness unifier))

liftUnifier :: Monad (UnifierMonad unifier) => unifier a -> Compose (UnifierMonad unifier) unifier a
liftUnifier ua = Compose $ return ua

solveUnifyPosTSNegWitnesses ::
       forall unifier a b. Unifier unifier
    => UnifierTSPosWitness unifier a
    -> UnifierTSNegWitness unifier b
    -> UnifierMonad unifier (a -> b)
solveUnifyPosTSNegWitnesses wa wb = do
    uab <- unifyPosTSNegWitnesses @unifier wa wb
    (ab, _) <- solveUnifier uab
    return ab

unifierExpressionSubstitute ::
       forall unifier a. Unifier unifier
    => UnifierSubstitutions unifier
    -> NamedExpression (UnifierName unifier) (UnifierTSNegWitness unifier) a
    -> NamedExpression (UnifierName unifier) (UnifierTSNegWitness unifier) a
unifierExpressionSubstitute _ (ClosedExpression a) = ClosedExpression a
unifierExpressionSubstitute subs (OpenExpression (MkNameWitness name tw) expr) =
    unifierNegSubstitute @unifier subs tw $ \tw' conv ->
        OpenExpression (MkNameWitness name tw') $
        fmap (\ta -> ta . conv) $ unifierExpressionSubstitute @unifier subs expr

unifierExpressionSubstituteAndSimplify ::
       forall unifier a. Unifier unifier
    => UnifierSubstitutions unifier
    -> UnifierTSPosWitness unifier a
    -> NamedExpression (UnifierName unifier) (UnifierTSNegWitness unifier) a
    -> UnifierMonad unifier (SealedExpression (UnifierName unifier) (UnifierTSNegWitness unifier) (UnifierTSPosWitness unifier))
unifierExpressionSubstituteAndSimplify subs twt expr =
    simplifyExpressionType @unifier $
    unifierPosSubstitute @unifier subs twt $ \twt' tconv ->
        MkSealedExpression twt' $ unifierExpressionSubstitute @unifier subs $ fmap tconv expr

data UnifyExpression unifier a =
    forall conv. MkUnifyExpression (unifier conv)
                                   (NamedExpression (UnifierName unifier) (UnifierTSNegWitness unifier) (conv -> a))

instance Functor (UnifyExpression unifier) where
    fmap ab (MkUnifyExpression uconv expr) = MkUnifyExpression uconv $ fmap (fmap ab) expr

instance Applicative unifier => Applicative (UnifyExpression unifier) where
    pure a = MkUnifyExpression (pure ()) $ pure $ \_ -> a
    liftA2 abc (MkUnifyExpression uconva expra) (MkUnifyExpression uconvb exprb) =
        MkUnifyExpression (liftA2 (,) uconva uconvb) $ liftA2 (\caa cbb (ca, cb) -> abc (caa ca) (cbb cb)) expra exprb

unifyExpression ::
       Applicative unifier
    => NamedExpression (UnifierName unifier) (UnifierTSNegWitness unifier) a
    -> UnifyExpression unifier a
unifyExpression expr = MkUnifyExpression (pure ()) $ fmap (\a _ -> a) expr

unifierExpression ::
       Functor unifier
    => UnifyExpression unifier a
    -> unifier (NamedExpression (UnifierName unifier) (UnifierTSNegWitness unifier) a)
unifierExpression (MkUnifyExpression uconv expr) = fmap (\conv -> fmap (\conva -> conva conv) expr) uconv
