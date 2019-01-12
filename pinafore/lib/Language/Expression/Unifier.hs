module Language.Expression.Unifier where

import Language.Expression.Named
import Language.Expression.Polarity
import Language.Expression.Sealed
import Language.Expression.TypeF
import Language.Expression.TypeMappable
import Shapes

class (Monad (UnifierMonad unifier), Applicative unifier, Eq (UnifierName unifier)) => Unifier unifier where
    type UnifierName unifier :: Type
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
           UnifierPosWitness unifier a -> UnifierNegWitness unifier b -> UnifierMonad unifier (unifier (a -> b))
    solveUnifier :: unifier a -> UnifierMonad unifier (a, UnifierSubstitutions unifier)
    unifierPosSubstitute ::
           UnifierSubstitutions unifier
        -> UnifierPosWitness unifier t
        -> UnifierMonad unifier (TypeF (UnifierPosWitness unifier) 'Positive t)
    unifierNegSubstitute ::
           UnifierSubstitutions unifier
        -> UnifierNegWitness unifier t
        -> UnifierMonad unifier (TypeF (UnifierNegWitness unifier) 'Negative t)
    simplify ::
           forall a. UnifierMappable unifier a
        => a
        -> UnifierMonad unifier a

type UnifierMappable unifier = TypeMappable (UnifierPosWitness unifier) (UnifierNegWitness unifier)

type UnifierOpenExpression unifier = NamedExpression (UnifierName unifier) (UnifierNegWitness unifier)

type UnifierSealedExpression unifier
     = SealedExpression (UnifierName unifier) (UnifierNegWitness unifier) (UnifierPosWitness unifier)

type UnifierOpenPattern unifier = NamedPattern (UnifierName unifier) (UnifierPosWitness unifier)

type UnifierSealedPattern unifier
     = SealedPattern (UnifierName unifier) (UnifierPosWitness unifier) (UnifierNegWitness unifier)

liftUnifier :: Monad (UnifierMonad unifier) => unifier a -> Compose (UnifierMonad unifier) unifier a
liftUnifier ua = Compose $ return ua

solveUnifyPosNegWitnesses ::
       forall unifier a b. Unifier unifier
    => UnifierPosWitness unifier a
    -> UnifierNegWitness unifier b
    -> UnifierMonad unifier (a -> b)
solveUnifyPosNegWitnesses wa wb = do
    uab <- unifyPosNegWitnesses @unifier wa wb
    (ab, _) <- solveUnifier uab
    return ab

unifierSubstitute ::
       forall unifier a. (Unifier unifier, UnifierMappable unifier a)
    => UnifierSubstitutions unifier
    -> a
    -> UnifierMonad unifier a
unifierSubstitute subs = mapTypesM (unifierPosSubstitute @unifier subs) (unifierNegSubstitute @unifier subs)

unifierSubstituteAndSimplify ::
       forall unifier a. (Unifier unifier, UnifierMappable unifier a)
    => UnifierSubstitutions unifier
    -> a
    -> UnifierMonad unifier a
unifierSubstituteAndSimplify subs a = do
    a' <- unifierSubstitute @unifier subs a
    simplify @unifier a'

unifierSolve ::
       forall unifier a. (Unifier unifier, UnifierMappable unifier a)
    => unifier a
    -> UnifierMonad unifier a
unifierSolve ua = do
    (a, subs) <- solveUnifier @unifier ua
    unifierSubstituteAndSimplify @unifier subs a

data UnifyExpression unifier a =
    forall conv. MkUnifyExpression (unifier conv)
                                   (UnifierOpenExpression unifier (conv -> a))

instance Functor (UnifyExpression unifier) where
    fmap ab (MkUnifyExpression uconv expr) = MkUnifyExpression uconv $ fmap (fmap ab) expr

instance Applicative unifier => Applicative (UnifyExpression unifier) where
    pure a = MkUnifyExpression (pure ()) $ pure $ \_ -> a
    liftA2 abc (MkUnifyExpression uconva expra) (MkUnifyExpression uconvb exprb) =
        MkUnifyExpression (liftA2 (,) uconva uconvb) $ liftA2 (\caa cbb (ca, cb) -> abc (caa ca) (cbb cb)) expra exprb

exprUnifyExpression :: Applicative unifier => UnifierOpenExpression unifier a -> UnifyExpression unifier a
exprUnifyExpression expr = MkUnifyExpression (pure ()) $ fmap (\a _ -> a) expr

unifierUnifyExpression :: Applicative unifier => unifier a -> UnifyExpression unifier a
unifierUnifyExpression ua = MkUnifyExpression ua $ pure id

unifierExpression :: Functor unifier => UnifyExpression unifier a -> unifier (UnifierOpenExpression unifier a)
unifierExpression (MkUnifyExpression uconv expr) = fmap (\conv -> fmap (\conva -> conva conv) expr) uconv
