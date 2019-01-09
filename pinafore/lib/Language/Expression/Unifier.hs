module Language.Expression.Unifier where

import Language.Expression.Expression
import Language.Expression.Named
import Language.Expression.Pattern
import Language.Expression.Sealed
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
        -> (forall t'. UnifierPosWitness unifier t' -> (t -> t') -> UnifierMonad unifier r)
        -> UnifierMonad unifier r
    unifierNegSubstitute ::
           UnifierSubstitutions unifier
        -> UnifierNegWitness unifier t
        -> (forall t'. UnifierNegWitness unifier t' -> (t' -> t) -> UnifierMonad unifier r)
        -> UnifierMonad unifier r
    simplifyExpressionType :: UnifierSealedExpression unifier -> UnifierMonad unifier (UnifierSealedExpression unifier)
    simplifyPatternType :: UnifierSealedPattern unifier -> UnifierMonad unifier (UnifierSealedPattern unifier)

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

unifierExpressionSubstitute ::
       forall unifier a. Unifier unifier
    => UnifierSubstitutions unifier
    -> UnifierOpenExpression unifier a
    -> UnifierMonad unifier (UnifierOpenExpression unifier a)
unifierExpressionSubstitute _ (ClosedExpression a) = return $ ClosedExpression a
unifierExpressionSubstitute subs (OpenExpression (MkNameWitness name tw) expr) =
    unifierNegSubstitute @unifier subs tw $ \tw' conv -> do
        expr' <- unifierExpressionSubstitute @unifier subs expr
        return $ OpenExpression (MkNameWitness name tw') $ fmap (\ta -> ta . conv) expr'

unifierPatternSubstitute ::
       forall unifier a b. Unifier unifier
    => UnifierSubstitutions unifier
    -> UnifierOpenPattern unifier a b
    -> UnifierMonad unifier (UnifierOpenPattern unifier a b)
unifierPatternSubstitute _ (ClosedPattern a) = return $ ClosedPattern a
unifierPatternSubstitute subs (OpenPattern (MkNameWitness name tw) pat) =
    unifierPosSubstitute @unifier subs tw $ \tw' conv -> do
        pat' <- unifierPatternSubstitute @unifier subs pat
        return $ OpenPattern (MkNameWitness name tw') $ fmap (\(t, b) -> (conv t, b)) pat'

unifierExpressionSubstituteAndSimplify ::
       forall unifier a. Unifier unifier
    => UnifierSubstitutions unifier
    -> UnifierPosWitness unifier a
    -> UnifierOpenExpression unifier a
    -> UnifierMonad unifier (UnifierSealedExpression unifier)
unifierExpressionSubstituteAndSimplify subs twt expr =
    unifierPosSubstitute @unifier subs twt $ \twt' tconv -> do
        expr' <- unifierExpressionSubstitute @unifier subs $ fmap tconv expr
        simplifyExpressionType @unifier $ MkSealedExpression twt' expr'

unifierPatternSubstituteAndSimplify ::
       forall unifier a. Unifier unifier
    => UnifierSubstitutions unifier
    -> UnifierNegWitness unifier a
    -> UnifierOpenPattern unifier a ()
    -> UnifierMonad unifier (UnifierSealedPattern unifier)
unifierPatternSubstituteAndSimplify subs twt pat =
    unifierNegSubstitute @unifier subs twt $ \twt' tconv -> do
        pat' <- unifierPatternSubstitute @unifier subs $ pat . arr tconv
        simplifyPatternType @unifier $ MkSealedPattern twt' pat'

solveUnifierExpression ::
       forall unifier a. Unifier unifier
    => UnifierPosWitness unifier a
    -> unifier (UnifierOpenExpression unifier a)
    -> UnifierMonad unifier (UnifierSealedExpression unifier)
solveUnifierExpression rtwt uexp = do
    (expr', subs) <- solveUnifier @unifier uexp
    unifierExpressionSubstituteAndSimplify @unifier subs rtwt expr'

solveUnifierPattern ::
       forall unifier a. Unifier unifier
    => UnifierNegWitness unifier a
    -> unifier (UnifierOpenPattern unifier a ())
    -> UnifierMonad unifier (UnifierSealedPattern unifier)
solveUnifierPattern rtwt uexp = do
    (pat', subs) <- solveUnifier @unifier uexp
    unifierPatternSubstituteAndSimplify @unifier subs rtwt pat'

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
