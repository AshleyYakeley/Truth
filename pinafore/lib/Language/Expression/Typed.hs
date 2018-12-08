module Language.Expression.Typed where

import Language.Expression.Abstract
import Language.Expression.Bindings
import Language.Expression.Renamer
import Language.Expression.Sealed
import Language.Expression.Unifier
import Shapes

class ( Monad (TSScoped ts)
      , Renamer (TSRenamer ts)
      , RenamerTSNegWitness (TSRenamer ts) ~ TSNegWitness ts
      , RenamerTSPosWitness (TSRenamer ts) ~ TSPosWitness ts
      , Unifier (TSUnifier ts)
      , UnifierMonad (TSUnifier ts) ~ TSRenamer ts (TSScoped ts)
      , UnifierTSNegWitness (TSUnifier ts) ~ TSNegWitness ts
      , UnifierTSPosWitness (TSUnifier ts) ~ TSPosWitness ts
      ) => TypeSystem (ts :: Type) where
    type TSRenamer ts :: (Type -> Type) -> (Type -> Type)
    type TSUnifier ts :: Type -> Type
    type TSNegWitness ts :: Type -> Type
    type TSPosWitness ts :: Type -> Type
    type TSScoped ts :: Type -> Type
    typeSystemFunctionTSPosWitness :: FunctionTSPosWitness (TSNegWitness ts) (TSPosWitness ts)
    typeSystemFunctionTSNegWitness :: FunctionTSNegWitness (TSNegWitness ts) (TSPosWitness ts)

type TSName ts = UnifierName (TSUnifier ts)

type TSValue ts = AnyValue (TSPosWitness ts)

type TSSealedExpression ts = SealedExpression (TSName ts) (TSNegWitness ts) (TSPosWitness ts)

type TSRenamerScoped ts = TSRenamer ts (TSScoped ts)

typedUnifyPosTSNegWitnesses ::
       forall ts a b. TypeSystem ts
    => TSPosWitness ts a
    -> TSNegWitness ts b
    -> TSScoped ts (a -> b)
typedUnifyPosTSNegWitnesses wa wb = runRenamer @(TSRenamer ts) $ solveUnifyPosTSNegWitnesses @(TSUnifier ts) wa wb

evalTSSealedExpression ::
       forall ts m. (MonadFail m, Show (TSName ts))
    => TSSealedExpression ts
    -> m (TSValue ts)
evalTSSealedExpression = evalSealedExpression

typedAnyToVal ::
       forall ts t. TypeSystem ts
    => TSNegWitness ts t
    -> TSValue ts
    -> TSScoped ts t
typedAnyToVal witn (MkAnyValue witp val) = do
    conv <- typedUnifyPosTSNegWitnesses @ts witp witn
    return $ conv val

evalTSSealedExpressionToType ::
       forall ts t. (TypeSystem ts, MonadFail (TSScoped ts), Show (TSName ts))
    => TSNegWitness ts t
    -> TSSealedExpression ts
    -> TSScoped ts t
evalTSSealedExpressionToType witn expr = do
    aval <- evalTSSealedExpression @ts expr
    typedAnyToVal @ts witn aval

applyTSSealedExpression ::
       forall ts. TypeSystem ts
    => TSSealedExpression ts
    -> TSSealedExpression ts
    -> TSScoped ts (TSSealedExpression ts)
applyTSSealedExpression tf ta =
    applySealedExpression @(TSRenamer ts) @(TSUnifier ts) (typeSystemFunctionTSNegWitness @ts) tf ta

abstractTSSealedExpression ::
       forall ts. TypeSystem ts
    => TSName ts
    -> TSSealedExpression ts
    -> TSScoped ts (TSSealedExpression ts)
abstractTSSealedExpression n expr =
    abstractSealedExpression @(TSRenamer ts) @(TSUnifier ts) (typeSystemFunctionTSPosWitness @ts) n expr

varTSSealedExpression ::
       forall ts. TypeSystem ts
    => TSName ts
    -> TSSealedExpression ts
varTSSealedExpression name =
    runIdentity $
    runRenamer @(TSRenamer ts) $
    renameNewVar $ \vwt twt conv -> withTransConstraintTM @Monad $ return $ varSealedExpression name vwt twt conv

constTSSealedExpression :: forall ts. TSValue ts -> TSSealedExpression ts
constTSSealedExpression = constSealedExpression

letTSSealedExpression ::
       forall ts. TypeSystem ts
    => TSName ts
    -> TSSealedExpression ts
    -> TSSealedExpression ts
    -> TSScoped ts (TSSealedExpression ts)
letTSSealedExpression n expv expb = letSealedExpression @(TSRenamer ts) @(TSUnifier ts) n expv expb

type TypedBindings ts = Bindings (TSUnifier ts)

singleTypedBinding ::
       forall ts. TypeSystem ts
    => TSName ts
    -> TSSealedExpression ts
    -> TypedBindings ts
singleTypedBinding = singleBinding

uncheckedBindingsLetTSSealedExpression ::
       forall ts. (Ord (TSName ts), TypeSystem ts)
    => TypedBindings ts
    -> TSSealedExpression ts
    -> TSScoped ts (TSSealedExpression ts)
uncheckedBindingsLetTSSealedExpression bb expb = bindingsLetSealedExpression @(TSRenamer ts) @(TSUnifier ts) bb expb

valuesLetTSSealedExpression ::
       forall ts. (Ord (TSName ts), TypeSystem ts)
    => (TSName ts -> Maybe (TSValue ts))
    -> TSSealedExpression ts
    -> TSScoped ts (TSSealedExpression ts)
valuesLetTSSealedExpression valbind expb = valuesLetSealedExpression @(TSRenamer ts) @(TSUnifier ts) valbind expb

typedBindingsCheckDuplicates ::
       forall ts m. (Show (TSName ts), TypeSystem ts, MonadFail m)
    => TypedBindings ts
    -> m ()
typedBindingsCheckDuplicates = bindingsCheckDuplicates
