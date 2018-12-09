module Language.Expression.TypeSystem where

import Language.Expression.Abstract
import Language.Expression.Bindings
import Language.Expression.Renamer
import Language.Expression.Sealed
import Language.Expression.Unifier
import Shapes

class ( Monad (TSScoped ts)
      , Renamer (TSRenamer ts)
      , Unifier (TSUnifier ts)
      , RenamerNegWitness (TSRenamer ts) ~ UnifierNegWitness (TSUnifier ts)
      , RenamerPosWitness (TSRenamer ts) ~ UnifierPosWitness (TSUnifier ts)
      , TSRenamer ts (TSScoped ts) ~ UnifierMonad (TSUnifier ts)
      ) => TypeSystem (ts :: Type) where
    type TSRenamer ts :: (Type -> Type) -> (Type -> Type)
    type TSUnifier ts :: Type -> Type
    type TSScoped ts :: Type -> Type
    tsFunctionPosWitness :: FunctionPosWitness (TSNegWitness ts) (TSPosWitness ts)
    tsFunctionNegWitness :: FunctionNegWitness (TSNegWitness ts) (TSPosWitness ts)

type TSNegWitness ts = UnifierNegWitness (TSUnifier ts)

type TSPosWitness ts = UnifierPosWitness (TSUnifier ts)

type TSName ts = UnifierName (TSUnifier ts)

type TSValue ts = AnyValue (TSPosWitness ts)

type TSSealedExpression ts = UnifierSealedExpression (TSUnifier ts)

type TSMonad ts = TSRenamer ts (TSScoped ts)

tsUnify ::
       forall ts a b. TypeSystem ts
    => TSPosWitness ts a
    -> TSNegWitness ts b
    -> TSScoped ts (a -> b)
tsUnify wa wb = runRenamer @(TSRenamer ts) $ solveUnifyPosNegWitnesses @(TSUnifier ts) wa wb

tsEval ::
       forall ts m. (MonadFail m, Show (TSName ts))
    => TSSealedExpression ts
    -> m (TSValue ts)
tsEval = evalSealedExpression

tsAnyToVal ::
       forall ts t. TypeSystem ts
    => TSNegWitness ts t
    -> TSValue ts
    -> TSScoped ts t
tsAnyToVal witn (MkAnyValue witp val) = do
    conv <- tsUnify @ts witp witn
    return $ conv val

tsEvalToType ::
       forall ts t. (TypeSystem ts, MonadFail (TSScoped ts), Show (TSName ts))
    => TSNegWitness ts t
    -> TSSealedExpression ts
    -> TSScoped ts t
tsEvalToType witn expr = do
    aval <- tsEval @ts expr
    tsAnyToVal @ts witn aval

tsApply ::
       forall ts. TypeSystem ts
    => TSSealedExpression ts
    -> TSSealedExpression ts
    -> TSScoped ts (TSSealedExpression ts)
tsApply tf ta = applySealedExpression @(TSRenamer ts) @(TSUnifier ts) (tsFunctionNegWitness @ts) tf ta

tsAbstract ::
       forall ts. TypeSystem ts
    => TSName ts
    -> TSSealedExpression ts
    -> TSScoped ts (TSSealedExpression ts)
tsAbstract n expr = abstractSealedExpression @(TSRenamer ts) @(TSUnifier ts) (tsFunctionPosWitness @ts) n expr

tsVar ::
       forall ts. TypeSystem ts
    => TSName ts
    -> TSSealedExpression ts
tsVar name =
    runIdentity $
    runRenamer @(TSRenamer ts) $
    renameNewVar $ \vwt twt conv -> withTransConstraintTM @Monad $ return $ varSealedExpression name vwt twt conv

tsConst :: forall ts. TSValue ts -> TSSealedExpression ts
tsConst = constSealedExpression

tsLet ::
       forall ts. TypeSystem ts
    => TSName ts
    -> TSSealedExpression ts
    -> TSSealedExpression ts
    -> TSScoped ts (TSSealedExpression ts)
tsLet n expv expb = letSealedExpression @(TSRenamer ts) @(TSUnifier ts) n expv expb

type TSBindings ts = Bindings (TSUnifier ts)

tsSingleBinding ::
       forall ts. TypeSystem ts
    => TSName ts
    -> TSSealedExpression ts
    -> TSBindings ts
tsSingleBinding = singleBinding

tsUncheckedLet ::
       forall ts. (Ord (TSName ts), TypeSystem ts)
    => TSBindings ts
    -> TSScoped ts (StrictMap (TSName ts) (TSSealedExpression ts))
tsUncheckedLet = bindingsLetSealedExpression @(TSRenamer ts) @(TSUnifier ts)

tsValuesLet ::
       forall ts. (Ord (TSName ts), TypeSystem ts)
    => StrictMap (TSName ts) (TSValue ts)
    -> StrictMap (TSName ts) (TSSealedExpression ts)
tsValuesLet = valuesLetSealedExpression @(TSUnifier ts)

tsBindingsCheckDuplicates ::
       forall ts m. (Show (TSName ts), TypeSystem ts, MonadFail m)
    => TSBindings ts
    -> m ()
tsBindingsCheckDuplicates = bindingsCheckDuplicates
