module Language.Expression.TypeSystem where

import Data.Shim.JoinMeet
import Data.Shim.ShimWit
import Language.Expression.Abstract
import Language.Expression.Bindings
import Language.Expression.Error
import Language.Expression.Renamer
import Language.Expression.Sealed
import Language.Expression.Subsumer
import Language.Expression.Unifier
import Shapes

type SubsumerRenamerConstraint subsumer renamer m
     = ( Monad m
       , Renamer renamer
       , Subsumer subsumer
       , SubsumerNegWitness subsumer ~ RenamerNegWitness renamer
       , SubsumerPosWitness subsumer ~ RenamerPosWitness renamer
       , SubsumerShim subsumer ~ RenamerShim renamer
       , SubsumerMonad subsumer ~ renamer m)

class ( UnifierRenamerConstraint (TSUnifier ts) (TSRenamer ts) (TSScoped ts)
      , SubsumerRenamerConstraint (TSSubsumer ts) (TSRenamer ts) (TSScoped ts)
      ) => TypeSystem (ts :: Type) where
    type TSRenamer ts :: (Type -> Type) -> (Type -> Type)
    type TSUnifier ts :: Type -> Type
    type TSSubsumer ts :: Type -> Type
    type TSScoped ts :: Type -> Type
    tsFunctionPosWitness :: forall a b. TSNegWitness ts a -> TSPosWitness ts b -> TSPosShimWit ts (a -> b)
    tsFunctionNegWitness :: forall a b. TSPosWitness ts a -> TSNegWitness ts b -> TSNegShimWit ts (a -> b)

type TSNegWitness ts = UnifierNegWitness (TSUnifier ts)

type TSPosWitness ts = UnifierPosWitness (TSUnifier ts)

type TSNegShimWit ts = UnifierNegShimWit (TSUnifier ts)

type TSPosShimWit ts = UnifierPosShimWit (TSUnifier ts)

type TSName ts = UnifierName (TSUnifier ts)

type TSShim ts = UnifierShim (TSUnifier ts)

type TSValue ts = AnyValue (TSPosShimWit ts)

type TSSealedExpression ts = UnifierSealedExpression (TSUnifier ts)

type TSSealedPattern ts = UnifierSealedPattern (TSUnifier ts)

type TSPatternConstructor ts = UnifierPatternConstructor (TSUnifier ts)

tsFunctionPosShimWit ::
       forall ts. TypeSystem ts
    => FunctionWitness (TSNegShimWit ts) (TSPosShimWit ts)
tsFunctionPosShimWit (MkShimWit wa conva) (MkShimWit wb convb) =
    mapShimWit (funcShim conva convb) $ tsFunctionPosWitness @ts wa wb

tsFunctionNegShimWit ::
       forall ts. TypeSystem ts
    => FunctionWitness (TSPosShimWit ts) (TSNegShimWit ts)
tsFunctionNegShimWit (MkShimWit wa conva) (MkShimWit wb convb) =
    mapShimWit (funcShim conva convb) $ tsFunctionNegWitness @ts wa wb

tsUnify ::
       forall ts a b. TypeSystem ts
    => TSPosShimWit ts a
    -> TSNegShimWit ts b
    -> TSScoped ts (TSShim ts a b)
tsUnify wa wb = runRenamer @(TSRenamer ts) $ solveUnifyPosNegShimWit @(TSUnifier ts) wa wb

tsEval ::
       forall ts m. (MonadThrow ExpressionError m, Show (TSName ts))
    => TSSealedExpression ts
    -> m (TSValue ts)
tsEval = evalSealedExpression

tsAnyToVal ::
       forall ts t. TypeSystem ts
    => TSNegShimWit ts t
    -> TSValue ts
    -> TSScoped ts t
tsAnyToVal witn (MkAnyValue witp val) = do
    conv <- tsUnify @ts witp witn
    return $ fromEnhanced conv val

tsEvalToType ::
       forall ts t. (TypeSystem ts, MonadThrow ExpressionError (TSScoped ts), Show (TSName ts))
    => TSNegShimWit ts t
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
tsApply tf ta = applySealedExpression @(TSRenamer ts) @(TSUnifier ts) (tsFunctionNegShimWit @ts) tf ta

tsAbstract ::
       forall ts. TypeSystem ts
    => TSName ts
    -> TSSealedExpression ts
    -> TSScoped ts (TSSealedExpression ts)
tsAbstract n expr = abstractSealedExpression @(TSRenamer ts) @(TSUnifier ts) (tsFunctionPosShimWit @ts) n expr

tsCase ::
       forall ts. TypeSystem ts
    => TSSealedExpression ts
    -> [(TSSealedPattern ts, TSSealedExpression ts)]
    -> TSScoped ts (TSSealedExpression ts)
tsCase expr cases = caseSealedExpression @(TSRenamer ts) @(TSUnifier ts) expr cases

tsCaseAbstract ::
       forall ts. TypeSystem ts
    => [(TSSealedPattern ts, TSSealedExpression ts)]
    -> TSScoped ts (TSSealedExpression ts)
tsCaseAbstract cases = caseAbstractSealedExpression @(TSRenamer ts) @(TSUnifier ts) (tsFunctionPosShimWit @ts) cases

tsVar ::
       forall ts. TypeSystem ts
    => TSName ts
    -> TSSealedExpression ts
tsVar name =
    runIdentity $
    runRenamer @(TSRenamer ts) $
    withTransConstraintTM @Monad $ do
        MkNewVar vwt twt <- renameNewVar
        return $ varSealedExpression name vwt twt

tsConst ::
       forall ts. TypeSystem ts
    => TSValue ts
    -> TSSealedExpression ts
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

tsUncheckedComponentLet ::
       forall ts. (Ord (TSName ts), TypeSystem ts)
    => TSBindings ts
    -> TSScoped ts (Map (TSName ts) (TSSealedExpression ts))
tsUncheckedComponentLet = bindingsComponentLetSealedExpression @(TSRenamer ts) @(TSUnifier ts)

tsValuesLet ::
       forall ts. (Ord (TSName ts), TypeSystem ts)
    => Map (TSName ts) (TSValue ts)
    -> Map (TSName ts) (TSSealedExpression ts)
tsValuesLet = valuesLetSealedExpression @(TSUnifier ts)

tsSubsume ::
       forall ts. TypeSystem ts
    => AnyW (TSPosShimWit ts)
    -> TSSealedExpression ts
    -> TSScoped ts (TSSealedExpression ts)
tsSubsume (MkAnyW t) expr =
    runRenamer @(TSRenamer ts) $ do
        at' <-
            namespace $
            withTransConstraintTM @Monad $ do
                MkShimWit t' _ <- renamePosShimWit t
                return $ MkAnyW t'
        expr' <- rename expr
        subsumeExpression @(TSSubsumer ts) at' expr'

tsVarPattern ::
       forall ts. TypeSystem ts
    => TSName ts
    -> TSSealedPattern ts
tsVarPattern name =
    runIdentity $
    runRenamer @(TSRenamer ts) $
    withTransConstraintTM @Monad $ do
        MkNewVar vwt twt <- renameNewVar
        return $ varSealedPattern name vwt twt

tsAnyPattern ::
       forall ts. TypeSystem ts
    => TSSealedPattern ts
tsAnyPattern =
    runIdentity $
    runRenamer @(TSRenamer ts) $
    withTransConstraintTM @Monad $ do
        MkNewVar twt _ <- renameNewVar
        return $ anySealedPattern twt

tsBothPattern ::
       forall ts. TypeSystem ts
    => TSSealedPattern ts
    -> TSSealedPattern ts
    -> TSScoped ts (TSSealedPattern ts)
tsBothPattern = bothSealedPattern @(TSRenamer ts) @(TSUnifier ts)

tsSealPatternConstructor ::
       forall ts m. MonadThrow ExpressionError m
    => TSPatternConstructor ts
    -> m (TSSealedPattern ts)
tsSealPatternConstructor = sealedPatternConstructor

tsApplyPatternConstructor ::
       forall ts. (TypeSystem ts, MonadThrow ExpressionError (TSScoped ts))
    => TSPatternConstructor ts
    -> TSSealedPattern ts
    -> TSScoped ts (TSPatternConstructor ts)
tsApplyPatternConstructor = applyPatternConstructor @(TSRenamer ts) @(TSUnifier ts)
