module Language.Expression.Common.CompleteTypeSystem where

import Data.Shim
import Language.Expression.Common.Abstract
import Language.Expression.Common.Bindings
import Language.Expression.Common.Error
import Language.Expression.Common.Rename.RenameTypeSystem
import Language.Expression.Common.Sealed
import Language.Expression.Common.Subsumer
import Language.Expression.Common.TypeSystem
import Language.Expression.Common.Unifier
import Shapes

class (AbstractTypeSystem ts, SubsumeTypeSystem ts) => CompleteTypeSystem (ts :: Type) where
    tsFunctionPosWitness :: forall a b. TSNegWitness ts a -> TSPosWitness ts b -> TSPosShimWit ts (a -> b)
    tsFunctionNegWitness :: forall a b. TSPosWitness ts a -> TSNegWitness ts b -> TSNegShimWit ts (a -> b)

type TSValue ts = AnyValue (TSPosShimWit ts)

tsFunctionPosShimWit ::
       forall ts. CompleteTypeSystem ts
    => FunctionWitness (TSNegShimWit ts) (TSPosShimWit ts)
tsFunctionPosShimWit ta tb =
    unNegShimWit ta $ \wa conva ->
        unPosShimWit tb $ \wb convb -> mapPosShimWit (funcShim conva convb) $ tsFunctionPosWitness @ts wa wb

tsFunctionNegShimWit ::
       forall ts. CompleteTypeSystem ts
    => FunctionWitness (TSPosShimWit ts) (TSNegShimWit ts)
tsFunctionNegShimWit ta tb =
    unPosShimWit ta $ \wa conva ->
        unNegShimWit tb $ \wb convb -> mapNegShimWit (funcShim conva convb) $ tsFunctionNegWitness @ts wa wb

tsUnify ::
       forall ts a b. CompleteTypeSystem ts
    => TSPosShimWit ts a
    -> TSNegShimWit ts b
    -> TSInner ts (TSShim ts a b)
tsUnify wa wb = runRenamer @ts $ solveUnifyPosNegShimWit @ts wa wb

tsEval ::
       forall ts m. (MonadThrow ExpressionError m, Show (TSName ts))
    => TSSealedExpression ts
    -> m (TSValue ts)
tsEval = evalSealedExpression

tsUnifyValue ::
       forall ts t. CompleteTypeSystem ts
    => TSNegShimWit ts t
    -> TSValue ts
    -> TSInner ts t
tsUnifyValue witn (MkAnyValue witp val) = do
    conv <- tsUnify @ts witp witn
    return $ shimToFunction conv val

tsSubsume ::
       forall ts inf decl. CompleteTypeSystem ts
    => TSPosShimWit ts inf
    -> TSPosWitness ts decl
    -> TSInner ts (TSShim ts inf decl)
tsSubsume winf tdecl = runRenamer @ts $ solveSubsumeShimWit @ts winf tdecl

tsSubsumeValue ::
       forall ts t. CompleteTypeSystem ts
    => TSPosWitness ts t
    -> TSValue ts
    -> TSInner ts t
tsSubsumeValue tdecl (MkAnyValue winf val) = do
    conv <- tsSubsume @ts winf tdecl
    return $ shimToFunction conv val

tsEvalToType ::
       forall ts t. (CompleteTypeSystem ts, MonadThrow ExpressionError (TSInner ts), Show (TSName ts))
    => TSNegShimWit ts t
    -> TSSealedExpression ts
    -> TSInner ts t
tsEvalToType witn expr = do
    aval <- tsEval @ts expr
    tsUnifyValue @ts witn aval

tsApply ::
       forall ts. CompleteTypeSystem ts
    => TSSealedExpression ts
    -> TSSealedExpression ts
    -> TSInner ts (TSSealedExpression ts)
tsApply tf ta = applySealedExpression @ts (tsFunctionNegShimWit @ts) tf ta

tsAbstract ::
       forall ts. CompleteTypeSystem ts
    => TSName ts
    -> TSSealedExpression ts
    -> TSInner ts (TSSealedExpression ts)
tsAbstract n expr = abstractSealedExpression @ts (tsFunctionPosShimWit @ts) n expr

tsCase ::
       forall ts. CompleteTypeSystem ts
    => TSSealedExpression ts
    -> [(TSSealedPattern ts, TSSealedExpression ts)]
    -> TSInner ts (TSSealedExpression ts)
tsCase expr cases = caseSealedExpression @ts expr cases

tsCaseAbstract ::
       forall ts. CompleteTypeSystem ts
    => [(TSSealedPattern ts, TSSealedExpression ts)]
    -> TSInner ts (TSSealedExpression ts)
tsCaseAbstract cases = caseAbstractSealedExpression @ts (tsFunctionPosShimWit @ts) cases

tsVar ::
       forall ts. CompleteTypeSystem ts
    => TSName ts
    -> TSSealedExpression ts
tsVar name =
    runIdentity $
    runRenamer @ts $
    withTransConstraintTM @Monad $ do
        MkNewVar vwt twt <- renameNewVar @ts
        return $ varSealedExpression name vwt twt

tsConst ::
       forall ts. CompleteTypeSystem ts
    => TSValue ts
    -> TSSealedExpression ts
tsConst = constSealedExpression

tsLet ::
       forall ts. CompleteTypeSystem ts
    => TSName ts
    -> TSSealedExpression ts
    -> TSSealedExpression ts
    -> TSInner ts (TSSealedExpression ts)
tsLet n expv expb = letSealedExpression @ts n expv expb

tsSingleBinding ::
       forall ts. CompleteTypeSystem ts
    => TSName ts
    -> TSSealedExpression ts
    -> Bindings ts
tsSingleBinding = singleBinding

tsUncheckedComponentLet ::
       forall ts. (Ord (TSName ts), CompleteTypeSystem ts)
    => Bindings ts
    -> TSInner ts (Map (TSName ts) (TSSealedExpression ts))
tsUncheckedComponentLet = bindingsComponentLetSealedExpression @ts

tsSubsumeExpression ::
       forall ts. CompleteTypeSystem ts
    => AnyW (TSPosShimWit ts)
    -> TSSealedExpression ts
    -> TSInner ts (TSSealedExpression ts)
tsSubsumeExpression (MkAnyW t) expr =
    runRenamer @ts $ do
        at' <-
            namespace @ts $
            withTransConstraintTM @Monad $ do
                MkShimWit t' _ <- renamePosShimWit @ts t
                return $ MkAnyW t'
        expr' <- rename @ts expr
        subsumeExpression @ts at' expr'

tsVarPattern ::
       forall ts. CompleteTypeSystem ts
    => TSName ts
    -> TSSealedPattern ts
tsVarPattern name =
    runIdentity $
    runRenamer @ts $
    withTransConstraintTM @Monad $ do
        MkNewVar vwt twt <- renameNewVar @ts
        return $ varSealedPattern name vwt twt

tsAnyPattern ::
       forall ts. CompleteTypeSystem ts
    => TSSealedPattern ts
tsAnyPattern =
    runIdentity $
    runRenamer @ts $
    withTransConstraintTM @Monad $ do
        MkNewVar twt _ <- renameNewVar @ts
        return $ anySealedPattern twt

tsBothPattern ::
       forall ts. CompleteTypeSystem ts
    => TSSealedPattern ts
    -> TSSealedPattern ts
    -> TSInner ts (TSSealedPattern ts)
tsBothPattern = bothSealedPattern @ts

tsSealPatternConstructor ::
       forall ts m. MonadThrow ExpressionError m
    => TSPatternConstructor ts
    -> m (TSSealedPattern ts)
tsSealPatternConstructor = sealedPatternConstructor

tsApplyPatternConstructor ::
       forall ts. (CompleteTypeSystem ts, MonadThrow ExpressionError (TSInner ts))
    => TSPatternConstructor ts
    -> TSSealedPattern ts
    -> TSInner ts (TSPatternConstructor ts)
tsApplyPatternConstructor = applyPatternConstructor @ts
