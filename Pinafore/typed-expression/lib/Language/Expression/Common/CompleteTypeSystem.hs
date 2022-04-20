module Language.Expression.Common.CompleteTypeSystem where

import Data.Shim
import Language.Expression.Common.Abstract
import Language.Expression.Common.Bindings
import Language.Expression.Common.Error
import Language.Expression.Common.Rename
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

tsEval ::
       forall ts m. (MonadThrow ExpressionError m, Show (TSVarID ts), AllWitnessConstraint Show (TSNegWitness ts))
    => TSSealedExpression ts
    -> m (TSValue ts)
tsEval = evalSealedExpression

-- | for debugging
tsUnifyRigidValue ::
       forall ts t. (CompleteTypeSystem ts, FromPolarShimWit (TSShim ts) (TSNegWitness ts) t)
    => TSValue ts
    -> TSInner ts t
tsUnifyRigidValue (MkAnyValue witp val) =
    runRenamer @ts $ do
        witp' <- rename @ts RigidName witp
        witn' <- rename @ts RigidName fromPolarShimWit
        conv <- solveUnifyPosNegShimWit @ts witp' witn'
        return $ shimToFunction conv val

tsUnifyValue ::
       forall ts t. (CompleteTypeSystem ts, FromPolarShimWit (TSShim ts) (TSNegWitness ts) t)
    => TSValue ts
    -> TSInner ts t
tsUnifyValue (MkAnyValue witp val) =
    runRenamer @ts $ do
        witp' <- rename @ts FreeName witp
        witn' <- rename @ts RigidName fromPolarShimWit
        conv <- solveUnifyPosNegShimWit @ts witp' witn'
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

tsApply ::
       forall ts. CompleteTypeSystem ts
    => TSSealedExpression ts
    -> TSSealedExpression ts
    -> TSInner ts (TSSealedExpression ts)
tsApply tf ta = applySealedExpression @ts (tsFunctionNegShimWit @ts) tf ta

tsAbstract ::
       forall ts. CompleteTypeSystem ts
    => TSVarID ts
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
    => TSVarID ts
    -> TSSealedExpression ts
tsVar name =
    runIdentity $
    runRenamer @ts $
    withTransConstraintTM @Monad $ do
        MkNewVar vwt twt <- renameNewFreeVar @ts
        return $ varSealedExpression name vwt twt

tsConst ::
       forall ts. CompleteTypeSystem ts
    => TSValue ts
    -> TSSealedExpression ts
tsConst = constSealedExpression

tsLet ::
       forall ts. CompleteTypeSystem ts
    => TSVarID ts
    -> TSSealedExpression ts
    -> TSSealedExpression ts
    -> TSInner ts (TSSealedExpression ts)
tsLet n expv expb = letSealedExpression @ts n expv expb

tsSingleBinding ::
       forall ts. CompleteTypeSystem ts
    => TSVarID ts
    -> TSBindingData ts
    -> Maybe (AnyW (TSPosWitness ts))
    -> TSSealedExpression ts
    -> Binding ts
tsSingleBinding name bd madecltype expr =
    singleBinding name bd $ do
        madecltype' <- for madecltype $ renameTypeSignature @ts
        expr' <- rename @ts FreeName expr
        subsumerExpression madecltype' expr'

tsSubsumeExpression ::
       forall ts. CompleteTypeSystem ts
    => AnyW (TSPosWitness ts)
    -> TSSealedExpression ts
    -> TSInner ts (TSSealedExpression ts)
tsSubsumeExpression decltype expr =
    runRenamer @ts $
    withTransConstraintTM @Monad $ do
        decltype' <- renameTypeSignature @ts decltype
        expr' <- rename @ts FreeName expr
        subsumeExpression @ts decltype' expr'

tsUncheckedRecursiveLet ::
       forall ts. (Ord (TSVarID ts), CompleteTypeSystem ts)
    => [Binding ts]
    -> TSInner ts (Map (TSVarID ts) (TSBindingData ts, TSSealedExpression ts))
tsUncheckedRecursiveLet = bindingsRecursiveLetSealedExpression @ts

tsSequentialLet ::
       forall ts. (Ord (TSVarID ts), CompleteTypeSystem ts)
    => Binding ts
    -> TSInner ts (Map (TSVarID ts) (TSBindingData ts, TSSealedExpression ts))
tsSequentialLet = bindingSequentialLetSealedExpression @ts

tsVarPattern ::
       forall ts. CompleteTypeSystem ts
    => TSVarID ts
    -> TSSealedPattern ts
tsVarPattern name =
    runIdentity $
    runRenamer @ts $
    withTransConstraintTM @Monad $ do
        MkNewVar vwt twt <- renameNewFreeVar @ts
        return $ varSealedPattern name vwt twt

tsAnyPattern ::
       forall ts. CompleteTypeSystem ts
    => TSSealedPattern ts
tsAnyPattern =
    runIdentity $
    runRenamer @ts $
    withTransConstraintTM @Monad $ do
        MkNewVar twt _ <- renameNewFreeVar @ts
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
       forall ts. CompleteTypeSystem ts
    => TSPatternConstructor ts
    -> TSSealedPattern ts
    -> TSInner ts (TSPatternConstructor ts)
tsApplyPatternConstructor = applyPatternConstructor @ts
