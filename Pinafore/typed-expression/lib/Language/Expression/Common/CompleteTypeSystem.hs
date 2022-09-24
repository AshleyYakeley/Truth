module Language.Expression.Common.CompleteTypeSystem where

import Data.Shim
import Language.Expression.Common.Abstract
import Language.Expression.Common.Bindings
import Language.Expression.Common.Error
import Language.Expression.Common.Expression
import Language.Expression.Common.Pattern
import Language.Expression.Common.Rename
import Language.Expression.Common.Sealed
import Language.Expression.Common.Subsumer
import Language.Expression.Common.TypeSystem
import Language.Expression.Common.Unifier
import Shapes

class (AbstractTypeSystem ts, SubsumeTypeSystem ts) => CompleteTypeSystem (ts :: Type) where
    tsFunctionPosWitness :: forall a b. TSNegWitness ts a -> TSPosWitness ts b -> TSPosShimWit ts (a -> b)
    tsFunctionNegWitness :: forall a b. TSPosWitness ts a -> TSNegWitness ts b -> TSNegShimWit ts (a -> b)

type TSValue ts = SomeOf (TSPosShimWit ts)

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
       forall ts m. (MonadThrow ExpressionError m, Show (TSVarID ts), AllConstraint Show (TSNegWitness ts))
    => TSSealedExpression ts
    -> m (TSValue ts)
tsEval = evalSealedExpression

-- | for debugging
tsUnifyRigidValue ::
       forall ts t. (CompleteTypeSystem ts, FromPolarShimWit (TSShim ts) (TSNegWitness ts) t)
    => TSValue ts
    -> TSInner ts t
tsUnifyRigidValue (MkSomeOf witp val) =
    runRenamer @ts $ do
        witp' <- rename @ts RigidName witp
        witn' <- rename @ts RigidName fromPolarShimWit
        (convexpr, _) <- unifyPosNegShimWit @ts witp' witn'
        conv <- lift $ evalExpression convexpr
        return $ shimToFunction conv val

tsUnifyExpressionTo ::
       forall ts t. CompleteTypeSystem ts
    => TSNegShimWit ts t
    -> TSSealedExpression ts
    -> TSInner ts (TSOpenExpression ts t)
tsUnifyExpressionTo witn (MkSealedExpression witp expr) =
    runRenamer @ts $ do
        witp' <- rename @ts FreeName witp
        witn' <- rename @ts RigidName witn
        uconv <- unifyUUPosNegShimWit @ts (uuLiftPosShimWit @ts witp') (uuLiftNegShimWit @ts witn')
        unifierSolveSubstituteSimplifyFinalRename @ts (uuGetShim @ts uconv) $ \convexpr ->
            liftA2 shimToFunction convexpr expr

tsUnifyValueTo ::
       forall ts t. CompleteTypeSystem ts
    => TSNegShimWit ts t
    -> TSValue ts
    -> TSInner ts t
tsUnifyValueTo witn (MkSomeOf witp val) =
    runRenamer @ts $ do
        witp' <- rename @ts FreeName witp
        witn' <- rename @ts RigidName witn
        (convexpr, _) <- unifyPosNegShimWit @ts witp' witn'
        conv <- lift $ evalExpression convexpr
        return $ shimToFunction conv val

tsUnifyValue ::
       forall ts t. (CompleteTypeSystem ts, FromPolarShimWit (TSShim ts) (TSNegWitness ts) t)
    => TSValue ts
    -> TSInner ts t
tsUnifyValue = tsUnifyValueTo @ts fromPolarShimWit

tsSubsume ::
       forall ts inf decl. CompleteTypeSystem ts
    => TSPosShimWit ts inf
    -> TSPosWitness ts decl
    -> TSInner ts (TSOpenExpression ts (TSShim ts inf decl))
tsSubsume winf tdecl = runRenamer @ts $ solveSubsumeShimWit @ts winf tdecl

tsSubsumeValue ::
       forall ts t. CompleteTypeSystem ts
    => TSPosWitness ts t
    -> TSValue ts
    -> TSInner ts t
tsSubsumeValue tdecl (MkSomeOf winf val) = do
    convexpr <- tsSubsume @ts winf tdecl
    conv <- evalExpression convexpr
    return $ shimToFunction conv val

tsApply ::
       forall ts. CompleteTypeSystem ts
    => TSSealedExpression ts
    -> TSSealedExpression ts
    -> TSInner ts (TSSealedExpression ts)
tsApply = applySealedExpression @ts (tsFunctionNegShimWit @ts)

tsAbstract ::
       forall ts. CompleteTypeSystem ts
    => TSVarID ts
    -> TSSealedExpression ts
    -> TSInner ts (TSSealedExpression ts)
tsAbstract = abstractSealedExpression @ts (tsFunctionPosShimWit @ts)

tsCase ::
       forall ts. CompleteTypeSystem ts
    => TSSealedExpression ts
    -> [(TSSealedExpressionPattern ts, TSSealedExpression ts)]
    -> TSInner ts (TSSealedExpression ts)
tsCase = caseSealedExpression @ts

tsCaseAbstract ::
       forall ts. CompleteTypeSystem ts
    => [(TSSealedExpressionPattern ts, TSSealedExpression ts)]
    -> TSInner ts (TSSealedExpression ts)
tsCaseAbstract = caseAbstractSealedExpression @ts (tsFunctionPosShimWit @ts)

tsMultiCaseAbstract ::
       forall ts n. CompleteTypeSystem ts
    => PeanoNatType n
    -> [(FixedList n (TSSealedExpressionPattern ts), TSSealedExpression ts)]
    -> TSInner ts (TSSealedExpression ts)
tsMultiCaseAbstract = multiCaseAbstractSealedExpression @ts (tsFunctionPosShimWit @ts)

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
    -> Maybe (Some (TSPosWitness ts))
    -> TSSealedExpression ts
    -> TSBinding ts
tsSingleBinding name bd madecltype expr =
    singleBinding name bd $ do
        madecltype' <- for madecltype $ renameTypeSignature @ts
        expr' <- rename @ts FreeName expr
        subsumerExpression madecltype' expr'

tsSubsumeExpression ::
       forall ts. CompleteTypeSystem ts
    => Some (TSPosWitness ts)
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
    => [TSBinding ts]
    -> TSInner ts (Map (TSVarID ts) (TSBindingData ts, TSSealedExpression ts))
tsUncheckedRecursiveLet = bindingsRecursiveLetSealedExpression @ts

tsSequentialLet ::
       forall ts. (Ord (TSVarID ts), CompleteTypeSystem ts)
    => TSBinding ts
    -> TSInner ts (Map (TSVarID ts) (TSBindingData ts, TSSealedExpression ts))
tsSequentialLet = bindingSequentialLetSealedExpression @ts

tsVarPattern ::
       forall ts. CompleteTypeSystem ts
    => TSVarID ts
    -> TSSealedExpressionPattern ts
tsVarPattern name =
    runIdentity $
    runRenamer @ts $
    withTransConstraintTM @Monad $ do
        MkNewVar vwt twt <- renameNewFreeVar @ts
        return $ varSealedExpressionPattern name vwt $ mapShimWit (MkPolarMap meet1) twt

tsAnyPattern ::
       forall ts. CompleteTypeSystem ts
    => TSSealedExpressionPattern ts
tsAnyPattern =
    runIdentity $
    runRenamer @ts $
    withTransConstraintTM @Monad $ do
        MkNewVar twt _ <- renameNewFreeVar @ts
        return $ anySealedExpressionPattern twt

tsBothPattern ::
       forall ts. CompleteTypeSystem ts
    => TSSealedExpressionPattern ts
    -> TSSealedExpressionPattern ts
    -> TSInner ts (TSSealedExpressionPattern ts)
tsBothPattern = bothSealedPattern @ts

tsSealPatternConstructor ::
       forall ts m. MonadThrow ExpressionError m
    => TSExpressionPatternConstructor ts
    -> m (TSSealedExpressionPattern ts)
tsSealPatternConstructor = sealedPatternConstructor

tsApplyPatternConstructor ::
       forall ts. CompleteTypeSystem ts
    => TSExpressionPatternConstructor ts
    -> TSSealedExpressionPattern ts
    -> TSInner ts (TSExpressionPatternConstructor ts)
tsApplyPatternConstructor = applyPatternConstructor @ts
