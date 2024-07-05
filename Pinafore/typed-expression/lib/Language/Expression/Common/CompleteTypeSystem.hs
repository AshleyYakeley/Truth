module Language.Expression.Common.CompleteTypeSystem where

import Data.Shim
import Language.Expression.Common.Abstract
import Language.Expression.Common.Bindings
import Language.Expression.Common.Error
import Language.Expression.Common.Expression
import Language.Expression.Common.Named
import Language.Expression.Common.Pattern
import Language.Expression.Common.Rename
import Language.Expression.Common.Sealed
import Language.Expression.Common.Simplifier
import Language.Expression.Common.Subsumer
import Language.Expression.Common.TypeSystem
import Language.Expression.Common.TypeVariable
import Language.Expression.Common.Unifier
import Shapes

class ( AbstractTypeSystem ts
      , SubsumeTypeSystem ts
      , SimplifyTypeSystem ts
      , MonadThrow (NamedExpressionError (TSVarID ts) (TSNegShimWit ts)) (TSInner ts)
      ) => CompleteTypeSystem (ts :: Type) where
    tsFunctionPosWitness :: forall a b. TSNegWitness ts a -> TSPosWitness ts b -> TSPosShimWit ts (a -> b)
    tsFunctionNegWitness :: forall a b. TSPosWitness ts a -> TSNegWitness ts b -> TSNegShimWit ts (a -> b)

type TSValue ts = SomeOf (TSPosShimWit ts)

type TSValueF ts f = SomeFor f (TSPosShimWit ts)

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
       forall ts m. (MonadThrow (NamedExpressionError (TSVarID ts) (TSNegShimWit ts)) m)
    => TSSealedExpression ts
    -> m (TSValue ts)
tsEval = evalSealedExpression

-- | for debugging
tsUnifyRigidValue ::
       forall ts t. (CompleteTypeSystem ts, FromPolarShimWit (TSShim ts) (TSNegWitness ts) t)
    => TSValue ts
    -> TSInner ts t
tsUnifyRigidValue (MkSomeOf witp val) = let
    witn = fromPolarShimWit
    in runRenamer @ts (renameableVars witp <> renameableVars witn) [] $ do
           uconv <- unifyPosNegShimWit @ts (uuLiftPosShimWit @ts witp) (uuLiftNegShimWit @ts witn)
           convexpr <- unifierSolve @ts uconv return
           conv <- lift $ evalExpression convexpr
           return $ shimToFunction conv val

tsUnifyExpressionTo ::
       forall ts t. CompleteTypeSystem ts
    => TSNegShimWit ts t
    -> TSSealedExpression ts
    -> TSInner ts (TSOpenExpression ts t)
tsUnifyExpressionTo witn (MkSealedExpression witp expr) =
    runRenamer @ts (renameableVars witn) [] $ do
        witp' <- renameMappableSimple @ts witp
        uconv <- unifyPosNegShimWit @ts (uuLiftPosShimWit @ts witp') (uuLiftNegShimWit @ts witn)
        unifierSolve @ts uconv $ \convexpr -> return $ liftA2 shimToFunction convexpr expr

tsUnifyValueTo ::
       forall ts t. CompleteTypeSystem ts
    => TSNegShimWit ts t
    -> TSValue ts
    -> TSInner ts t
tsUnifyValueTo witn (MkSomeOf witp val) =
    runRenamer @ts (renameableVars witn) [] $ do
        witp' <- renameMappableSimple @ts witp
        uconv <- unifyPosNegShimWit @ts (uuLiftPosShimWit @ts witp') (uuLiftNegShimWit @ts witn)
        convexpr <- unifierSolve @ts uconv return
        conv <- lift $ evalExpression convexpr
        return $ shimToFunction conv val

-- | for debugging
tsUnifyValueToFree ::
       forall ts t. (CompleteTypeSystem ts, FromPolarShimWit (TSShim ts) (TSNegWitness ts) t)
    => TSValue ts
    -> TSInner ts t
tsUnifyValueToFree (MkSomeOf witp val) = let
    witn = fromPolarShimWit
    in runRenamer @ts [] [] $ do
           witp' <- renameMappableSimple @ts witp
           witn' <- renameMappableSimple @ts witn
           uconv <- unifyPosNegShimWit @ts (uuLiftPosShimWit @ts witp') (uuLiftNegShimWit @ts witn')
           convexpr <- unifierSolve @ts uconv return
           conv <- lift $ evalExpression convexpr
           return $ shimToFunction conv val

tsUnifyValue ::
       forall ts t. (CompleteTypeSystem ts, FromPolarShimWit (TSShim ts) (TSNegWitness ts) t)
    => TSValue ts
    -> TSInner ts t
tsUnifyValue = tsUnifyValueTo @ts fromPolarShimWit

tsUnifyF ::
       forall ts f. CompleteTypeSystem ts
    => (forall t. TSNegShimWit ts t -> TSNegShimWit ts (f t))
    -> TSValue ts
    -> TSInner ts (TSValueF ts f)
tsUnifyF mapwit (MkSomeOf witp val) =
    runRenamer @ts [] [] $ do
        witp' <- renameMappableSimple @ts witp
        MkNewVar varn varp <- renameNewFreeVar @ts
        uconv <- unifyPosNegShimWit @ts (uuLiftPosShimWit @ts witp') (uuLiftNegShimWit @ts (mapwit varn))
        unifierSolve @ts uconv $ \convexpr -> do
            conv <- lift $ evalExpression convexpr
            return $ MkSomeFor varp (shimToFunction conv val)

tsSubsume ::
       forall ts inf decl. CompleteTypeSystem ts
    => TSPosShimWit ts inf
    -> TSPosWitness ts decl
    -> TSInner ts (TSOpenExpression ts (TSShim ts inf decl))
tsSubsume winf tdecl =
    runRenamer @ts (renameableVars tdecl) [] $ do
        winf' <- renameMappableSimple @ts winf
        solveSubsumeShimWit @ts winf' tdecl

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

tsSubstitute ::
       forall ts. CompleteTypeSystem ts
    => (TSVarID ts -> Maybe (TSSealedExpression ts))
    -> TSSealedExpression ts
    -> TSInner ts (TSSealedExpression ts)
tsSubstitute = substituteSealedExpression @ts

tsAbstract ::
       forall ts. CompleteTypeSystem ts
    => TSVarID ts
    -> TSSealedExpression ts
    -> TSInner ts (TSSealedExpression ts)
tsAbstract = abstractSealedExpression @ts (tsFunctionPosShimWit @ts)

tsAbstractF ::
       forall ts p q. CompleteTypeSystem ts
    => TSVarID ts
    -> TSPosShimWit ts p
    -> TSSealedFExpression ts ((->) q)
    -> TSInner ts (TSSealedFExpression ts ((->) (p, q)))
tsAbstractF = abstractSealedFExpression @ts

tsSimplify ::
       forall ts a. (CompleteTypeSystem ts, TSMappable ts a)
    => a
    -> TSInner ts a
tsSimplify a = runRenamer @ts [] [] $ unEndoM (simplify @ts) a

tsVar ::
       forall ts. CompleteTypeSystem ts
    => TSVarID ts
    -> TSSealedExpression ts
tsVar name =
    runIdentity $
    runRenamer @ts [] [] $
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
    -> (TSSealedExpression ts -> TSBindingData ts)
    -> Maybe (Some (TSPosWitness ts))
    -> TSSealedExpression ts
    -> TSBinding ts
tsSingleBinding name bdf madecltype expr =
    singleBinding name bdf (isJust madecltype) $ do
        madecltype' <- unEndoM (endoFor $ renameTypeSignature @ts) madecltype
        expr' <- renameMappableSimple @ts expr
        subsumerExpression madecltype' expr'

tsSubsumeExpressionTo ::
       forall ts t. CompleteTypeSystem ts
    => FiniteSet SomeTypeVarT
    -> TSPosWitness ts t
    -> TSSealedExpression ts
    -> TSInner ts (TSOpenExpression ts t)
tsSubsumeExpressionTo freevars tdecl expr = let
    fixednames = renameableVars tdecl
    freenames = fmap someTypeVarName $ toList freevars
    rigidnames = fixednames \\ freenames
    in runRenamer @ts rigidnames freenames $ do
           expr' <- renameMappableSimple @ts expr
           subsumeExpressionTo @ts tdecl expr'

tsSubsumeExpression ::
       forall ts. CompleteTypeSystem ts
    => Some (TSPosWitness ts)
    -> TSSealedExpression ts
    -> TSInner ts (TSSealedExpression ts)
tsSubsumeExpression stdecl@(MkSome tdecl) expr =
    runRenamer @ts (renameableVars tdecl) [] $
    withTransConstraintTM @Monad $ do
        expr' <- renameMappableSimple @ts expr
        subsumeExpression @ts stdecl expr'

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
    runRenamer @ts [] [] $
    withTransConstraintTM @Monad $ do
        MkNewVar vwt twt <- renameNewFreeVar @ts
        return $ varSealedExpressionPattern name vwt $ mapShimWit (MkPolarShim meet1) twt

tsAnyPattern ::
       forall ts. CompleteTypeSystem ts
    => TSSealedExpressionPattern ts
tsAnyPattern =
    runIdentity $
    runRenamer @ts [] [] $
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
       forall ts m. MonadThrow PatternError m
    => TSExpressionPatternConstructor ts
    -> m (TSSealedExpressionPattern ts)
tsSealPatternConstructor = sealedPatternConstructor

tsApplyPatternConstructor ::
       forall ts. CompleteTypeSystem ts
    => TSExpressionPatternConstructor ts
    -> TSSealedExpressionPattern ts
    -> TSInner ts (TSExpressionPatternConstructor ts)
tsApplyPatternConstructor = applyPatternConstructor @ts
