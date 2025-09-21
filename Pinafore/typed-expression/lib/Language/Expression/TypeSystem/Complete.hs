module Language.Expression.TypeSystem.Complete where

import Data.Shim
import Shapes

import Language.Expression.Common
import Language.Expression.TypeSystem.Abstract
import Language.Expression.TypeSystem.Bindings
import Language.Expression.TypeSystem.Rename
import Language.Expression.TypeSystem.Simplify
import Language.Expression.TypeSystem.Subsume
import Language.Expression.TypeSystem.TypeSystem
import Language.Expression.TypeSystem.TypeVariable
import Language.Expression.TypeSystem.Unify

class
    ( AbstractTypeSystem ts
    , SubsumeTypeSystem ts
    , SimplifyTypeSystem ts
    , MonadThrow (ExpressionError (TSVarWit ts)) (TSInner ts)
    ) =>
    CompleteTypeSystem (ts :: Type)
    where
    tsFunctionPosWitness :: forall a b. TSNegWitness ts a -> TSPosWitness ts b -> TSPosShimWit ts (a -> b)
    tsFunctionNegWitness :: forall a b. TSPosWitness ts a -> TSNegWitness ts b -> TSNegShimWit ts (a -> b)

type TSValue ts = SomeOf (TSPosShimWit ts)

type TSValueF ts f = SomeFor f (TSPosShimWit ts)

tsFunctionPosShimWit ::
    forall ts.
    CompleteTypeSystem ts =>
    FunctionWitness (TSNegShimWit ts) (TSPosShimWit ts)
tsFunctionPosShimWit ta tb =
    unNegShimWit ta $ \wa conva ->
        unPosShimWit tb $ \wb convb -> mapPosShimWit (funcShim conva convb) $ tsFunctionPosWitness @ts wa wb

tsFunctionNegShimWit ::
    forall ts.
    CompleteTypeSystem ts =>
    FunctionWitness (TSPosShimWit ts) (TSNegShimWit ts)
tsFunctionNegShimWit ta tb =
    unPosShimWit ta $ \wa conva ->
        unNegShimWit tb $ \wb convb -> mapNegShimWit (funcShim conva convb) $ tsFunctionNegWitness @ts wa wb

tsEval ::
    forall ts m.
    MonadThrow (ExpressionError (TSVarWit ts)) m =>
    TSSealedExpression ts ->
    m (TSValue ts)
tsEval = evalSealedExpression

tsEvalMaybe :: forall ts. TSSealedExpression ts -> Maybe (TSValue ts)
tsEvalMaybe = evalSealedExpressionMaybe

tsUnifyRigid ::
    forall ts a b.
    CompleteTypeSystem ts =>
    TSPosShimWit ts a ->
    TSNegShimWit ts b ->
    TSInner ts (TSShim ts a b)
tsUnifyRigid witp witn =
    runRenamer @ts (renameableVars witp <> renameableVars witn) [] $ do
        uconv <- unifyPosNegShimWit @ts (uuLiftPosShimWit @ts witp) (uuLiftNegShimWit @ts witn)
        convexpr <- unifierSolve @ts uconv return
        lift $ evalExpression convexpr

-- | for debugging
tsUnifyRigidValue ::
    forall ts t.
    (CompleteTypeSystem ts, FromPolarShimWit (TSShim ts) (TSNegWitness ts) t) =>
    TSValue ts ->
    TSInner ts t
tsUnifyRigidValue (MkSomeOf witp val) = let
    witn = fromPolarShimWit
    in do
        conv <- tsUnifyRigid @ts witp witn
        return $ shimToFunction conv val

tsUnifyExpressionTo ::
    forall ts t.
    CompleteTypeSystem ts =>
    TSNegShimWit ts t ->
    TSSealedExpression ts ->
    TSInner ts (TSOpenExpression ts t)
tsUnifyExpressionTo witn (MkSealedExpression witp expr) =
    runRenamer @ts (renameableVars witn) [] $ do
        witp' <- renameMappableSimple @ts witp
        uconv <- unifyPosNegShimWit @ts (uuLiftPosShimWit @ts witp') (uuLiftNegShimWit @ts witn)
        unifierSolve @ts uconv $ \convexpr -> return $ liftA2 shimToFunction convexpr expr

tsUnifyValueTo ::
    forall ts t.
    CompleteTypeSystem ts =>
    TSNegShimWit ts t ->
    TSValue ts ->
    TSInner ts t
tsUnifyValueTo witn (MkSomeOf witp val) =
    runRenamer @ts (renameableVars witn) [] $ do
        witp' <- renameMappableSimple @ts witp
        uconv <- unifyPosNegShimWit @ts (uuLiftPosShimWit @ts witp') (uuLiftNegShimWit @ts witn)
        convexpr <- unifierSolve @ts uconv return
        conv <- lift $ evalExpression convexpr
        return $ shimToFunction conv val

-- | for debugging
tsUnifyValueToFree ::
    forall ts t.
    (CompleteTypeSystem ts, FromPolarShimWit (TSShim ts) (TSNegWitness ts) t) =>
    TSValue ts ->
    TSInner ts t
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
    forall ts t.
    (CompleteTypeSystem ts, FromPolarShimWit (TSShim ts) (TSNegWitness ts) t) =>
    TSValue ts ->
    TSInner ts t
tsUnifyValue = tsUnifyValueTo @ts fromPolarShimWit

tsUnifyF ::
    forall ts f.
    CompleteTypeSystem ts =>
    (forall t. TSNegShimWit ts t -> TSNegShimWit ts (f t)) ->
    TSValue ts ->
    TSInner ts (TSValueF ts f)
tsUnifyF mapwit (MkSomeOf witp val) =
    runRenamer @ts [] [] $ do
        witp' <- renameMappableSimple @ts witp
        MkNewVar _ varn varp <- renameNewFreeVar @ts
        uconv <- unifyPosNegShimWit @ts (uuLiftPosShimWit @ts witp') (uuLiftNegShimWit @ts (mapwit varn))
        unifierSolve @ts uconv $ \convexpr -> do
            conv <- lift $ evalExpression convexpr
            return $ MkSomeFor varp (shimToFunction conv val)

tsSubsume ::
    forall ts inf decl.
    CompleteTypeSystem ts =>
    TSPosShimWit ts inf ->
    TSPosWitness ts decl ->
    TSInner ts (TSOpenExpression ts (TSShim ts inf decl))
tsSubsume winf tdecl =
    runRenamer @ts (renameableVars tdecl) [] $ do
        winf' <- renameMappableSimple @ts winf
        solveSubsumeShimWit @ts winf' tdecl

tsSubsumeValue ::
    forall ts t.
    CompleteTypeSystem ts =>
    TSPosWitness ts t ->
    TSValue ts ->
    TSInner ts t
tsSubsumeValue tdecl (MkSomeOf winf val) = do
    convexpr <- tsSubsume @ts winf tdecl
    conv <- evalExpression convexpr
    return $ shimToFunction conv val

tsApply ::
    forall ts.
    CompleteTypeSystem ts =>
    TSSealedExpression ts ->
    TSSealedExpression ts ->
    TSInner ts (TSSealedExpression ts)
tsApply = applySealedExpression @ts (tsFunctionNegShimWit @ts)

tsSubstitute ::
    forall ts.
    CompleteTypeSystem ts =>
    (TSVarID ts -> Maybe (TSSealedExpression ts)) ->
    TSSealedExpression ts ->
    TSInner ts (TSSealedExpression ts)
tsSubstitute = substituteSealedExpression @ts

tsAbstract ::
    forall ts.
    CompleteTypeSystem ts =>
    TSVarID ts ->
    TSSealedExpression ts ->
    TSInner ts (TSSealedExpression ts)
tsAbstract = abstractSealedExpression @ts (tsFunctionPosShimWit @ts)

tsPolyAbstractF ::
    forall ts p q.
    CompleteTypeSystem ts =>
    TSVarID ts ->
    TSPosShimWit ts p ->
    TSSealedFExpression ts ((->) q) ->
    TSInner ts (TSSealedFExpression ts ((->) (p, q)))
tsPolyAbstractF = polyAbstractSealedFExpression @ts

tsSimplify ::
    forall ts a.
    (CompleteTypeSystem ts, TSMappable ts a) =>
    a ->
    TSInner ts a
tsSimplify a = runRenamer @ts [] [] $ unEndoM (simplify @ts) a

tsVar ::
    forall ts.
    CompleteTypeSystem ts =>
    TSVarID ts ->
    TSSealedExpression ts
tsVar name =
    runIdentity
        $ runRenamer @ts [] []
        $ withTransConstraintTM @Monad
        $ do
            MkNewVar _ vwt twt <- renameNewFreeVar @ts
            return $ varSealedExpression name vwt twt

tsConst :: forall ts. TSValue ts -> TSSealedExpression ts
tsConst = constSealedExpression

tsLet ::
    forall ts.
    CompleteTypeSystem ts =>
    TSVarID ts ->
    TSSealedExpression ts ->
    TSSealedExpression ts ->
    TSInner ts (TSSealedExpression ts)
tsLet n expv expb = letSealedExpression @ts n expv expb

tsSingleBinding ::
    forall ts.
    CompleteTypeSystem ts =>
    TSVarID ts ->
    (TSSealedExpression ts -> TSBindingData ts) ->
    Maybe (Some (TSPosWitness ts)) ->
    TSSealedExpression ts ->
    TSBinding ts
tsSingleBinding name bdf madecltype expr =
    singleBinding name bdf (isJust madecltype) $ do
        madecltype' <- unEndoM (endoFor $ renameTypeSignature @ts) madecltype
        expr' <- renameMappableSimple @ts expr
        subsumerExpression madecltype' expr'

tsSubsumeExpressionTo ::
    forall ts t.
    CompleteTypeSystem ts =>
    ListSet SomeTypeVarT ->
    TSPosWitness ts t ->
    TSSealedExpression ts ->
    TSInner ts (TSOpenExpression ts t)
tsSubsumeExpressionTo freevars tdecl expr = let
    fixednames = renameableVars tdecl
    freenames = fmap someTypeVarTName $ toList freevars
    rigidnames = fixednames \\ freenames
    in runRenamer @ts rigidnames freenames $ do
        expr' <- renameMappableSimple @ts expr
        subsumeExpressionTo @ts tdecl expr'

tsSubsumeExpression ::
    forall ts.
    CompleteTypeSystem ts =>
    Some (TSPosWitness ts) ->
    TSSealedExpression ts ->
    TSInner ts (TSSealedExpression ts)
tsSubsumeExpression stdecl@(MkSome tdecl) expr =
    runRenamer @ts (renameableVars tdecl) []
        $ withTransConstraintTM @Monad
        $ do
            expr' <- renameMappableSimple @ts expr
            subsumeExpression @ts stdecl expr'

tsSubsumeFExpression ::
    forall ts f.
    (CompleteTypeSystem ts, Functor f) =>
    Some (TSPosWitness ts) ->
    TSSealedFExpression ts f ->
    TSInner ts (TSSealedFExpression ts f)
tsSubsumeFExpression stdecl@(MkSome tdecl) expr =
    runRenamer @ts (renameableVars tdecl) []
        $ withTransConstraintTM @Monad
        $ do
            expr' <- renameMappableSimple @ts expr
            subsumeFExpression @ts stdecl expr'

tsUncheckedRecursiveLet ::
    forall ts.
    CompleteTypeSystem ts =>
    [TSBinding ts] ->
    TSInner ts (Map (TSVarID ts) (TSBindingData ts, TSSealedExpression ts))
tsUncheckedRecursiveLet = bindingsRecursiveLetSealedExpression @ts

tsSequentialLet ::
    forall ts.
    CompleteTypeSystem ts =>
    TSBinding ts ->
    TSInner ts (Map (TSVarID ts) (TSBindingData ts, TSSealedExpression ts))
tsSequentialLet = bindingSequentialLetSealedExpression @ts

tsVarPattern ::
    forall ts.
    CompleteTypeSystem ts =>
    TSVarID ts ->
    TSSealedPattern ts
tsVarPattern name =
    runIdentity
        $ runRenamer @ts [] []
        $ withTransConstraintTM @Monad
        $ do
            MkNewVar _ vwt twt <- renameNewFreeVar @ts
            return $ varNamedSealedPattern name vwt twt

tsAnyPattern ::
    forall ts.
    CompleteTypeSystem ts =>
    TSSealedPattern ts
tsAnyPattern =
    runIdentity
        $ runRenamer @ts [] []
        $ withTransConstraintTM @Monad
        $ do
            MkNewVar _ twt _ <- renameNewFreeVar @ts
            return $ anySealedPattern twt

tsBothPattern ::
    forall ts.
    CompleteTypeSystem ts =>
    TSSealedPattern ts ->
    TSSealedPattern ts ->
    TSInner ts (TSSealedPattern ts)
tsBothPattern = bothSealedPattern @ts

tsSealPatternConstructor ::
    forall ts m.
    MonadThrow PatternError m =>
    TSPatternConstructor ts ->
    m (TSSealedPattern ts)
tsSealPatternConstructor = sealedPatternConstructor

tsApplyPatternConstructor ::
    forall ts.
    CompleteTypeSystem ts =>
    TSPatternConstructor ts ->
    TSSealedPattern ts ->
    TSInner ts (TSPatternConstructor ts)
tsApplyPatternConstructor = applyPatternConstructor @ts
