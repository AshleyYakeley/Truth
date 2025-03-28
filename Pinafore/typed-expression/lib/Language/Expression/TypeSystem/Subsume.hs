module Language.Expression.TypeSystem.Subsume
    ( SubsumeTypeSystem (..)
    , ShowSubsumeTypeSystem (..)
    , solveSubsumerExpression
    , usubSolveSubsumer
    , subsumePosShimWit
    , solveSubsumeShimWit
    , subsumerSubstitute
    , OpenSubsumerExpression
    , SealedSubsumerExpression (..)
    , subsumerExpression
    , subsumerExpressionTo
    , subsumeExpression
    , subsumeFExpression
    , subsumeExpressionTo
    )
where

import Data.Shim
import Shapes

import Language.Expression.Common
import Language.Expression.TypeSystem.Simplify
import Language.Expression.TypeSystem.SolverExpression
import Language.Expression.TypeSystem.TypeSystem
import Language.Expression.TypeSystem.Unify

type OpenSubsumerExpression ts = TSOpenSolverExpression ts (Subsumer ts)

class (UnifyTypeSystem ts, Applicative (Subsumer ts), RecoverShim (TSShim ts)) => SubsumeTypeSystem ts where
    type Subsumer ts :: Type -> Type
    type SubsumerSubstitutions ts :: Type
    usubSubsumer :: forall a. UnifierSubstitutions ts -> Subsumer ts a -> TSOuter ts (OpenSubsumerExpression ts a)
    solveSubsumer :: forall a. Subsumer ts a -> TSOuter ts (TSOpenExpression ts a, SubsumerSubstitutions ts)
    subsumerPosSubstitute :: SubsumerSubstitutions ts -> TSPosWitness ts t -> TSOuter ts (TSPosShimWit ts t)
    subsumerNegSubstitute :: SubsumerSubstitutions ts -> TSNegWitness ts t -> TSOuter ts (TSNegShimWit ts t)
    subsumePosWitnesses ::
        TSPosWitness ts inf -> TSPosWitness ts decl -> TSOuter ts (OpenSubsumerExpression ts (TSShim ts inf decl))

class (ShowTypeSystem ts, Show (SubsumerSubstitutions ts)) => ShowSubsumeTypeSystem ts where
    showSubsumer :: forall a. Subsumer ts a -> String
    default showSubsumer :: AllConstraint Show (Subsumer ts) => forall a. Subsumer ts a -> String
    showSubsumer = allShow

solveSubsumerExpression ::
    forall ts a.
    SubsumeTypeSystem ts =>
    OpenSubsumerExpression ts a ->
    TSOuter ts (TSOpenExpression ts a, SubsumerSubstitutions ts)
solveSubsumerExpression (MkSolverExpression texpr vexpr) = do
    (expr, subs) <- solveSubsumer @ts texpr
    return (vexpr <*> expr, subs)

usubSolveSubsumer ::
    forall ts a.
    SubsumeTypeSystem ts =>
    UnifierSubstitutions ts ->
    Subsumer ts a ->
    TSOuter ts (TSOpenExpression ts a, SubsumerSubstitutions ts)
usubSolveSubsumer subs subsumer = do
    sexp <- usubSubsumer @ts subs subsumer
    solveSubsumerExpression @ts sexp

subsumePosShimWit ::
    forall ts inf decl.
    SubsumeTypeSystem ts =>
    TSPosShimWit ts inf ->
    TSPosWitness ts decl ->
    TSOuter ts (OpenSubsumerExpression ts (TSShim ts inf decl))
subsumePosShimWit winf tdecl =
    unPosShimWit winf $ \tinf convinf -> do
        subsumer <- subsumePosWitnesses @ts tinf tdecl
        return $ fmap (\td -> td . convinf) subsumer

solveSubsumeShimWit ::
    forall ts inf decl.
    SubsumeTypeSystem ts =>
    TSPosShimWit ts inf ->
    TSPosWitness ts decl ->
    TSOuter ts (TSOpenExpression ts (TSShim ts inf decl))
solveSubsumeShimWit winf tdecl = do
    subsumer <- subsumePosShimWit @ts winf tdecl
    (ab, _) <- solveSubsumerExpression @ts subsumer
    return ab

subsumerSubstitute ::
    forall ts a.
    (SubsumeTypeSystem ts, TSMappable ts a) =>
    SubsumerSubstitutions ts ->
    EndoM (TSOuter ts) a
subsumerSubstitute subs =
    tsMapWitnessesM
        @ts
        (MkEndoM $ chainPolarShimWitM $ subsumerPosSubstitute @ts subs)
        (MkEndoM $ chainPolarShimWitM $ subsumerNegSubstitute @ts subs)

data SealedSubsumerExpression ts
    = forall tdecl. MkSealedSubsumerExpression
        (TSPosShimWit ts tdecl)
        (OpenSubsumerExpression ts tdecl)

data SealedSubsumerFExpression ts f
    = forall tdecl. MkSealedSubsumerFExpression
        (TSPosShimWit ts tdecl)
        (OpenSubsumerExpression ts (f tdecl))

instance (ShowSubsumeTypeSystem ts, SubsumeTypeSystem ts) => Show (SealedSubsumerExpression ts) where
    show (MkSealedSubsumerExpression t (MkSolverExpression subs expr)) =
        showSubsumer @ts subs <> "/" <> allShow expr <> " => " <> show t

expressionSubsumption ::
    forall ts.
    SubsumeTypeSystem ts =>
    TSSealedExpression ts ->
    SealedSubsumerExpression ts
expressionSubsumption (MkSealedExpression tw expr) = MkSealedSubsumerExpression tw $ solverExpressionLiftValue expr

fExpressionSubsumption ::
    forall ts f.
    SubsumeTypeSystem ts =>
    TSSealedFExpression ts f ->
    SealedSubsumerFExpression ts f
fExpressionSubsumption (MkSealedFExpression tw expr) = MkSealedSubsumerFExpression tw $ solverExpressionLiftValue expr

subsumerExpressionTo ::
    forall ts t.
    (SubsumeTypeSystem ts, SimplifyTypeSystem ts) =>
    TSPosWitness ts t ->
    TSSealedExpression ts ->
    TSOuter ts (OpenSubsumerExpression ts t)
subsumerExpressionTo tdecl (MkSealedExpression tinf expr) = do
    sexpr <- subsumePosShimWit @ts tinf tdecl
    return $ liftA2 shimToFunction sexpr $ solverExpressionLiftValue expr

subsumerFExpressionTo ::
    forall ts f t.
    (SubsumeTypeSystem ts, SimplifyTypeSystem ts, Functor f) =>
    TSPosWitness ts t ->
    TSSealedFExpression ts f ->
    TSOuter ts (OpenSubsumerExpression ts (f t))
subsumerFExpressionTo tdecl (MkSealedFExpression tinf expr) = do
    sexpr <- subsumePosShimWit @ts tinf tdecl
    return $ liftA2 (fmap . shimToFunction) sexpr $ solverExpressionLiftValue expr

-- Note the user's declared type will be simplified first, so they'll end up seeing a simplified version of the type they declared for their expression.
subsumerExpression ::
    forall ts.
    (SubsumeTypeSystem ts, SimplifyTypeSystem ts) =>
    Maybe (Some (TSPosWitness ts)) ->
    TSSealedExpression ts ->
    TSOuter ts (SealedSubsumerExpression ts)
subsumerExpression marawdecltype rawinfexpr = do
    expr <- unEndoM (simplify @ts) rawinfexpr
    case marawdecltype of
        Nothing -> return $ expressionSubsumption expr
        Just (MkSome rawdecltype) -> do
            MkShimWit tdecl _ <- unEndoM (simplify @ts) $ mkPolarShimWit @Type @(TSShim ts) @_ @'Positive rawdecltype
            sexpr <- subsumerExpressionTo @ts tdecl expr
            return $ MkSealedSubsumerExpression (mkPolarShimWit tdecl) sexpr

subsumerFExpression ::
    forall ts f.
    (SubsumeTypeSystem ts, SimplifyTypeSystem ts, Functor f) =>
    Maybe (Some (TSPosWitness ts)) ->
    TSSealedFExpression ts f ->
    TSOuter ts (SealedSubsumerFExpression ts f)
subsumerFExpression marawdecltype rawinfexpr = do
    expr <- unEndoM (simplify @ts) rawinfexpr
    case marawdecltype of
        Nothing -> return $ fExpressionSubsumption expr
        Just (MkSome rawdecltype) -> do
            MkShimWit tdecl _ <- unEndoM (simplify @ts) $ mkPolarShimWit @Type @(TSShim ts) @_ @'Positive rawdecltype
            sexpr <- subsumerFExpressionTo @ts tdecl expr
            return $ MkSealedSubsumerFExpression (mkPolarShimWit tdecl) sexpr

subsumeExpression ::
    forall ts.
    (SubsumeTypeSystem ts, SimplifyTypeSystem ts) =>
    Some (TSPosWitness ts) ->
    TSSealedExpression ts ->
    TSOuter ts (TSSealedExpression ts)
subsumeExpression t expr = do
    MkSealedSubsumerExpression tp sexpr <- subsumerExpression @ts (Just t) expr
    (oexpr, ssubs) <- solveSubsumerExpression @ts sexpr
    oexpr' <- unEndoM (subsumerSubstitute @ts ssubs) oexpr
    return $ MkSealedExpression tp oexpr'

subsumeFExpression ::
    forall ts f.
    (SubsumeTypeSystem ts, SimplifyTypeSystem ts, Functor f) =>
    Some (TSPosWitness ts) ->
    TSSealedFExpression ts f ->
    TSOuter ts (TSSealedFExpression ts f)
subsumeFExpression t expr = do
    MkSealedSubsumerFExpression tp sexpr <- subsumerFExpression @ts (Just t) expr
    (oexpr, ssubs) <- solveSubsumerExpression @ts sexpr
    oexpr' <- unEndoM (subsumerSubstitute @ts ssubs) oexpr
    return $ MkSealedFExpression tp oexpr'

subsumeExpressionTo ::
    forall ts t.
    (SubsumeTypeSystem ts, SimplifyTypeSystem ts) =>
    TSPosWitness ts t ->
    TSSealedExpression ts ->
    TSOuter ts (TSOpenExpression ts t)
subsumeExpressionTo tdecl expr = do
    sexpr <- subsumerExpressionTo @ts tdecl expr
    (oexpr, ssubs) <- solveSubsumerExpression @ts sexpr
    unEndoM (subsumerSubstitute @ts ssubs) oexpr
