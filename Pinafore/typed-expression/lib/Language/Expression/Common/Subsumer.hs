module Language.Expression.Common.Subsumer
    ( SubsumeTypeSystem(..)
    , usubSubsumerExpression
    , usubSolveSubsumer
    , subsumePosShimWit
    , solveSubsumeShimWit
    , subsumerSubstitute
    , OpenSubsumerExpression
    , SealedSubsumerExpression(..)
    , subsumerExpression
    , subsumeExpression
    , subsumeExpressionTo
    ) where

import Data.Shim
import Language.Expression.Common.Sealed
import Language.Expression.Common.Simplifier
import Language.Expression.Common.SolverExpression
import Language.Expression.Common.TypeSystem
import Language.Expression.Common.Unifier
import Shapes

type OpenSubsumerExpression ts = TSOpenSolverExpression ts (Subsumer ts)

class ( UnifyTypeSystem ts
      , Applicative (Subsumer ts)
      , Show (SubsumerSubstitutions ts)
      , AllConstraint Show (TSPosWitness ts)
      , AllConstraint Show (TSNegWitness ts)
      ) => SubsumeTypeSystem ts where
    type Subsumer ts :: Type -> Type
    type SubsumerSubstitutions ts :: Type
    usubSubsumer :: forall a. UnifierSubstitutions ts -> Subsumer ts a -> TSOuter ts (OpenSubsumerExpression ts a)
    solveSubsumer :: forall a. Subsumer ts a -> TSOuter ts (TSOpenExpression ts a, SubsumerSubstitutions ts)
    showSubsumer :: forall a. Subsumer ts a -> String
    default showSubsumer :: AllConstraint Show (Subsumer ts) => forall a. Subsumer ts a -> String
    showSubsumer = allShow
    subsumerPosSubstitute :: SubsumerSubstitutions ts -> TSPosWitness ts t -> TSOuter ts (TSPosShimWit ts t)
    subsumerNegSubstitute :: SubsumerSubstitutions ts -> TSNegWitness ts t -> TSOuter ts (TSNegShimWit ts t)
    subsumePosWitnesses ::
           TSPosWitness ts inf -> TSPosWitness ts decl -> TSOuter ts (OpenSubsumerExpression ts (TSShim ts inf decl))

solveSubsumerExpression ::
       forall ts a. SubsumeTypeSystem ts
    => OpenSubsumerExpression ts a
    -> TSOuter ts (TSOpenExpression ts a, SubsumerSubstitutions ts)
solveSubsumerExpression (MkSolverExpression texpr vexpr) = do
    (expr, subs) <- solveSubsumer @ts texpr
    return (vexpr <*> expr, subs)

usubSubsumerExpression ::
       forall ts a. SubsumeTypeSystem ts
    => UnifierSubstitutions ts
    -> OpenSubsumerExpression ts a
    -> TSOuter ts (OpenSubsumerExpression ts a)
usubSubsumerExpression subs (MkSolverExpression texpr vexpr) = do
    sexpr <- usubSubsumer @ts subs texpr
    return $ solverExpressionLiftValue vexpr <*> sexpr

usubSolveSubsumer ::
       forall ts a. SubsumeTypeSystem ts
    => UnifierSubstitutions ts
    -> Subsumer ts a
    -> TSOuter ts (TSOpenExpression ts a, SubsumerSubstitutions ts)
usubSolveSubsumer subs subsumer = do
    sexp <- usubSubsumer @ts subs subsumer
    solveSubsumerExpression @ts sexp

subsumePosShimWit ::
       forall ts inf decl. SubsumeTypeSystem ts
    => TSPosShimWit ts inf
    -> TSPosWitness ts decl
    -> TSOuter ts (OpenSubsumerExpression ts (TSShim ts inf decl))
subsumePosShimWit winf tdecl =
    unPosShimWit winf $ \tinf convinf -> do
        subsumer <- subsumePosWitnesses @ts tinf tdecl
        return $ fmap (\td -> td . convinf) subsumer

solveSubsumeShimWit ::
       forall ts inf decl. SubsumeTypeSystem ts
    => TSPosShimWit ts inf
    -> TSPosWitness ts decl
    -> TSOuter ts (TSOpenExpression ts (TSShim ts inf decl))
solveSubsumeShimWit winf tdecl = do
    subsumer <- subsumePosShimWit @ts winf tdecl
    (ab, _) <- solveSubsumerExpression @ts subsumer
    return ab

subsumerSubstitute ::
       forall ts a. (SubsumeTypeSystem ts, TSMappable ts a)
    => SubsumerSubstitutions ts
    -> a
    -> TSOuter ts a
subsumerSubstitute subs =
    tsMapWitnessesM
        @ts
        (chainPolarShimWitM $ subsumerPosSubstitute @ts subs)
        (chainPolarShimWitM $ subsumerNegSubstitute @ts subs)

data SealedSubsumerExpression ts =
    forall tdecl. MkSealedSubsumerExpression (TSPosShimWit ts tdecl)
                                             (OpenSubsumerExpression ts tdecl)

instance SubsumeTypeSystem ts => Show (SealedSubsumerExpression ts) where
    show (MkSealedSubsumerExpression t (MkSolverExpression subs expr)) =
        showSubsumer @ts subs <> "/" <> allShow expr <> " => " <> show t

expressionSubsumption ::
       forall ts. SubsumeTypeSystem ts
    => TSSealedExpression ts
    -> SealedSubsumerExpression ts
expressionSubsumption (MkSealedExpression tw expr) = MkSealedSubsumerExpression tw $ solverExpressionLiftValue expr

subsumerExpressionTo ::
       forall ts t. (FunctionShim (TSShim ts), SubsumeTypeSystem ts, SimplifyTypeSystem ts)
    => TSPosWitness ts t
    -> TSSealedExpression ts
    -> TSOuter ts (OpenSubsumerExpression ts t)
subsumerExpressionTo tdecl (MkSealedExpression tinf expr) = do
    sexpr <- subsumePosShimWit @ts tinf tdecl
    return $ liftA2 shimToFunction sexpr $ solverExpressionLiftValue expr

-- Note the user's declared type will be simplified first, so they'll end up seeing a simplified version of the type they declared for their expression.
subsumerExpression ::
       forall ts. (FunctionShim (TSShim ts), SubsumeTypeSystem ts, SimplifyTypeSystem ts)
    => Maybe (Some (TSPosWitness ts))
    -> TSSealedExpression ts
    -> TSOuter ts (SealedSubsumerExpression ts)
subsumerExpression marawdecltype rawinfexpr = do
    expr <- simplify @ts rawinfexpr
    case marawdecltype of
        Nothing -> return $ expressionSubsumption expr
        Just (MkSome rawdecltype) -> do
            MkShimWit tdecl _ <- simplify @ts $ mkPolarShimWit @Type @(TSShim ts) @_ @'Positive rawdecltype
            sexpr <- subsumerExpressionTo @ts tdecl expr
            return $ MkSealedSubsumerExpression (mkPolarShimWit tdecl) sexpr

subsumeExpression ::
       forall ts. (FunctionShim (TSShim ts), SubsumeTypeSystem ts, SimplifyTypeSystem ts)
    => Some (TSPosWitness ts)
    -> TSSealedExpression ts
    -> TSOuter ts (TSSealedExpression ts)
subsumeExpression t expr = do
    MkSealedSubsumerExpression tp sexpr <- subsumerExpression @ts (Just t) expr
    (oexpr, ssubs) <- solveSubsumerExpression @ts sexpr
    oexpr' <- subsumerSubstitute @ts ssubs oexpr
    return $ MkSealedExpression tp oexpr'

subsumeExpressionTo ::
       forall ts t. (FunctionShim (TSShim ts), SubsumeTypeSystem ts, SimplifyTypeSystem ts)
    => TSPosWitness ts t
    -> TSSealedExpression ts
    -> TSOuter ts (TSOpenExpression ts t)
subsumeExpressionTo tdecl expr = do
    sexpr <- subsumerExpressionTo @ts tdecl expr
    (oexpr, ssubs) <- solveSubsumerExpression @ts sexpr
    subsumerSubstitute @ts ssubs oexpr
