module Language.Expression.Common.Subsumer
    ( SubsumeTypeSystem(..)
    , usubSubsumerExpression
    , usubSolveSubsumer
    , subsumePosShimWit
    , solveSubsumeShimWit
    , subsumerExpressionSubstitute
    , SubsumerExpression
    , Subsumption(..)
    , subsumerExpression
    , subsumeExpression
    , subsumeExpressionTo
    ) where

import Data.Shim
import Language.Expression.Common.Expression
import Language.Expression.Common.Named
import Language.Expression.Common.Sealed
import Language.Expression.Common.Simplifier
import Language.Expression.Common.SolverExpression
import Language.Expression.Common.TypeSystem
import Language.Expression.Common.Unifier
import Shapes

type SubsumerExpression ts = TSSolverExpression ts (Subsumer ts)

class ( UnifyTypeSystem ts
      , Applicative (Subsumer ts)
      , Show (SubsumerSubstitutions ts)
      , AllConstraint Show (TSPosWitness ts)
      , AllConstraint Show (TSNegWitness ts)
      ) => SubsumeTypeSystem ts where
    type Subsumer ts :: Type -> Type
    type SubsumerSubstitutions ts :: Type
    usubSubsumer :: forall a. UnifierSubstitutions ts -> Subsumer ts a -> TSOuter ts (SubsumerExpression ts a)
    solveSubsumer :: forall a. Subsumer ts a -> TSOuter ts (TSOpenExpression ts a, SubsumerSubstitutions ts)
    showSubsumer :: forall a. Subsumer ts a -> String
    default showSubsumer :: AllConstraint Show (Subsumer ts) => forall a. Subsumer ts a -> String
    showSubsumer = allShow
    -- This should generate substitutions only for the inferred type, not the declared type.
    subsumerNegSubstitute :: SubsumerSubstitutions ts -> TSNegWitness ts t -> TSOuter ts (TSNegShimWit ts t)
    subsumePosWitnesses ::
           TSPosWitness ts inf -> TSPosWitness ts decl -> TSOuter ts (SubsumerExpression ts (TSShim ts inf decl))

solveSubsumerExpression ::
       forall ts a. SubsumeTypeSystem ts
    => SubsumerExpression ts a
    -> TSOuter ts (TSOpenExpression ts a, SubsumerSubstitutions ts)
solveSubsumerExpression (MkSolverExpression texpr vexpr) = do
    (expr, subs) <- solveSubsumer @ts texpr
    return (vexpr <*> expr, subs)

usubSubsumerExpression ::
       forall ts a. SubsumeTypeSystem ts
    => UnifierSubstitutions ts
    -> SubsumerExpression ts a
    -> TSOuter ts (SubsumerExpression ts a)
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
    -> TSOuter ts (SubsumerExpression ts (TSShim ts inf decl))
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

subsumerExpressionSubstitute ::
       forall ts a. SubsumeTypeSystem ts
    => SubsumerSubstitutions ts
    -> TSOpenExpression ts a
    -> TSOuter ts (TSOpenExpression ts a)
subsumerExpressionSubstitute _ (ClosedExpression a) = return $ ClosedExpression a
subsumerExpressionSubstitute subs (OpenExpression (MkNameWitness name tw) expr) = do
    tw' <- chainPolarShimWitM (subsumerNegSubstitute @ts subs) tw
    expr' <- subsumerExpressionSubstitute @ts subs expr
    return $ OpenExpression (MkNameWitness name tw') expr'

data Subsumption ts =
    forall tdecl. MkSubsumption (TSPosShimWit ts tdecl)
                                (SubsumerExpression ts tdecl)

instance SubsumeTypeSystem ts => Show (Subsumption ts) where
    show (MkSubsumption t (MkSolverExpression subs expr)) =
        showSubsumer @ts subs <> "/" <> allShow expr <> " => " <> show t

expressionSubsumption ::
       forall ts. SubsumeTypeSystem ts
    => TSSealedExpression ts
    -> Subsumption ts
expressionSubsumption (MkSealedExpression tw expr) = MkSubsumption tw $ solverExpressionLiftValue expr

subsumerExpressionTo ::
       forall ts t. (FunctionShim (TSShim ts), SubsumeTypeSystem ts, SimplifyTypeSystem ts)
    => TSPosWitness ts t
    -> TSSealedExpression ts
    -> TSOuter ts (SubsumerExpression ts t)
subsumerExpressionTo tdecl (MkSealedExpression tinf expr) = do
    sexpr <- subsumePosShimWit @ts tinf tdecl
    return $ liftA2 shimToFunction sexpr $ solverExpressionLiftValue expr

-- Note the user's declared type will be simplified first, so they'll end up seeing a simplified version of the type they declared for their expression.
subsumerExpression ::
       forall ts. (FunctionShim (TSShim ts), SubsumeTypeSystem ts, SimplifyTypeSystem ts)
    => Maybe (Some (TSPosWitness ts))
    -> TSSealedExpression ts
    -> TSOuter ts (Subsumption ts)
subsumerExpression marawdecltype rawinfexpr = do
    expr <- simplify @ts rawinfexpr
    case marawdecltype of
        Nothing -> return $ expressionSubsumption expr
        Just (MkSome rawdecltype) -> do
            MkShimWit tdecl _ <- simplify @ts $ mkPolarShimWit @Type @(TSShim ts) @_ @'Positive rawdecltype
            sexpr <- subsumerExpressionTo @ts tdecl expr
            return $ MkSubsumption (mkPolarShimWit tdecl) sexpr

subsumeExpression ::
       forall ts. (FunctionShim (TSShim ts), SubsumeTypeSystem ts, SimplifyTypeSystem ts)
    => Some (TSPosWitness ts)
    -> TSSealedExpression ts
    -> TSOuter ts (TSSealedExpression ts)
subsumeExpression t expr = do
    MkSubsumption tp sexpr <- subsumerExpression @ts (Just t) expr
    (oexpr, ssubs) <- solveSubsumerExpression @ts sexpr
    oexpr' <- subsumerExpressionSubstitute @ts ssubs oexpr
    return $ MkSealedExpression tp oexpr'

subsumeExpressionTo ::
       forall ts t. (FunctionShim (TSShim ts), SubsumeTypeSystem ts, SimplifyTypeSystem ts)
    => TSPosWitness ts t
    -> TSSealedExpression ts
    -> TSOuter ts (TSOpenExpression ts t)
subsumeExpressionTo tdecl expr = do
    sexpr <- subsumerExpressionTo @ts tdecl expr
    (oexpr, ssubs) <- solveSubsumerExpression @ts sexpr
    subsumerExpressionSubstitute @ts ssubs oexpr
