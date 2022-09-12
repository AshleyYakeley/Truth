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

{-
data SubsumerOpenExpression ts tdecl =
    forall tinf. MkSubsumerOpenExpression (Subsumer ts (tinf -> tdecl))
                                          (TSOpenExpression ts tinf)
instance SubsumeTypeSystem ts => Show (SubsumerOpenExpression ts tdecl) where
    show (MkSubsumerOpenExpression subs expr) = showSubsumer @ts subs <> "/" <> allShow expr

instance SubsumeTypeSystem ts => Functor (SubsumerOpenExpression ts) where
    fmap ab (MkSubsumerOpenExpression subsumer expr) = MkSubsumerOpenExpression (fmap (fmap ab) subsumer) expr

instance SubsumeTypeSystem ts => Invariant (SubsumerOpenExpression ts) where
    invmap ab _ = fmap ab

instance SubsumeTypeSystem ts => Productable (SubsumerOpenExpression ts) where
    rUnit = MkSubsumerOpenExpression (pure $ \_ -> ()) rUnit
    MkSubsumerOpenExpression subsumerA exprA <***> MkSubsumerOpenExpression subsumerB exprB = let
        subsumerAB = liftA2 (\fa fb ~(a, b) -> (fa a, fb b)) subsumerA subsumerB
        exprAB = exprA <***> exprB
        in MkSubsumerOpenExpression subsumerAB exprAB
-}
{-
instance SubsumeTypeSystem ts => Show (SolverExpression poswit negwit (Subsumer ts) valexpr tdecl) where
    show (MkSolverExpression subs expr) = showSubsumer @ts subs <> "/" <> allShow expr
-}
data Subsumption ts =
    forall tdecl. MkSubsumption (TSPosShimWit ts tdecl)
                                (SubsumerExpression ts tdecl)

instance SubsumeTypeSystem ts => Show (Subsumption ts) where
    show (MkSubsumption t (MkSolverExpression subs expr)) =
        showSubsumer @ts subs <> "/" <> allShow expr <> " => " <> show t

-- Note the user's declared type will be simplified first, so they'll end up seeing a simplified version of the type they declared for their expression.
subsumerExpression ::
       forall ts. (FunctionShim (TSShim ts), SubsumeTypeSystem ts, SimplifyTypeSystem ts)
    => Maybe (Some (TSPosWitness ts))
    -> TSSealedExpression ts
    -> TSOuter ts (Subsumption ts)
subsumerExpression marawdecltype rawinfexpr = do
    MkSealedExpression infwit expr <- simplify @ts rawinfexpr
    case marawdecltype of
        Nothing -> return $ MkSubsumption infwit $ solverExpressionLiftValue expr
        Just (MkSome rawdecltype) -> do
            MkShimWit decltype _ <- simplify @ts $ mkPolarShimWit @Type @(TSShim ts) @_ @'Positive rawdecltype
            sexpr <- subsumePosShimWit @ts infwit decltype
            return $
                MkSubsumption (mkPolarShimWit decltype) $ liftA2 shimToFunction sexpr $ solverExpressionLiftValue expr

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
