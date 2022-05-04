module Language.Expression.Common.Subsumer
    ( SubsumeTypeSystem(..)
    , usubSolveSubsumer
    , subsumePosShimWit
    , solveSubsumeShimWit
    , subsumerExpressionSubstitute
    , SubsumerOpenExpression(..)
    , SubsumerExpression(..)
    , subsumerExpression
    , subsumeExpression
    ) where

import Data.Shim
import Language.Expression.Common.Expression
import Language.Expression.Common.Named
import Language.Expression.Common.Sealed
import Language.Expression.Common.Simplifier
import Language.Expression.Common.TypeSystem
import Language.Expression.Common.Unifier
import Shapes

class ( UnifyTypeSystem ts
      , Applicative (Subsumer ts)
      , Show (SubsumerSubstitutions ts)
      , AllWitnessConstraint Show (TSPosWitness ts)
      , AllWitnessConstraint Show (TSNegWitness ts)
      ) => SubsumeTypeSystem ts where
    type Subsumer ts :: Type -> Type
    type SubsumerSubstitutions ts :: Type
    usubSubsumer :: forall a. UnifierSubstitutions ts -> Subsumer ts a -> TSOuter ts (Subsumer ts a)
    solveSubsumer :: forall a. Subsumer ts a -> TSOuter ts (a, SubsumerSubstitutions ts)
    showSubsumer :: forall a. Subsumer ts a -> String
    default showSubsumer :: AllWitnessConstraint Show (Subsumer ts) => forall a. Subsumer ts a -> String
    showSubsumer = showAllWitness
    -- This should generate substitutions only for the inferred type, not the declared type.
    subsumerNegSubstitute :: SubsumerSubstitutions ts -> TSNegWitness ts t -> TSOuter ts (TSNegShimWit ts t)
    subsumePosWitnesses :: TSPosWitness ts inf -> TSPosWitness ts decl -> TSOuter ts (Subsumer ts (TSShim ts inf decl))

usubSolveSubsumer ::
       forall ts a. SubsumeTypeSystem ts
    => UnifierSubstitutions ts
    -> Subsumer ts a
    -> TSOuter ts (a, SubsumerSubstitutions ts)
usubSolveSubsumer subs subsumer = do
    subsumer' <- usubSubsumer @ts subs subsumer
    solveSubsumer @ts subsumer'

subsumePosShimWit ::
       forall ts inf decl. SubsumeTypeSystem ts
    => TSPosShimWit ts inf
    -> TSPosWitness ts decl
    -> TSOuter ts (Subsumer ts (TSShim ts inf decl))
subsumePosShimWit winf tdecl =
    unPosShimWit winf $ \tinf convinf -> do
        subsumer <- subsumePosWitnesses @ts tinf tdecl
        return $ fmap (\td -> td . convinf) subsumer

solveSubsumeShimWit ::
       forall ts inf decl. SubsumeTypeSystem ts
    => TSPosShimWit ts inf
    -> TSPosWitness ts decl
    -> TSOuter ts (TSShim ts inf decl)
solveSubsumeShimWit winf tdecl = do
    subsumer <- subsumePosShimWit @ts winf tdecl
    (ab, _) <- solveSubsumer @ts subsumer
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

data SubsumerOpenExpression ts tdecl =
    forall tinf. MkSubsumerOpenExpression (Subsumer ts (tinf -> tdecl))
                                          (TSOpenExpression ts tinf)

instance SubsumeTypeSystem ts => Show (SubsumerOpenExpression ts tdecl) where
    show (MkSubsumerOpenExpression subs expr) = showSubsumer @ts subs <> "/" <> showAllWitness expr

instance SubsumeTypeSystem ts => Functor (SubsumerOpenExpression ts) where
    fmap ab (MkSubsumerOpenExpression subsumer expr) = MkSubsumerOpenExpression (fmap (fmap ab) subsumer) expr

instance SubsumeTypeSystem ts => IsoVariant (SubsumerOpenExpression ts) where
    isoMap ab _ = fmap ab

instance SubsumeTypeSystem ts => Productish (SubsumerOpenExpression ts) where
    pUnit = MkSubsumerOpenExpression (pure $ \_ -> ()) pUnit
    MkSubsumerOpenExpression subsumerA exprA <***> MkSubsumerOpenExpression subsumerB exprB = let
        subsumerAB = liftA2 (\fa fb ~(a, b) -> (fa a, fb b)) subsumerA subsumerB
        exprAB = exprA <***> exprB
        in MkSubsumerOpenExpression subsumerAB exprAB

data SubsumerExpression ts =
    forall tdecl. MkSubsumerExpression (TSPosShimWit ts tdecl)
                                       (SubsumerOpenExpression ts tdecl)

instance SubsumeTypeSystem ts => Show (SubsumerExpression ts) where
    show (MkSubsumerExpression t expr) = show expr <> " => " <> show t

-- Note the user's declared type will be simplified first, so they'll end up seeing a simplified version of the type they declared for their expression.
subsumerExpression ::
       forall ts. (FunctionShim (TSShim ts), SubsumeTypeSystem ts, SimplifyTypeSystem ts)
    => Maybe (AnyW (TSPosWitness ts))
    -> TSSealedExpression ts
    -> TSOuter ts (SubsumerExpression ts)
subsumerExpression marawdecltype rawinfexpr = do
    MkSealedExpression infwit expr <- simplify @ts rawinfexpr
    case marawdecltype of
        Nothing -> return $ MkSubsumerExpression infwit $ MkSubsumerOpenExpression (pure id) expr
        Just (MkAnyW rawdecltype) -> do
            MkShimWit decltype _ <- simplify @ts $ mkPolarShimWit @Type @(TSShim ts) @_ @'Positive rawdecltype
            subsumer <- subsumePosShimWit @ts infwit decltype
            return $
                MkSubsumerExpression (mkPolarShimWit decltype) $
                MkSubsumerOpenExpression (fmap shimToFunction subsumer) expr

subsumeExpression ::
       forall ts. (FunctionShim (TSShim ts), SubsumeTypeSystem ts, SimplifyTypeSystem ts)
    => AnyW (TSPosWitness ts)
    -> TSSealedExpression ts
    -> TSOuter ts (TSSealedExpression ts)
subsumeExpression t expr = do
    MkSubsumerExpression tp (MkSubsumerOpenExpression subsumer oexpr) <- subsumerExpression @ts (Just t) expr
    (subconv, ssubs) <- solveSubsumer @ts subsumer
    oexpr' <- subsumerExpressionSubstitute @ts ssubs oexpr
    return $ MkSealedExpression tp $ fmap subconv oexpr'
