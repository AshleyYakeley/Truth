module Language.Expression.Common.Subsumer
    ( SubsumeTypeSystem(..)
    , subsumePosShimWit
    , solveSubsumeShimWit
    , subsumerExpressionSubstitute
    , SubsumerOpenExpression(..)
    , SubsumerExpression(..)
    , subsumerExpression
    ) where

import Data.Shim
import Language.Expression.Common.Expression
import Language.Expression.Common.Named
import Language.Expression.Common.Sealed
import Language.Expression.Common.Simplifier
import Language.Expression.Common.TypeSystem
import Shapes

class (TypeSystem ts, Applicative (Subsumer ts)) => SubsumeTypeSystem ts where
    type Subsumer ts :: Type -> Type
    type SubsumerSubstitutions ts :: Type
    solveSubsumer :: Subsumer ts a -> TSOuter ts (a, SubsumerSubstitutions ts)
    -- This should generate substitutions only for the inferred type, not the declared type.
    subsumerNegSubstitute :: SubsumerSubstitutions ts -> TSNegWitness ts t -> TSOuter ts (TSNegShimWit ts t)
    subsumePosWitnesses :: TSPosWitness ts inf -> TSPosWitness ts decl -> TSOuter ts (Subsumer ts (TSShim ts inf decl))

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
    tw' <- chainShimWitM (subsumerNegSubstitute @ts subs) tw
    expr' <- subsumerExpressionSubstitute @ts subs expr
    return $ OpenExpression (MkNameWitness name tw') expr'

data SubsumerOpenExpression ts tdecl =
    forall tinf. MkSubsumerOpenExpression (Subsumer ts (tinf -> tdecl))
                                          (TSOpenExpression ts tinf)

instance SubsumeTypeSystem ts => Functor (SubsumerOpenExpression ts) where
    fmap ab (MkSubsumerOpenExpression subsumer expr) = MkSubsumerOpenExpression (fmap (fmap ab) subsumer) expr

instance SubsumeTypeSystem ts => IsoVariant (SubsumerOpenExpression ts)

instance SubsumeTypeSystem ts => Productish (SubsumerOpenExpression ts) where
    pUnit = MkSubsumerOpenExpression (pure $ \_ -> ()) pUnit
    MkSubsumerOpenExpression subsumerA exprA <***> MkSubsumerOpenExpression subsumerB exprB = let
        subsumerAB = liftA2 (\fa fb ~(a, b) -> (fa a, fb b)) subsumerA subsumerB
        exprAB = exprA <***> exprB
        in MkSubsumerOpenExpression subsumerAB exprAB

data SubsumerExpression ts =
    forall tdecl. MkSubsumerExpression (TSPosShimWit ts tdecl)
                                       (SubsumerOpenExpression ts tdecl)

-- Note the user's declared type will be simplified first, so they'll end up seeing a simplified version of the type they declared for their expression.
subsumerExpression ::
       forall ts. (FunctionShim (TSShim ts), SubsumeTypeSystem ts, SimplifyTypeSystem ts)
    => Maybe (AnyW (TSPosWitness ts))
    -> TSSealedExpression ts
    -> TSOuter ts (SubsumerExpression ts)
subsumerExpression marawdecltype rawinfexpr = do
    MkSealedExpression wtinf expr <- simplify @ts rawinfexpr
    case marawdecltype of
        Nothing -> return $ MkSubsumerExpression wtinf $ MkSubsumerOpenExpression (pure id) expr
        Just (MkAnyW rawdecltype) -> do
            MkShimWit decltype _ <- simplify @ts $ mkShimWit @Type @(TSShim ts) @_ @'Positive rawdecltype
            subsumer <- subsumePosShimWit @ts wtinf decltype
            return $
                MkSubsumerExpression (mkShimWit decltype) $ MkSubsumerOpenExpression (fmap shimToFunction subsumer) expr
