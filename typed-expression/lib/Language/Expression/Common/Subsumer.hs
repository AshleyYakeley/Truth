module Language.Expression.Common.Subsumer
    ( SubsumeTypeSystem(..)
    , subsumeExpression
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

-- Note the user's declared type will be simplified first, so they'll end up seeing a simplified version of the type they declared for their expression.
subsumeExpression ::
       forall ts. (SubsumeTypeSystem ts, SimplifyTypeSystem ts)
    => AnyW (TSPosWitness ts)
    -> TSSealedExpression ts
    -> TSOuter ts (TSSealedExpression ts)
subsumeExpression (MkAnyW rawdecltype) (MkSealedExpression rawinfwit expr) = do
    MkShimWit decltype _ <- simplifyPosType @ts TPWhole rawdecltype
    MkShimWit inftype infconv <- chainShimWitM (simplifyPosType @ts TPPartial) rawinfwit
    uab <- subsumePosWitnesses @ts inftype decltype
    (conv, subs) <- solveSubsumer @ts uab
    expr' <- subsumerExpressionSubstitute @ts subs expr
    return $ MkSealedExpression (MkShimWit decltype $ MkPolarMap conv . infconv) expr'
