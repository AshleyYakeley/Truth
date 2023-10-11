module Language.Expression.Dolan.Solver.WholeConstraint where

import Data.Shim
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeResult
import Language.Expression.Dolan.TypeSystem
import Shapes

type SolverBisubstitution :: GroundTypeKind -> Type
type SolverBisubstitution ground = Bisubstitution ground (DolanPolyIsoShim ground Type) (TypeResult ground)

type WholeConstraint :: GroundTypeKind -> Type -> Type
data WholeConstraint ground t where
    MkWholeConstraint
        :: forall (ground :: GroundTypeKind) a b.
           FlipType ground 'Positive a
        -> FlipType ground 'Negative b
        -> WholeConstraint ground (DolanShim ground a b)

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => TestEquality (WholeConstraint ground) where
    testEquality (MkWholeConstraint ta1 tb1) (MkWholeConstraint ta2 tb2) = do
        Refl <- testEquality ta1 ta2
        Refl <- testEquality tb1 tb2
        return Refl

instance forall (ground :: GroundTypeKind) t. IsDolanGroundType ground => Show (WholeConstraint ground t) where
    show (MkWholeConstraint ta tb) = allShow ta <> " <: " <> allShow tb

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => AllConstraint Show (WholeConstraint ground) where
    allConstraint = Dict

type WholeConstraintShim :: GroundTypeKind -> Type -> Type
type WholeConstraintShim ground = ShimWit (CatDual (->)) (WholeConstraint ground)

bisubstituteWholeConstraint ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => SolverBisubstitution ground
    -> WholeConstraint ground a
    -> TypeResult ground (WholeConstraintShim ground a)
bisubstituteWholeConstraint bisub (MkWholeConstraint fta ftb) = do
    MkShimWit fta' (MkPolarShim (MkPolyMapT conva)) <- bisubstituteFlipType bisub fta
    MkShimWit ftb' (MkPolarShim (MkPolyMapT convb)) <- bisubstituteFlipType bisub ftb
    return $ MkShimWit (MkWholeConstraint fta' ftb') $ MkCatDual $ \conv -> isoForwards convb . conv . isoForwards conva

bisubstituteWholeConstraintShim ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => SolverBisubstitution ground
    -> WholeConstraintShim ground a
    -> TypeResult ground (WholeConstraintShim ground a)
bisubstituteWholeConstraintShim bisub = chainShimWitM $ bisubstituteWholeConstraint bisub

bisubstitutesWholeConstraintShim ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => [SolverBisubstitution ground]
    -> WholeConstraintShim ground a
    -> TypeResult ground (WholeConstraintShim ground a)
bisubstitutesWholeConstraintShim bisubs = unEndoM $ mconcat $ fmap (MkEndoM . bisubstituteWholeConstraintShim) bisubs
