module Language.Expression.Dolan.Unifier.WholeConstraint where

import Data.Shim
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unifier.FlipType
import Language.Expression.Dolan.Unifier.UnifierM
import Shapes

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
    => UnifierBisubstitution ground
    -> WholeConstraint ground a
    -> UnifierM ground (WholeConstraintShim ground a)
bisubstituteWholeConstraint bisub (MkWholeConstraint fta ftb) = do
    MkShimWit fta' (MkPolarMap conva) <- bisubstituteFlipType bisub fta
    MkShimWit ftb' (MkPolarMap convb) <- bisubstituteFlipType bisub ftb
    return $ MkShimWit (MkWholeConstraint fta' ftb') $ MkCatDual $ \conv -> convb . conv . conva

bisubstituteWholeConstraintShim ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => UnifierBisubstitution ground
    -> WholeConstraintShim ground a
    -> UnifierM ground (WholeConstraintShim ground a)
bisubstituteWholeConstraintShim bisub = chainShimWitM $ bisubstituteWholeConstraint bisub

bisubstitutesWholeConstraintShim ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => [UnifierBisubstitution ground]
    -> WholeConstraintShim ground a
    -> UnifierM ground (WholeConstraintShim ground a)
bisubstitutesWholeConstraintShim bisubs = unEndoM $ mconcat $ fmap (MkEndoM . bisubstituteWholeConstraintShim) bisubs
