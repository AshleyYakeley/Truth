module Language.Expression.Dolan.Solver.FlipType where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Occur
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Solver.UnifierM
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

type UnifierBisubstitution :: GroundTypeKind -> Type
type UnifierBisubstitution ground = Bisubstitution ground (DolanShim ground) (UnifierM ground)

type FlipType :: GroundTypeKind -> Polarity -> Type -> Type
data FlipType ground polarity t
    = NormalFlipType (DolanType ground polarity t)
    | InvertFlipType (DolanType ground (InvertPolarity polarity) t)

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             TestEquality (FlipType ground polarity) where
    testEquality (NormalFlipType t1) (NormalFlipType t2) = do
        Refl <- testEquality t1 t2
        return Refl
    testEquality (InvertFlipType t1) (InvertFlipType t2) =
        withInvertPolarity @polarity $ do
            Refl <- testEquality t1 t2
            return Refl
    testEquality _ _ = Nothing

instance forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity) =>
             Show (FlipType ground polarity t) where
    show (NormalFlipType t) = showDolanType t
    show (InvertFlipType t) = withInvertPolarity @polarity $ showDolanType t <> " [inv]"

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             AllConstraint Show (FlipType ground polarity) where
    allConstraint = Dict

occursInFlipType ::
       forall (ground :: GroundTypeKind) polarity tv a. IsDolanGroundType ground
    => TypeVarT tv
    -> FlipType ground polarity a
    -> Bool
occursInFlipType v (NormalFlipType t) = occursInType v t
occursInFlipType v (InvertFlipType t) = occursInType v t

bisubstituteFlipType ::
       forall (ground :: GroundTypeKind) polarity a. (IsDolanGroundType ground, Is PolarityType polarity)
    => UnifierBisubstitution ground
    -> FlipType ground polarity a
    -> UnifierM ground (PShimWit (DolanShim ground) (FlipType ground) polarity a)
bisubstituteFlipType bisub (NormalFlipType t) = do
    MkShimWit t' conv <- bisubstituteType bisub t
    return $ MkShimWit (NormalFlipType t') conv
bisubstituteFlipType _bisub (InvertFlipType t) = return $ mkShimWit $ InvertFlipType t
