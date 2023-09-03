module Language.Expression.Dolan.Unifier.Piece where

import Data.Shim
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

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

type PuzzlePiece :: GroundTypeKind -> Type -> Type
data PuzzlePiece ground t where
    MkPuzzlePiece
        :: forall (ground :: GroundTypeKind) a b.
           FlipType ground 'Positive a
        -> FlipType ground 'Negative b
        -> PuzzlePiece ground (DolanShim ground a b)

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => TestEquality (PuzzlePiece ground) where
    testEquality (MkPuzzlePiece ta1 tb1) (MkPuzzlePiece ta2 tb2) = do
        Refl <- testEquality ta1 ta2
        Refl <- testEquality tb1 tb2
        return Refl

instance forall (ground :: GroundTypeKind) t. IsDolanGroundType ground => Show (PuzzlePiece ground t) where
    show (MkPuzzlePiece ta tb) = allShow ta <> " <: " <> allShow tb

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => AllConstraint Show (PuzzlePiece ground) where
    allConstraint = Dict
