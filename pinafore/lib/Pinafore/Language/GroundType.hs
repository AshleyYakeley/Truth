module Pinafore.Language.GroundType where

import Language.Expression.Dolan
import Language.Expression.Polarity
import Pinafore.Base
import Pinafore.Language.Morphism
import Pinafore.Language.Order
import Pinafore.Language.Reference
import Pinafore.Language.Set
import Pinafore.Language.SimpleEntityType
import Pinafore.Language.UI
import Shapes

{-
-- could really use https://github.com/ghc-proposals/ghc-proposals/pull/81

data DolanType (gt :: Polarity -> forall (dk :: DolanVariance) -> DolanVarianceKind dk -> Type) (polarity :: Polarity) (t :: Type) where
    NilDolanType :: DolanType gt polarity (LimitType polarity)
    ConsDolanType
        :: DolanSingularType gt polarity t
        -> DolanType gt polarity tr
        -> DolanType gt polarity (JoinMeetType polarity t tr)

data DolanSingularType (gt :: Polarity -> forall (dk :: DolanVariance) -> DolanVarianceKind dk -> Type) (polarity :: Polarity) (t :: Type) where
    GroundDolanSingularType
        :: gt polarity dv t
        -> DolanArguments dv (DolanType gt) t polarity ta
        -> DolanSingularType gt polarity ta
    VarDolanSingularType :: SymbolType name -> DolanSingularType gt polarity (UVar name)

-- PinaforeGroundType baseedit :: Polarity -> forall (dk :: DolanVariance) -> DolanVarianceKind dk -> Type
-}
data PinaforeGroundType baseedit (polarity :: Polarity) (dk :: DolanVariance) (t :: DolanVarianceKind dk) where
    ActionPinaforeGroundType :: PinaforeGroundType baseedit polarity '[] (PinaforeAction baseedit ())
    OrderPinaforeGroundType :: PinaforeGroundType baseedit polarity '[ 'Contravariance] (PinaforeOrder baseedit)
    UserInterfacePinaforeGroundType :: PinaforeGroundType baseedit polarity '[ 'Covariance] (PinaforeUI baseedit)
    SimpleEntityPinaforeGroundType :: SimpleEntityType t -> PinaforeGroundType baseedit polarity '[] t
    FuncPinaforeGroundType :: PinaforeGroundType baseedit polarity '[ 'Contravariance, 'Covariance] (->)
    MaybePinaforeGroundType :: PinaforeGroundType baseedit polarity '[ 'Covariance] Maybe
    ListPinaforeGroundType :: PinaforeGroundType baseedit polarity '[ 'Covariance] []
    PairPinaforeGroundType :: PinaforeGroundType baseedit polarity '[ 'Covariance, 'Covariance] (,)
    EitherPinaforeGroundType :: PinaforeGroundType baseedit polarity '[ 'Covariance, 'Covariance] Either
    ReferencePinaforeGroundType :: PinaforeGroundType baseedit polarity '[ 'Rangevariance] (PinaforeReference baseedit)
    SetPinaforeGroundType :: PinaforeGroundType baseedit polarity '[ 'Rangevariance] (PinaforeSet baseedit)
    MorphismPinaforeGroundType
        :: PinaforeGroundType baseedit polarity '[ 'Rangevariance, 'Rangevariance] (PinaforeMorphism baseedit)

testPinaforeGroundTypeEquality ::
       PinaforeGroundType baseedit polarity dka ta
    -> PinaforeGroundType baseedit polarity dkb tb
    -> Maybe (dka :~: dkb, ta :~~: tb)
testPinaforeGroundTypeEquality ActionPinaforeGroundType ActionPinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality OrderPinaforeGroundType OrderPinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality UserInterfacePinaforeGroundType UserInterfacePinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality (SimpleEntityPinaforeGroundType t1) (SimpleEntityPinaforeGroundType t2) = do
    Refl <- testEquality t1 t2
    Just (Refl, HRefl)
testPinaforeGroundTypeEquality FuncPinaforeGroundType FuncPinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality MaybePinaforeGroundType MaybePinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality ListPinaforeGroundType ListPinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality PairPinaforeGroundType PairPinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality EitherPinaforeGroundType EitherPinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality ReferencePinaforeGroundType ReferencePinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality SetPinaforeGroundType SetPinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality MorphismPinaforeGroundType MorphismPinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality _ _ = Nothing

pinaforeGroundTypeVary ::
       forall baseedit polarity (dk :: DolanVariance) (f :: DolanVarianceKind dk).
       PinaforeGroundType baseedit polarity dk f
    -> DolanKindVary dk f
pinaforeGroundTypeVary ActionPinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary OrderPinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary UserInterfacePinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary (SimpleEntityPinaforeGroundType _) = dolanVary @dk
pinaforeGroundTypeVary FuncPinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary MaybePinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary ListPinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary PairPinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary EitherPinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary ReferencePinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary SetPinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary MorphismPinaforeGroundType = dolanVary @dk

pinaforeGroundTypeKind :: PinaforeGroundType baseedit polarity dk t -> DolanVarianceType dk
pinaforeGroundTypeKind ActionPinaforeGroundType = representative
pinaforeGroundTypeKind OrderPinaforeGroundType = representative
pinaforeGroundTypeKind UserInterfacePinaforeGroundType = representative
pinaforeGroundTypeKind (SimpleEntityPinaforeGroundType _) = representative
pinaforeGroundTypeKind FuncPinaforeGroundType = representative
pinaforeGroundTypeKind MaybePinaforeGroundType = representative
pinaforeGroundTypeKind ListPinaforeGroundType = representative
pinaforeGroundTypeKind PairPinaforeGroundType = representative
pinaforeGroundTypeKind EitherPinaforeGroundType = representative
pinaforeGroundTypeKind ReferencePinaforeGroundType = representative
pinaforeGroundTypeKind SetPinaforeGroundType = representative
pinaforeGroundTypeKind MorphismPinaforeGroundType = representative

invertGroundTypePolarity ::
       PinaforeGroundType baseedit polarity dk t -> Maybe (PinaforeGroundType baseedit (InvertPolarity polarity) dk t)
invertGroundTypePolarity ActionPinaforeGroundType = Just ActionPinaforeGroundType
invertGroundTypePolarity OrderPinaforeGroundType = Just OrderPinaforeGroundType
invertGroundTypePolarity UserInterfacePinaforeGroundType = Just UserInterfacePinaforeGroundType
invertGroundTypePolarity (SimpleEntityPinaforeGroundType t) = Just $ SimpleEntityPinaforeGroundType t
invertGroundTypePolarity FuncPinaforeGroundType = Just FuncPinaforeGroundType
invertGroundTypePolarity MaybePinaforeGroundType = Just MaybePinaforeGroundType
invertGroundTypePolarity ListPinaforeGroundType = Just ListPinaforeGroundType
invertGroundTypePolarity PairPinaforeGroundType = Just PairPinaforeGroundType
invertGroundTypePolarity EitherPinaforeGroundType = Just EitherPinaforeGroundType
invertGroundTypePolarity ReferencePinaforeGroundType = Just ReferencePinaforeGroundType
invertGroundTypePolarity SetPinaforeGroundType = Just SetPinaforeGroundType
invertGroundTypePolarity MorphismPinaforeGroundType = Just MorphismPinaforeGroundType
