module Pinafore.Language.GroundType where

import Language.Expression.Dolan
import Pinafore.Language.Entity

import Pinafore.Action
import Pinafore.Language.Literal
import Pinafore.Language.Morphism
import Pinafore.Language.Order
import Pinafore.Language.Reference
import Pinafore.Language.Set
import Pinafore.Table (Point)
import Shapes
import Truth.Core

-- could really use https://github.com/ghc-proposals/ghc-proposals/pull/81
data PinaforeGroundType baseedit (polarity :: TypePolarity) (dk :: DolanVariance) (t :: DolanVarianceKind dk) where
    InvertLimitPinaforeGroundType :: PinaforeGroundType baseedit polarity '[] (LimitType (InvertPolarity polarity))
    ActionPinaforeGroundType :: PinaforeGroundType baseedit polarity '[] (PinaforeAction baseedit)
    OrderPinaforeGroundType :: PinaforeGroundType baseedit polarity '[ 'Contravariance] (PinaforeOrder baseedit)
    UserInterfacePinaforeGroundType :: PinaforeGroundType baseedit polarity '[] (UISpec (ConstEdit Entity) baseedit)
    LiteralPinaforeGroundType :: LiteralType t -> PinaforeGroundType baseedit polarity '[] t
    PointPinaforeGroundType :: PinaforeGroundType baseedit polarity '[] Point
    EntityPinaforeGroundType :: PinaforeGroundType baseedit polarity '[] Entity
    NamedEntityPinaforeGroundType :: SymbolWitness name -> PinaforeGroundType baseedit polarity '[] (NamedEntity name)
    FuncPinaforeGroundType :: PinaforeGroundType baseedit polarity '[ 'Contravariance, 'Covariance] (->)
    ListPinaforeGroundType :: PinaforeGroundType baseedit polarity '[ 'Covariance] []
    PairPinaforeGroundType :: PinaforeGroundType baseedit polarity '[ 'Covariance, 'Covariance] (,)
    ReferencePinaforeGroundType :: PinaforeGroundType baseedit polarity '[ 'Rangevariance] (PinaforeReference baseedit)
    SetPinaforeGroundType :: PinaforeGroundType baseedit polarity '[ 'Rangevariance] (PinaforeSet baseedit)
    MorphismPinaforeGroundType
        :: PinaforeGroundType baseedit polarity '[ 'Rangevariance, 'Rangevariance] (PinaforeMorphism baseedit)

testPinaforeGroundTypeEquality ::
       PinaforeGroundType baseedit polarity dka ta
    -> PinaforeGroundType baseedit polarity dkb tb
    -> Maybe (dka :~: dkb, ta :~~: tb)
testPinaforeGroundTypeEquality InvertLimitPinaforeGroundType InvertLimitPinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality ActionPinaforeGroundType ActionPinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality OrderPinaforeGroundType OrderPinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality UserInterfacePinaforeGroundType UserInterfacePinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality (LiteralPinaforeGroundType t1) (LiteralPinaforeGroundType t2) = do
    Refl <- testEquality t1 t2
    Just (Refl, HRefl)
testPinaforeGroundTypeEquality PointPinaforeGroundType PointPinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality EntityPinaforeGroundType EntityPinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality (NamedEntityPinaforeGroundType t1) (NamedEntityPinaforeGroundType t2) = do
    Refl <- testEquality t1 t2
    Just (Refl, HRefl)
testPinaforeGroundTypeEquality FuncPinaforeGroundType FuncPinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality ListPinaforeGroundType ListPinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality PairPinaforeGroundType PairPinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality ReferencePinaforeGroundType ReferencePinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality SetPinaforeGroundType SetPinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality MorphismPinaforeGroundType MorphismPinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality _ _ = Nothing

pinaforeGroundTypeVary ::
       forall baseedit polarity (dk :: DolanVariance) (f :: DolanVarianceKind dk).
       PinaforeGroundType baseedit polarity dk f
    -> DolanKindVary dk f
pinaforeGroundTypeVary InvertLimitPinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary ActionPinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary OrderPinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary UserInterfacePinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary (LiteralPinaforeGroundType _) = dolanVary @dk
pinaforeGroundTypeVary PointPinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary EntityPinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary (NamedEntityPinaforeGroundType _) = dolanVary @dk
pinaforeGroundTypeVary FuncPinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary ListPinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary PairPinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary ReferencePinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary SetPinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary MorphismPinaforeGroundType = dolanVary @dk

pinaforeGroundTypeKind :: PinaforeGroundType baseedit polarity dk t -> DolanVarianceType dk
pinaforeGroundTypeKind InvertLimitPinaforeGroundType = representative
pinaforeGroundTypeKind ActionPinaforeGroundType = representative
pinaforeGroundTypeKind OrderPinaforeGroundType = representative
pinaforeGroundTypeKind UserInterfacePinaforeGroundType = representative
pinaforeGroundTypeKind (LiteralPinaforeGroundType _) = representative
pinaforeGroundTypeKind PointPinaforeGroundType = representative
pinaforeGroundTypeKind EntityPinaforeGroundType = representative
pinaforeGroundTypeKind (NamedEntityPinaforeGroundType _) = representative
pinaforeGroundTypeKind FuncPinaforeGroundType = representative
pinaforeGroundTypeKind ListPinaforeGroundType = representative
pinaforeGroundTypeKind PairPinaforeGroundType = representative
pinaforeGroundTypeKind ReferencePinaforeGroundType = representative
pinaforeGroundTypeKind SetPinaforeGroundType = representative
pinaforeGroundTypeKind MorphismPinaforeGroundType = representative
