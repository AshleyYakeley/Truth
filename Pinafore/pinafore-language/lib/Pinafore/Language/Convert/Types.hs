{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Convert.Types
    (
    ) where

import Data.Time
import Pinafore.Base
import Pinafore.Language.Convert.HasType
import Pinafore.Language.Type
import Pinafore.Language.Value
import Pinafore.Language.Var
import Shapes
import Shapes.Numeric

-- top, bottom, join, meet
instance HasPinaforeType 'Positive BottomType where
    pinaforeType = nilDolanShimWit

instance HasPinaforeType 'Negative TopType where
    pinaforeType = nilDolanShimWit

instance (HasPinaforeType 'Positive a, HasPinaforeType 'Positive b) => HasPinaforeType 'Positive (JoinType a b) where
    pinaforeType = joinMeetShimWit pinaforeType pinaforeType

instance (HasPinaforeType 'Negative a, HasPinaforeType 'Negative b) => HasPinaforeType 'Negative (MeetType a b) where
    pinaforeType = joinMeetShimWit pinaforeType pinaforeType

-- Var Type
instance (Is PolarityType polarity, KnownSymbol name) => HasPinaforeType polarity (Var name) where
    pinaforeType =
        singleDolanShimWit $
        MkShimWit (VarDolanSingularType $ MkSymbolType @name) $
        case polarityType @polarity of
            PositiveType -> MkPolarMap $ coerceEnhanced "var"
            NegativeType -> MkPolarMap $ coerceEnhanced "var"

-- (,)
instance HasPinaforeGroundType '[ CoCCRVariance, CoCCRVariance] (,) where
    pinaforeGroundType =
        EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) PairEntityGroundType

-- Either
instance HasPinaforeGroundType '[ CoCCRVariance, CoCCRVariance] Either where
    pinaforeGroundType =
        EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) EitherEntityGroundType

-- (->)
instance HasPinaforeGroundType '[ ContraCCRVariance, CoCCRVariance] (->) where
    pinaforeGroundType = funcGroundType

-- Maybe
instance HasPinaforeGroundType '[ CoCCRVariance] Maybe where
    pinaforeGroundType = EntityPinaforeGroundType (ConsListType Refl NilListType) MaybeEntityGroundType

-- []
instance HasPinaforeGroundType '[ CoCCRVariance] [] where
    pinaforeGroundType = EntityPinaforeGroundType (ConsListType Refl NilListType) ListEntityGroundType

-- PinaforeAction
instance HasPinaforeGroundType '[ CoCCRVariance] PinaforeAction where
    pinaforeGroundType = actionGroundType

-- LangWholeRef
instance HasPinaforeGroundType '[ 'RangeCCRVariance] LangWholeRef where
    pinaforeGroundType = wholeRefGroundType

-- PinaforeImmutableWholeRef
instance (HasPinaforeType 'Negative a) => HasPinaforeType 'Negative (PinaforeImmutableWholeRef a) where
    pinaforeType = mapNegShimWit (functionToShim "subtype" $ langWholeRefToImmutable @BottomType) pinaforeType

instance (HasPinaforeType 'Positive a) => HasPinaforeType 'Positive (PinaforeImmutableWholeRef a) where
    pinaforeType = mapPosShimWit (functionToShim "subtype" pinaforeImmutableToWholeRef) pinaforeType

-- Literal types
literalType :: Is LiteralType t => PinaforeGroundType '[] t
literalType = EntityPinaforeGroundType NilListType $ LiteralEntityGroundType representative

instance HasPinaforeGroundType '[] Literal where
    pinaforeGroundType = literalType

instance HasPinaforeGroundType '[] Text where
    pinaforeGroundType = literalType

instance HasPinaforeGroundType '[] Number where
    pinaforeGroundType = literalType

instance HasPinaforeGroundType '[] SafeRational where
    pinaforeGroundType = literalType

instance HasPinaforeGroundType '[] Integer where
    pinaforeGroundType = literalType

instance HasPinaforeGroundType '[] Bool where
    pinaforeGroundType = literalType

instance HasPinaforeGroundType '[] Ordering where
    pinaforeGroundType = literalType

instance HasPinaforeGroundType '[] UTCTime where
    pinaforeGroundType = literalType

instance HasPinaforeGroundType '[] NominalDiffTime where
    pinaforeGroundType = literalType

instance HasPinaforeGroundType '[] Day where
    pinaforeGroundType = literalType

instance HasPinaforeGroundType '[] TimeOfDay where
    pinaforeGroundType = literalType

instance HasPinaforeGroundType '[] LocalTime where
    pinaforeGroundType = literalType

instance HasPinaforeGroundType '[] () where
    pinaforeGroundType = literalType

-- Double
instance HasPinaforeType 'Positive Double where
    pinaforeType = mapPosShimWit (functionToShim "InexactNumber" InexactNumber) pinaforeType

instance HasPinaforeType 'Negative Double where
    pinaforeType = mapNegShimWit (functionToShim "numberToDouble" numberToDouble) pinaforeType

-- Int
instance HasPinaforeType 'Positive Int where
    pinaforeType = mapPosShimWit (functionToShim "toInteger" toInteger) pinaforeType

instance HasPinaforeType 'Negative Int where
    pinaforeType = mapNegShimWit (functionToShim "fromInteger" fromInteger) pinaforeType

-- Int32
instance HasPinaforeType 'Positive Int32 where
    pinaforeType = mapPosShimWit (functionToShim "toInteger" toInteger) pinaforeType

instance HasPinaforeType 'Negative Int32 where
    pinaforeType = mapNegShimWit (functionToShim "fromInteger" fromInteger) pinaforeType

-- Int64
instance HasPinaforeType 'Positive Int64 where
    pinaforeType = mapPosShimWit (functionToShim "toInteger" toInteger) pinaforeType

instance HasPinaforeType 'Negative Int64 where
    pinaforeType = mapNegShimWit (functionToShim "fromInteger" fromInteger) pinaforeType

-- Rational
instance HasPinaforeType 'Positive Rational where
    pinaforeType = mapPosShimWit (functionToShim "fromRational" $ fromRational @SafeRational) pinaforeType

instance HasPinaforeType 'Negative Rational where
    pinaforeType = mapNegShimWit (functionToShim "toRational" $ toRational @SafeRational) pinaforeType

-- Fixed
instance HasResolution r => HasPinaforeType 'Positive (Fixed r) where
    pinaforeType = mapPosShimWit (functionToShim "toRational" toRational) pinaforeType

instance HasResolution r => HasPinaforeType 'Negative (Fixed r) where
    pinaforeType = mapNegShimWit (functionToShim "fromRational" fromRational) pinaforeType

-- Vector
instance HasPinaforeType 'Positive a => HasPinaforeType 'Positive (Vector a) where
    pinaforeType = mapPosShimWit (functionToShim "toList" toList) pinaforeType

instance HasPinaforeType 'Negative a => HasPinaforeType 'Negative (Vector a) where
    pinaforeType = mapNegShimWit (functionToShim "fromList" fromList) pinaforeType
