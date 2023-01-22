{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Convert.Types where

import Changes.Core
import Data.Time
import Pinafore.Base
import Pinafore.Language.Convert.HasType
import Pinafore.Language.Type
import Pinafore.Language.Value
import Pinafore.Language.Var
import Shapes
import Shapes.Numeric

-- top, bottom, join, meet
instance HasQType 'Positive BottomType where
    qType = nilDolanShimWit

instance HasQType 'Negative TopType where
    qType = nilDolanShimWit

instance (HasQType 'Positive a, HasQType 'Positive b) => HasQType 'Positive (JoinType a b) where
    qType = joinMeetShimWit qType qType

instance (HasQType 'Negative a, HasQType 'Negative b) => HasQType 'Negative (MeetType a b) where
    qType = joinMeetShimWit qType qType

-- Var Type
instance (Is PolarityType polarity, KnownSymbol name) => HasQType polarity (Var name) where
    qType =
        shimWitToDolan $
        MkShimWit (VarDolanSingularType $ MkSymbolType @name) $
        case polarityType @polarity of
            PositiveType -> MkPolarMap $ coerceShim "var"
            NegativeType -> MkPolarMap $ coerceShim "var"

-- (,)
instance HasQGroundType '[ CoCCRVariance, CoCCRVariance] (,) where
    qGroundType = pairGroundType

-- Either
instance HasQGroundType '[ CoCCRVariance, CoCCRVariance] Either where
    qGroundType = eitherGroundType

-- (->)
instance HasQGroundType '[ ContraCCRVariance, CoCCRVariance] (->) where
    qGroundType = funcGroundType

-- Maybe
instance HasQGroundType '[ CoCCRVariance] Maybe where
    qGroundType = maybeGroundType

-- []
instance HasQGroundType '[ CoCCRVariance] [] where
    qGroundType = listGroundType

-- NonEmpty
instance HasQGroundType '[ CoCCRVariance] NonEmpty where
    qGroundType = list1GroundType

-- Showable
instance HasQGroundType '[] Showable where
    qGroundType = showableGroundType

-- Action
instance HasQGroundType '[ CoCCRVariance] Action where
    qGroundType = actionGroundType

-- LangWholeModel
wholeModelGroundType :: QGroundType '[ 'RangeCCRVariance] LangWholeModel
wholeModelGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangWholeModel)|]) "WholeModel"

instance HasQGroundType '[ 'RangeCCRVariance] LangWholeModel where
    qGroundType = wholeModelGroundType

-- ImmutableWholeModel
instance (HasQType 'Negative a) => HasQType 'Negative (ImmutableWholeModel a) where
    qType = mapNegShimWit (functionToShim "langWholeModelToImmutable" $ langWholeModelToImmutable @BottomType) qType

instance (HasQType 'Positive a) => HasQType 'Positive (ImmutableWholeModel a) where
    qType = mapPosShimWit (functionToShim "immutableToWholeModel" immutableToWholeModel) qType

-- Literal types
instance HasQGroundType '[] Literal where
    qGroundType = literalGroundType

unitGroundType :: QGroundType '[] ()
unitGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily ())|]) "Unit")
        {qgtGreatestDynamicSupertype = literalGreatestDynamicSupertype}

instance HasQGroundType '[] () where
    qGroundType = unitGroundType

textGroundType :: QGroundType '[] Text
textGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Text)|]) "Text")
        {qgtGreatestDynamicSupertype = literalGreatestDynamicSupertype}

textShimWit ::
       forall polarity. Is PolarityType polarity
    => QShimWit polarity Text
textShimWit = typeToDolan $ MkDolanGroundedType textGroundType NilCCRArguments

instance HasQGroundType '[] Text where
    qGroundType = textGroundType

numberGroundType :: QGroundType '[] Number
numberGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Number)|]) "Number")
        {qgtGreatestDynamicSupertype = literalGreatestDynamicSupertype}

instance HasQGroundType '[] Number where
    qGroundType = numberGroundType

rationalGroundType :: QGroundType '[] SafeRational
rationalGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily SafeRational)|]) "Rational")
        {qgtGreatestDynamicSupertype = literalGreatestDynamicSupertype}

instance HasQGroundType '[] SafeRational where
    qGroundType = rationalGroundType

integerGroundType :: QGroundType '[] Integer
integerGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Integer)|]) "Integer")
        {qgtGreatestDynamicSupertype = literalGreatestDynamicSupertype}

instance HasQGroundType '[] Integer where
    qGroundType = integerGroundType

booleanGroundType :: QGroundType '[] Bool
booleanGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Bool)|]) "Boolean")
        {qgtGreatestDynamicSupertype = literalGreatestDynamicSupertype}

instance HasQGroundType '[] Bool where
    qGroundType = booleanGroundType

orderingGroundType :: QGroundType '[] Ordering
orderingGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Ordering)|]) "Ordering")
        {qgtGreatestDynamicSupertype = literalGreatestDynamicSupertype}

instance HasQGroundType '[] Ordering where
    qGroundType = orderingGroundType

timeGroundType :: QGroundType '[] UTCTime
timeGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily UTCTime)|]) "Time")
        {qgtGreatestDynamicSupertype = literalGreatestDynamicSupertype}

instance HasQGroundType '[] UTCTime where
    qGroundType = timeGroundType

durationGroundType :: QGroundType '[] NominalDiffTime
durationGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily NominalDiffTime)|]) "Duration")
        {qgtGreatestDynamicSupertype = literalGreatestDynamicSupertype}

instance HasQGroundType '[] NominalDiffTime where
    qGroundType = durationGroundType

dateGroundType :: QGroundType '[] Day
dateGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Day)|]) "Date")
        {qgtGreatestDynamicSupertype = literalGreatestDynamicSupertype}

instance HasQGroundType '[] Day where
    qGroundType = dateGroundType

timeOfDayGroundType :: QGroundType '[] TimeOfDay
timeOfDayGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily TimeOfDay)|]) "TimeOfDay")
        {qgtGreatestDynamicSupertype = literalGreatestDynamicSupertype}

instance HasQGroundType '[] TimeOfDay where
    qGroundType = timeOfDayGroundType

localTimeGroundType :: QGroundType '[] LocalTime
localTimeGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LocalTime)|]) "LocalTime")
        {qgtGreatestDynamicSupertype = literalGreatestDynamicSupertype}

instance HasQGroundType '[] LocalTime where
    qGroundType = localTimeGroundType

-- Double
instance HasQType 'Positive Double where
    qType = mapPosShimWit (functionToShim "InexactNumber" InexactNumber) qType

instance HasQType 'Negative Double where
    qType = mapNegShimWit (functionToShim "numberToDouble" numberToDouble) qType

-- Int
instance HasQType 'Positive Int where
    qType = mapPosShimWit (functionToShim "toInteger" toInteger) qType

instance HasQType 'Negative Int where
    qType = mapNegShimWit (functionToShim "fromInteger" fromInteger) qType

-- Int32
instance HasQType 'Positive Int32 where
    qType = mapPosShimWit (functionToShim "toInteger" toInteger) qType

instance HasQType 'Negative Int32 where
    qType = mapNegShimWit (functionToShim "fromInteger" fromInteger) qType

-- Int64
instance HasQType 'Positive Int64 where
    qType = mapPosShimWit (functionToShim "toInteger" toInteger) qType

instance HasQType 'Negative Int64 where
    qType = mapNegShimWit (functionToShim "fromInteger" fromInteger) qType

-- Rational
instance HasQType 'Positive Rational where
    qType = mapPosShimWit (functionToShim "fromRational" $ fromRational @SafeRational) qType

instance HasQType 'Negative Rational where
    qType = mapNegShimWit (functionToShim "toRational" $ toRational @SafeRational) qType

-- Fixed
instance HasResolution r => HasQType 'Positive (Fixed r) where
    qType = mapPosShimWit (functionToShim "toRational" toRational) qType

instance HasResolution r => HasQType 'Negative (Fixed r) where
    qType = mapNegShimWit (functionToShim "fromRational" fromRational) qType

-- Vector
instance HasQType 'Positive a => HasQType 'Positive (Vector a) where
    qType = mapPosShimWit (functionToShim "toList" toList) qType

instance HasQType 'Negative a => HasQType 'Negative (Vector a) where
    qType = mapNegShimWit (functionToShim "fromList" fromList) qType

-- SequencePoint
instance HasQType 'Positive SequencePoint where
    qType = mapPosShimWit (coerceShim "unSequencePoint") $ qType @_ @Int64

instance HasQType 'Negative SequencePoint where
    qType = mapNegShimWit (coerceShim "MkSequencePoint") $ qType @_ @Int64

-- SequenceRun
instance HasQType 'Positive SequenceRun where
    qType = mapPosShimWit (functionToShim "unSequenceRun" (\(MkSequenceRun s e) -> (s, e))) qType

instance HasQType 'Negative SequenceRun where
    qType = mapNegShimWit (functionToShim "MkSequenceRun" (\(s, e) -> MkSequenceRun s e)) qType
