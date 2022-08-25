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
        shimWitToDolan $
        MkShimWit (VarDolanSingularType $ MkSymbolType @name) $
        case polarityType @polarity of
            PositiveType -> MkPolarMap $ coerceShim "var"
            NegativeType -> MkPolarMap $ coerceShim "var"

-- (,)
instance HasPinaforeGroundType '[ CoCCRVariance, CoCCRVariance] (,) where
    pinaforeGroundType = pairGroundType

-- Either
instance HasPinaforeGroundType '[ CoCCRVariance, CoCCRVariance] Either where
    pinaforeGroundType = eitherGroundType

-- (->)
instance HasPinaforeGroundType '[ ContraCCRVariance, CoCCRVariance] (->) where
    pinaforeGroundType = funcGroundType

-- Maybe
instance HasPinaforeGroundType '[ CoCCRVariance] Maybe where
    pinaforeGroundType = maybeGroundType

-- []
instance HasPinaforeGroundType '[ CoCCRVariance] [] where
    pinaforeGroundType = listGroundType

-- NonEmpty
instance HasPinaforeGroundType '[ CoCCRVariance] NonEmpty where
    pinaforeGroundType = list1GroundType

-- PinaforeAction
instance HasPinaforeGroundType '[ CoCCRVariance] PinaforeAction where
    pinaforeGroundType = actionGroundType

-- LangWholeRef
wholeRefGroundType :: PinaforeGroundType '[ 'RangeCCRVariance] LangWholeRef
wholeRefGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangWholeRef)|]) "WholeRef"

instance HasPinaforeGroundType '[ 'RangeCCRVariance] LangWholeRef where
    pinaforeGroundType = wholeRefGroundType

-- PinaforeImmutableWholeRef
instance (HasPinaforeType 'Negative a) => HasPinaforeType 'Negative (PinaforeImmutableWholeRef a) where
    pinaforeType =
        mapNegShimWit (functionToShim "langWholeRefToImmutable" $ langWholeRefToImmutable @BottomType) pinaforeType

instance (HasPinaforeType 'Positive a) => HasPinaforeType 'Positive (PinaforeImmutableWholeRef a) where
    pinaforeType = mapPosShimWit (functionToShim "pinaforeImmutableToWholeRef" pinaforeImmutableToWholeRef) pinaforeType

-- Literal types
instance HasPinaforeGroundType '[] Literal where
    pinaforeGroundType = literalGroundType

unitGroundType :: PinaforeGroundType '[] ()
unitGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily ())|]) "Unit")
        {pgtGreatestDynamicSupertype = literalGreatestDynamicSupertype}

instance HasPinaforeGroundType '[] () where
    pinaforeGroundType = unitGroundType

textGroundType :: PinaforeGroundType '[] Text
textGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Text)|]) "Text")
        {pgtGreatestDynamicSupertype = literalGreatestDynamicSupertype}

textShimWit ::
       forall polarity. Is PolarityType polarity
    => PinaforeShimWit polarity Text
textShimWit = typeToDolan $ MkDolanGroundedType textGroundType NilCCRArguments

instance HasPinaforeGroundType '[] Text where
    pinaforeGroundType = textGroundType

numberGroundType :: PinaforeGroundType '[] Number
numberGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Number)|]) "Number")
        {pgtGreatestDynamicSupertype = literalGreatestDynamicSupertype}

instance HasPinaforeGroundType '[] Number where
    pinaforeGroundType = numberGroundType

rationalGroundType :: PinaforeGroundType '[] SafeRational
rationalGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily SafeRational)|]) "Rational")
        {pgtGreatestDynamicSupertype = literalGreatestDynamicSupertype}

instance HasPinaforeGroundType '[] SafeRational where
    pinaforeGroundType = rationalGroundType

integerGroundType :: PinaforeGroundType '[] Integer
integerGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Integer)|]) "Integer")
        {pgtGreatestDynamicSupertype = literalGreatestDynamicSupertype}

instance HasPinaforeGroundType '[] Integer where
    pinaforeGroundType = integerGroundType

booleanGroundType :: PinaforeGroundType '[] Bool
booleanGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Bool)|]) "Boolean")
        {pgtGreatestDynamicSupertype = literalGreatestDynamicSupertype}

instance HasPinaforeGroundType '[] Bool where
    pinaforeGroundType = booleanGroundType

orderingGroundType :: PinaforeGroundType '[] Ordering
orderingGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Ordering)|]) "Ordering")
        {pgtGreatestDynamicSupertype = literalGreatestDynamicSupertype}

instance HasPinaforeGroundType '[] Ordering where
    pinaforeGroundType = orderingGroundType

timeGroundType :: PinaforeGroundType '[] UTCTime
timeGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily UTCTime)|]) "Time")
        {pgtGreatestDynamicSupertype = literalGreatestDynamicSupertype}

instance HasPinaforeGroundType '[] UTCTime where
    pinaforeGroundType = timeGroundType

durationGroundType :: PinaforeGroundType '[] NominalDiffTime
durationGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily NominalDiffTime)|]) "Duration")
        {pgtGreatestDynamicSupertype = literalGreatestDynamicSupertype}

instance HasPinaforeGroundType '[] NominalDiffTime where
    pinaforeGroundType = durationGroundType

dateGroundType :: PinaforeGroundType '[] Day
dateGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Day)|]) "Date")
        {pgtGreatestDynamicSupertype = literalGreatestDynamicSupertype}

instance HasPinaforeGroundType '[] Day where
    pinaforeGroundType = dateGroundType

timeOfDayGroundType :: PinaforeGroundType '[] TimeOfDay
timeOfDayGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily TimeOfDay)|]) "TimeOfDay")
        {pgtGreatestDynamicSupertype = literalGreatestDynamicSupertype}

instance HasPinaforeGroundType '[] TimeOfDay where
    pinaforeGroundType = timeOfDayGroundType

localTimeGroundType :: PinaforeGroundType '[] LocalTime
localTimeGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LocalTime)|]) "LocalTime")
        {pgtGreatestDynamicSupertype = literalGreatestDynamicSupertype}

instance HasPinaforeGroundType '[] LocalTime where
    pinaforeGroundType = localTimeGroundType

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

-- SequencePoint
instance HasPinaforeType 'Positive SequencePoint where
    pinaforeType = mapPosShimWit (coerceShim "unSequencePoint") $ pinaforeType @_ @Int64

instance HasPinaforeType 'Negative SequencePoint where
    pinaforeType = mapNegShimWit (coerceShim "MkSequencePoint") $ pinaforeType @_ @Int64

-- SequenceRun
instance HasPinaforeType 'Positive SequenceRun where
    pinaforeType = mapPosShimWit (functionToShim "unSequenceRun" (\(MkSequenceRun s e) -> (s, e))) pinaforeType

instance HasPinaforeType 'Negative SequenceRun where
    pinaforeType = mapNegShimWit (functionToShim "MkSequenceRun" (\(s, e) -> MkSequenceRun s e)) pinaforeType
