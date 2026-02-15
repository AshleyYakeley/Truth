{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Convert.Types where

import Data.Time
import Shapes.Numeric

import Import
import Pinafore.Language.Convert.FromQIsoShim
import Pinafore.Language.Convert.HasType
import Pinafore.Language.Convert.JoinMeet ()
import Pinafore.Language.Convert.Literal
import Pinafore.Language.Convert.Sequence ()
import Pinafore.Language.Type
import Pinafore.Language.Value

-- (->)
instance HasQGroundType '[ContraCCRVariance, CoCCRVariance] (->) where
    qGroundType = funcGroundType

-- Equivalence
instance HasQGroundType '[ContraCCRVariance] Equivalence where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Equivalence)|]) "Equivalence"

-- Preorder
instance HasQGroundType '[ContraCCRVariance] Preorder where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Preorder)|]) "Preorder"

-- Order
instance HasQGroundType '[ContraCCRVariance] Order where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Order)|]) "Order"

-- LangWholeModel
wholeModelGroundType :: QGroundType '[ 'RangeCCRVariance] LangWholeModel
wholeModelGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangWholeModel)|]) "WholeModel"

instance HasQGroundType '[ 'RangeCCRVariance] LangWholeModel where
    qGroundType = wholeModelGroundType

-- ImmutableWholeModel
instance HasQType QPolyShim 'Negative a => HasQType QPolyShim 'Negative (ImmutableWholeModel a) where
    qType = mapNegShimWit (functionToShim "langWholeModelToImmutable" $ langWholeModelToImmutable @BottomType) qType

instance HasQType QPolyShim 'Positive a => HasQType QPolyShim 'Positive (ImmutableWholeModel a) where
    qType = mapPosShimWit (functionToShim "immutableToWholeModel" immutableToWholeModel) qType

voidGroundType :: QGroundType '[] Void
voidGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily Void)|]) "Void"

instance HasQGroundType '[] Void where
    qGroundType = voidGroundType

textGroundType :: QGroundType '[] Text
textGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily Text)|]) "Text"

textShimWit ::
    forall polarity.
    Is PolarityType polarity =>
    QShimWit polarity Text
textShimWit = typeToDolan $ MkDolanGroundedType textGroundType NilCCRArguments

instance HasQGroundType '[] Text where
    qGroundType = textGroundType

numberGroundType :: QGroundType '[] Number
numberGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily Number)|]) "Number"

instance HasQGroundType '[] Number where
    qGroundType = numberGroundType

rationalGroundType :: QGroundType '[] SafeRational
rationalGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily SafeRational)|]) "Rational"

instance HasQGroundType '[] SafeRational where
    qGroundType = rationalGroundType

integerGroundType :: QGroundType '[] Integer
integerGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily Integer)|]) "Integer"

instance HasQGroundType '[] Integer where
    qGroundType = integerGroundType

naturalGroundType :: QGroundType '[] Natural
naturalGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily Natural)|]) "Natural"

instance HasQGroundType '[] Natural where
    qGroundType = naturalGroundType

booleanGroundType :: QGroundType '[] Bool
booleanGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily Bool)|]) "Boolean"

instance HasQGroundType '[] Bool where
    qGroundType = booleanGroundType

orderingGroundType :: QGroundType '[] Ordering
orderingGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily Ordering)|]) "Ordering"

instance HasQGroundType '[] Ordering where
    qGroundType = orderingGroundType

timeGroundType :: QGroundType '[] UTCTime
timeGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily UTCTime)|]) "Time"

instance HasQGroundType '[] UTCTime where
    qGroundType = timeGroundType

durationGroundType :: QGroundType '[] NominalDiffTime
durationGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily NominalDiffTime)|]) "Duration"

instance HasQGroundType '[] NominalDiffTime where
    qGroundType = durationGroundType

dateGroundType :: QGroundType '[] Day
dateGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily Day)|]) "Date"

instance HasQGroundType '[] Day where
    qGroundType = dateGroundType

timeOfDayGroundType :: QGroundType '[] TimeOfDay
timeOfDayGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily TimeOfDay)|]) "TimeOfDay"

instance HasQGroundType '[] TimeOfDay where
    qGroundType = timeOfDayGroundType

localTimeGroundType :: QGroundType '[] LocalTime
localTimeGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily LocalTime)|]) "LocalTime"

instance HasQGroundType '[] LocalTime where
    qGroundType = localTimeGroundType

instance HasQGroundType '[] PrecText where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily PrecText)|]) "PrecText"

-- Double
instance
    forall (pshim :: PolyShimKind) polarity.
    (FromQIsoShim pshim, Is PolarityType polarity) =>
    HasQType pshim polarity Double
    where
    qType =
        mapQIsoShimWit
            (functionToShim "InexactNumber" InexactNumber)
            (functionToShim "numberToDouble" numberToDouble)
            qType

-- Int
instance
    forall (pshim :: PolyShimKind) polarity.
    (FromQIsoShim pshim, Is PolarityType polarity) =>
    HasQType pshim polarity Int
    where
    qType = mapQIsoShimWit (functionToShim "toInteger" toInteger) (functionToShim "fromInteger" fromInteger) qType

-- Int8
instance
    forall (pshim :: PolyShimKind) polarity.
    (FromQIsoShim pshim, Is PolarityType polarity) =>
    HasQType pshim polarity Int8
    where
    qType = mapQIsoShimWit (functionToShim "toInteger" toInteger) (functionToShim "fromInteger" fromInteger) qType

-- Int16
instance
    forall (pshim :: PolyShimKind) polarity.
    (FromQIsoShim pshim, Is PolarityType polarity) =>
    HasQType pshim polarity Int16
    where
    qType = mapQIsoShimWit (functionToShim "toInteger" toInteger) (functionToShim "fromInteger" fromInteger) qType

-- Int32
instance
    forall (pshim :: PolyShimKind) polarity.
    (FromQIsoShim pshim, Is PolarityType polarity) =>
    HasQType pshim polarity Int32
    where
    qType = mapQIsoShimWit (functionToShim "toInteger" toInteger) (functionToShim "fromInteger" fromInteger) qType

-- Int64
instance
    forall (pshim :: PolyShimKind) polarity.
    (FromQIsoShim pshim, Is PolarityType polarity) =>
    HasQType pshim polarity Int64
    where
    qType = mapQIsoShimWit (functionToShim "toInteger" toInteger) (functionToShim "fromInteger" fromInteger) qType

-- Word
instance
    forall (pshim :: PolyShimKind) polarity.
    (FromQIsoShim pshim, Is PolarityType polarity) =>
    HasQType pshim polarity Word
    where
    qType =
        mapQIsoShimWit
            (functionToShim "toNaturalForce" toNaturalForce)
            (functionToShim "fromIntegral" fromIntegral)
            qType

-- Word8
instance
    forall (pshim :: PolyShimKind) polarity.
    (FromQIsoShim pshim, Is PolarityType polarity) =>
    HasQType pshim polarity Word8
    where
    qType =
        mapQIsoShimWit
            (functionToShim "toNaturalForce" toNaturalForce)
            (functionToShim "fromIntegral" fromIntegral)
            qType

-- Word16
instance
    forall (pshim :: PolyShimKind) polarity.
    (FromQIsoShim pshim, Is PolarityType polarity) =>
    HasQType pshim polarity Word16
    where
    qType =
        mapQIsoShimWit
            (functionToShim "toNaturalForce" toNaturalForce)
            (functionToShim "fromIntegral" fromIntegral)
            qType

-- Word32
instance
    forall (pshim :: PolyShimKind) polarity.
    (FromQIsoShim pshim, Is PolarityType polarity) =>
    HasQType pshim polarity Word32
    where
    qType =
        mapQIsoShimWit
            (functionToShim "toNaturalForce" toNaturalForce)
            (functionToShim "fromIntegral" fromIntegral)
            qType

-- Word64
instance
    forall (pshim :: PolyShimKind) polarity.
    (FromQIsoShim pshim, Is PolarityType polarity) =>
    HasQType pshim polarity Word64
    where
    qType =
        mapQIsoShimWit
            (functionToShim "toNaturalForce" toNaturalForce)
            (functionToShim "fromIntegral" fromIntegral)
            qType

-- Rational
instance
    forall (pshim :: PolyShimKind) polarity.
    (FromQIsoShim pshim, Is PolarityType polarity) =>
    HasQType pshim polarity Rational
    where
    qType =
        mapQIsoShimWit
            (functionToShim "fromRational" $ fromRational @SafeRational)
            (functionToShim "toRational" $ toRational @SafeRational)
            qType

-- Fixed
instance
    forall (pshim :: PolyShimKind) polarity r.
    (FromQIsoShim pshim, Is PolarityType polarity, HasResolution r) =>
    HasQType pshim polarity (Fixed r)
    where
    qType = mapQIsoShimWit (functionToShim "toRational" toRational) (functionToShim "realToFrac" realToFrac) qType

-- DiffTime
instance
    forall (pshim :: PolyShimKind) polarity.
    (FromQIsoShim pshim, Is PolarityType polarity) =>
    HasQType pshim polarity DiffTime
    where
    qType =
        mapQIsoShimWit
            (functionToShim "realToFrac" realToFrac)
            (functionToShim "realToFrac" realToFrac)
            (qType :: _ NominalDiffTime)

-- Vector
instance
    forall (pshim :: PolyShimKind) polarity a.
    ( FromQIsoShim pshim
    , Is PolarityType polarity
    , HasQType pshim polarity a
    ) =>
    HasQType pshim polarity (Vector a)
    where
    qType = mapQIsoShimWit (functionToShim "toList" toList) (functionToShim "fromList" fromList) qType

-- SequencePoint
instance
    forall (pshim :: PolyShimKind) polarity.
    (FromQIsoShim pshim, Is PolarityType polarity) =>
    HasQType pshim polarity SequencePoint
    where
    qType =
        mapQIsoShimWit
            (functionToShim "unSequencePoint" $ toNaturalForce . unSequencePoint)
            (functionToShim "MkSequencePoint" $ MkSequencePoint . fromIntegral)
            (qType :: _ Natural)

-- SequenceRun
instance
    forall (pshim :: PolyShimKind) polarity.
    (FromQIsoShim pshim, Is PolarityType polarity) =>
    HasQType pshim polarity SequenceRun
    where
    qType =
        mapQIsoShimWit
            (functionToShim "unSequenceRun" (\(MkSequenceRun s e) -> (s, e)))
            (functionToShim "MkSequenceRun" (\(s, e) -> MkSequenceRun s e))
            qType
