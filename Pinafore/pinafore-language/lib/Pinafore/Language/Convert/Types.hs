{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Convert.Types where

import Data.Time
import Shapes.Numeric

import Import
import Pinafore.Language.Convert.HasType
import Pinafore.Language.Convert.Literal
import Pinafore.Language.Type
import Pinafore.Language.Value
import Pinafore.Language.Var

class (CCRVariancesShim pshim, JoinMeetIsoShim (pshim Type)) => FromQIsoShim (pshim :: PolyShimKind) where
    fromQShims :: forall a b. QShim a b -> QShim b a -> pshim Type a b

instance FromQIsoShim QPolyIsoShim where
    fromQShims ab ba = MkPolyMapT $ MkIsomorphism ab ba

instance FromQIsoShim QPolyShim where
    fromQShims ab _ = ab

instance FromQIsoShim (PolyMapT CatDual QPolyShim) where
    fromQShims _ ba = MkPolyMapT $ MkCatDual ba

fromQShimsPolar ::
    forall (pshim :: PolyShimKind) polarity a b.
    (FromQIsoShim pshim, Is PolarityType polarity) =>
    QShim a b ->
    QShim b a ->
    PolarShim (pshim Type) polarity a b
fromQShimsPolar ab ba =
    case polarityType @polarity of
        PositiveType -> MkPolarShim $ fromQShims ab ba
        NegativeType -> MkPolarShim $ fromQShims ba ab

mapQIsoShimWit ::
    forall (pshim :: PolyShimKind) polarity a b.
    (FromQIsoShim pshim, Is PolarityType polarity) =>
    QShim a b ->
    QShim b a ->
    PShimWit (pshim Type) QType polarity b ->
    PShimWit (pshim Type) QType polarity a
mapQIsoShimWit ab ba = mapShimWit $ fromQShimsPolar ab ba

-- top, bottom, join, meet
instance forall (pshim :: PolyShimKind). CCRVariancesShim pshim => HasQType pshim 'Positive BottomType where
    qType = nilDolanShimWit

instance forall (pshim :: PolyShimKind). CCRVariancesShim pshim => HasQType pshim 'Negative TopType where
    qType = nilDolanShimWit

instance
    forall (pshim :: PolyShimKind) a b.
    ( CCRVariancesShim pshim
    , JoinMeetIsoShim (pshim Type)
    , HasQType pshim 'Positive a
    , HasQType pshim 'Positive b
    ) =>
    HasQType pshim 'Positive (JoinType a b)
    where
    qType = joinMeetShimWit qType qType

instance
    forall (pshim :: PolyShimKind) a b.
    ( CCRVariancesShim pshim
    , JoinMeetIsoShim (pshim Type)
    , HasQType pshim 'Negative a
    , HasQType pshim 'Negative b
    ) =>
    HasQType pshim 'Negative (MeetType a b)
    where
    qType = joinMeetShimWit qType qType

-- Var Type
instance
    forall (pshim :: PolyShimKind) polarity name.
    ( CCRVariancesShim pshim
    , JoinMeetIsoShim (pshim Type)
    , CoerceShim (pshim Type)
    , Is PolarityType polarity
    , KnownSymbol name
    ) =>
    HasQType pshim polarity (Var name)
    where
    qType =
        shimWitToDolan
            $ MkShimWit (VarDolanSingularType $ MkTypeVar $ MkSymbolType @name)
            $ case polarityType @polarity of
                PositiveType -> MkPolarShim $ coerceShim "var"
                NegativeType -> MkPolarShim $ coerceShim "var"

-- (,)
instance HasQGroundType '[CoCCRVariance, CoCCRVariance] (,) where
    qGroundType = pairGroundType

-- Either
instance HasQGroundType '[CoCCRVariance, CoCCRVariance] Either where
    qGroundType = eitherGroundType

-- Result
instance HasQGroundType '[CoCCRVariance, CoCCRVariance] Result where
    qGroundType = resultGroundType

-- (->)
instance HasQGroundType '[ContraCCRVariance, CoCCRVariance] (->) where
    qGroundType = funcGroundType

-- Maybe
instance HasQGroundType '[CoCCRVariance] Maybe where
    qGroundType = maybeGroundType

-- []
instance HasQGroundType '[CoCCRVariance] [] where
    qGroundType = listGroundType

-- NonEmpty
instance HasQGroundType '[CoCCRVariance] NonEmpty where
    qGroundType = list1GroundType

-- EntityMap
instance HasQGroundType '[CoCCRVariance] EntityMap where
    qGroundType = entityMapGroundType

-- Showable
instance HasQGroundType '[] Showable where
    qGroundType = showableGroundType

-- Equivalence
instance HasQGroundType '[ContraCCRVariance] Equivalence where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Equivalence)|]) "Equivalence"

-- Preorder
instance HasQGroundType '[ContraCCRVariance] Preorder where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Preorder)|]) "Preorder"

-- Order
instance HasQGroundType '[ContraCCRVariance] Order where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Order)|]) "Order"

showableShimWit ::
    forall polarity.
    Is PolarityType polarity =>
    QShimWit polarity Showable
showableShimWit = typeToDolan $ MkDolanGroundedType showableGroundType NilCCRArguments

-- Action
instance HasQGroundType '[CoCCRVariance] Action where
    qGroundType = actionGroundType

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

unitGroundType :: QGroundType '[] ()
unitGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily ())|]) "Unit"

instance HasQGroundType '[] () where
    qGroundType = unitGroundType

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
