{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Convert where

import Data.Time
import Pinafore.Base
import Pinafore.Language.Convert.TH
import Pinafore.Language.Shim
import Pinafore.Language.Type
import Pinafore.Language.Value
import Pinafore.Language.Var
import Shapes
import Shapes.Numeric

type ToPinaforeType = ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive)

type FromPinaforeType = FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative)

-- top, bottom, join, meet
instance ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) BottomType where
    toPolarShimWit = mkPolarShimWit NilDolanType

instance FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) TopType where
    fromPolarShimWit = mkPolarShimWit NilDolanType

instance ( ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a
         , ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) b
         ) => ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (JoinType a b) where
    toPolarShimWit = joinMeetShimWit toJMShimWit toJMShimWit

instance ( FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a
         , FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) b
         ) => FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (MeetType a b) where
    fromPolarShimWit = joinMeetShimWit fromJMShimWit fromJMShimWit

-- Var Type
instance KnownSymbol name => ToPolarShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) (Var name) where
    toPolarShimWit = MkShimWit (VarDolanSingularType $ MkSymbolType @name) $ MkPolarMap $ coerceEnhanced "var"

instance KnownSymbol name => ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (Var name) where
    toPolarShimWit = singleDolanShimWit toJMShimWit

instance KnownSymbol name => FromPolarShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) (Var name) where
    fromPolarShimWit = MkShimWit (VarDolanSingularType $ MkSymbolType @name) $ MkPolarMap $ coerceEnhanced "var"

instance KnownSymbol name => FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (Var name) where
    fromPolarShimWit = singleDolanShimWit fromJMShimWit

-- (,)
instance ( ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a
         , ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) b
         ) => ToPolarShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) (a, b) where
    toPolarShimWit =
        unPosShimWit toJMShimWit $ \ta conva ->
            unPosShimWit toJMShimWit $ \tb convb ->
                mapPosShimWit (applyCoPolyShim (cfmap conva) convb) $
                mkPolarShimWit $
                GroundedDolanSingularType
                    (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) PairEntityGroundType) $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a
         , ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) b
         ) => ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (a, b) where
    toPolarShimWit = singleDolanShimWit toJMShimWit

instance ( FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a
         , FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) b
         ) => FromPolarShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) (a, b) where
    fromPolarShimWit =
        unNegShimWit fromJMShimWit $ \ta conva ->
            unNegShimWit fromJMShimWit $ \tb convb ->
                mapNegShimWit (applyCoPolyShim (cfmap conva) convb) $
                mkPolarShimWit $
                GroundedDolanSingularType
                    (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) PairEntityGroundType) $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a
         , FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) b
         ) => FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (a, b) where
    fromPolarShimWit = singleDolanShimWit fromJMShimWit

-- Either
instance ( ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a
         , ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) b
         ) => ToPolarShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) (Either a b) where
    toPolarShimWit =
        unPosShimWit toJMShimWit $ \ta conva ->
            unPosShimWit toJMShimWit $ \tb convb ->
                mapPosShimWit (applyCoPolyShim (cfmap conva) convb) $
                mkPolarShimWit $
                GroundedDolanSingularType
                    (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) EitherEntityGroundType) $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a
         , ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) b
         ) => ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (Either a b) where
    toPolarShimWit = singleDolanShimWit toJMShimWit

instance ( FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a
         , FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) b
         ) => FromPolarShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) (Either a b) where
    fromPolarShimWit =
        unNegShimWit fromJMShimWit $ \ta conva ->
            unNegShimWit fromJMShimWit $ \tb convb ->
                mapNegShimWit (applyCoPolyShim (cfmap conva) convb) $
                mkPolarShimWit $
                GroundedDolanSingularType
                    (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) EitherEntityGroundType) $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a
         , FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) b
         ) => FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (Either a b) where
    fromPolarShimWit = singleDolanShimWit fromJMShimWit

-- (->)
instance ( FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a
         , ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) b
         ) => ToPolarShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) (a -> b) where
    toPolarShimWit =
        unNegShimWit fromJMShimWit $ \ta conva ->
            unPosShimWit toJMShimWit $ \tb convb ->
                mapPosShimWit (applyCoPolyShim (ccontramap conva) convb) $
                mkPolarShimWit $
                GroundedDolanSingularType funcGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a
         , ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) b
         ) => ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (a -> b) where
    toPolarShimWit = singleDolanShimWit toJMShimWit

instance ( ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a
         , FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) b
         ) => FromPolarShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) (a -> b) where
    fromPolarShimWit =
        unPosShimWit toJMShimWit $ \ta conva ->
            unNegShimWit fromJMShimWit $ \tb convb ->
                mapNegShimWit (applyCoPolyShim (ccontramap conva) convb) $
                mkPolarShimWit $
                GroundedDolanSingularType funcGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a
         , FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) b
         ) => FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (a -> b) where
    fromPolarShimWit = singleDolanShimWit fromJMShimWit

-- Maybe
instance (ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a) =>
             ToPolarShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) (Maybe a) where
    toPolarShimWit =
        unPosShimWit toJMShimWit $ \ta conva ->
            mapPosShimWit (cfmap conva) $
            mkPolarShimWit $
            GroundedDolanSingularType (EntityPinaforeGroundType (ConsListType Refl NilListType) MaybeEntityGroundType) $
            ConsDolanArguments ta NilDolanArguments

instance (ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a) =>
             ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (Maybe a) where
    toPolarShimWit = singleDolanShimWit toJMShimWit

instance (FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a) =>
             FromPolarShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) (Maybe a) where
    fromPolarShimWit =
        unNegShimWit fromJMShimWit $ \ta conva ->
            mapNegShimWit (cfmap conva) $
            mkPolarShimWit $
            GroundedDolanSingularType (EntityPinaforeGroundType (ConsListType Refl NilListType) MaybeEntityGroundType) $
            ConsDolanArguments ta NilDolanArguments

instance (FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a) =>
             FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (Maybe a) where
    fromPolarShimWit = singleDolanShimWit fromJMShimWit

-- []
instance (ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a) =>
             ToPolarShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) [a] where
    toPolarShimWit =
        unPosShimWit toJMShimWit $ \ta conva ->
            mapPosShimWit (cfmap conva) $
            mkPolarShimWit $ GroundedDolanSingularType listGroundType $ ConsDolanArguments ta NilDolanArguments

instance (ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a) =>
             ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) [a] where
    toPolarShimWit = singleDolanShimWit toJMShimWit

instance (FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a) =>
             FromPolarShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) [a] where
    fromPolarShimWit =
        unNegShimWit fromJMShimWit $ \ta conva ->
            mapNegShimWit (cfmap conva) $
            mkPolarShimWit $ GroundedDolanSingularType listGroundType $ ConsDolanArguments ta NilDolanArguments

instance (FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a) =>
             FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) [a] where
    fromPolarShimWit = singleDolanShimWit fromJMShimWit

-- PinaforeAction
instance (ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a) =>
             ToPolarShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) (PinaforeAction a) where
    toPolarShimWit =
        unPosShimWit toJMShimWit $ \ta conva ->
            mapPosShimWit (cfmap conva) $
            mkPolarShimWit $ GroundedDolanSingularType actionGroundType $ ConsDolanArguments ta NilDolanArguments

instance (ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a) =>
             ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (PinaforeAction a) where
    toPolarShimWit = singleDolanShimWit toJMShimWit

instance (FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a) =>
             FromPolarShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) (PinaforeAction a) where
    fromPolarShimWit =
        unNegShimWit fromJMShimWit $ \ta conva ->
            mapNegShimWit (cfmap conva) $
            mkPolarShimWit $ GroundedDolanSingularType actionGroundType $ ConsDolanArguments ta NilDolanArguments

instance (FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a) =>
             FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (PinaforeAction a) where
    fromPolarShimWit = singleDolanShimWit fromJMShimWit

-- LangWholeRef
instance ( FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) p
         , ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) q
         ) => ToPolarShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) (LangWholeRef '( p, q)) where
    toPolarShimWit =
        unToRangeShimWit $ \tpq conv ->
            mapPosShimWit (applyPolyShim RangevarianceType cid conv) $
            mkPolarShimWit $ GroundedDolanSingularType wholeRefGroundType $ ConsDolanArguments tpq NilDolanArguments

instance ( FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) p
         , ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) q
         ) => ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (LangWholeRef '( p, q)) where
    toPolarShimWit = singleDolanShimWit toJMShimWit

instance ( ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) p
         , FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) q
         ) => FromPolarShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) (LangWholeRef '( p, q)) where
    fromPolarShimWit =
        unFromRangeShimWit $ \tpq conv ->
            mapNegShimWit (applyPolyShim RangevarianceType cid conv) $
            mkPolarShimWit $ GroundedDolanSingularType wholeRefGroundType $ ConsDolanArguments tpq NilDolanArguments

instance ( ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) p
         , FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) q
         ) => FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (LangWholeRef '( p, q)) where
    fromPolarShimWit = singleDolanShimWit fromJMShimWit

-- PinaforeImmutableWholeRef
instance (FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a) =>
             FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (PinaforeImmutableWholeRef a) where
    fromPolarShimWit = mapNegShimWit (functionToShim "subtype" $ langWholeRefToImmutable @BottomType) fromJMShimWit

instance (ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a) =>
             ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (PinaforeImmutableWholeRef a) where
    toPolarShimWit = mapPosShimWit (functionToShim "subtype" pinaforeImmutableToWholeRef) toJMShimWit

-- Literal types
$(literalInstances [t|Literal|])

$(literalInstances [t|Text|])

$(literalInstances [t|Number|])

$(literalInstances [t|SafeRational|])

$(literalInstances [t|Integer|])

$(literalInstances [t|Bool|])

$(literalInstances [t|Ordering|])

$(literalInstances [t|UTCTime|])

$(literalInstances [t|NominalDiffTime|])

$(literalInstances [t|Day|])

$(literalInstances [t|TimeOfDay|])

$(literalInstances [t|LocalTime|])

$(literalInstances [t|()|])

-- Double
instance ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) Double where
    toPolarShimWit = mapPosShimWit (functionToShim "subtype" InexactNumber) toJMShimWit

instance FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) Double where
    fromPolarShimWit = mapNegShimWit (functionToShim "subtype" numberToDouble) fromJMShimWit

-- Int
instance ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) Int where
    toPolarShimWit = mapPosShimWit (functionToShim "subtype" toInteger) toJMShimWit

instance FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) Int where
    fromPolarShimWit = mapNegShimWit (functionToShim "subtype" fromInteger) fromJMShimWit

-- Int32
instance ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) Int32 where
    toPolarShimWit = mapPosShimWit (functionToShim "subtype" toInteger) toJMShimWit

instance FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) Int32 where
    fromPolarShimWit = mapNegShimWit (functionToShim "subtype" fromInteger) fromJMShimWit

-- Rational
instance ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) Rational where
    toPolarShimWit = mapPosShimWit (functionToShim "subtype" $ fromRational @SafeRational) toJMShimWit

instance FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) Rational where
    fromPolarShimWit = mapNegShimWit (functionToShim "subtype" $ toRational @SafeRational) fromJMShimWit

-- Fixed
instance HasResolution r => ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (Fixed r) where
    toPolarShimWit = mapPosShimWit (functionToShim "subtype" toRational) toJMShimWit

instance HasResolution r => FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (Fixed r) where
    fromPolarShimWit = mapNegShimWit (functionToShim "subtype" fromRational) fromJMShimWit
