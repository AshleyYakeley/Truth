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

type ToPinaforeType = ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive)

type FromPinaforeType = FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative)

-- top, bottom, join, meet
instance ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) BottomType where
    toShimWit = mkShimWit NilDolanType

instance FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) TopType where
    fromShimWit = mkShimWit NilDolanType

instance ( ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) b
         ) => ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (JoinType a b) where
    toShimWit = joinMeetShimWit toJMShimWit toJMShimWit

instance ( FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a
         , FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) b
         ) => FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (MeetType a b) where
    fromShimWit = joinMeetShimWit fromJMShimWit fromJMShimWit

-- Var Type
instance KnownSymbol name => ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) (Var name) where
    toShimWit = MkShimWit (VarDolanSingularType $ MkSymbolType @name) $ MkPolarMap $ coerceEnhanced "var"

instance KnownSymbol name => ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (Var name) where
    toShimWit = singleDolanShimWit toJMShimWit

instance KnownSymbol name => FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) (Var name) where
    fromShimWit = MkShimWit (VarDolanSingularType $ MkSymbolType @name) $ MkPolarMap $ coerceEnhanced "var"

instance KnownSymbol name => FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (Var name) where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- (,)
instance ( ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) b
         ) => ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) (a, b) where
    toShimWit =
        unPosShimWit toJMShimWit $ \ta conva ->
            unPosShimWit toJMShimWit $ \tb convb ->
                mapPosShimWit (applyCoPolyShim (cfmap conva) convb) $
                mkShimWit $
                GroundDolanSingularType
                    (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) PairEntityGroundType) $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) b
         ) => ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (a, b) where
    toShimWit = singleDolanShimWit toJMShimWit

instance ( FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a
         , FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) b
         ) => FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) (a, b) where
    fromShimWit =
        unNegShimWit fromJMShimWit $ \ta conva ->
            unNegShimWit fromJMShimWit $ \tb convb ->
                mapNegShimWit (applyCoPolyShim (cfmap conva) convb) $
                mkShimWit $
                GroundDolanSingularType
                    (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) PairEntityGroundType) $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a
         , FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) b
         ) => FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (Either a b) where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- Either
instance ( ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) b
         ) => ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) (Either a b) where
    toShimWit =
        unPosShimWit toJMShimWit $ \ta conva ->
            unPosShimWit toJMShimWit $ \tb convb ->
                mapPosShimWit (applyCoPolyShim (cfmap conva) convb) $
                mkShimWit $
                GroundDolanSingularType
                    (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) EitherEntityGroundType) $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) b
         ) => ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (Either a b) where
    toShimWit = singleDolanShimWit toJMShimWit

instance ( FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a
         , FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) b
         ) => FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) (Either a b) where
    fromShimWit =
        unNegShimWit fromJMShimWit $ \ta conva ->
            unNegShimWit fromJMShimWit $ \tb convb ->
                mapNegShimWit (applyCoPolyShim (cfmap conva) convb) $
                mkShimWit $
                GroundDolanSingularType
                    (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) EitherEntityGroundType) $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a
         , FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) b
         ) => FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (a, b) where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- (->)
instance ( FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) b
         ) => ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) (a -> b) where
    toShimWit =
        unNegShimWit fromJMShimWit $ \ta conva ->
            unPosShimWit toJMShimWit $ \tb convb ->
                mapPosShimWit (applyCoPolyShim (ccontramap conva) convb) $
                mkShimWit $
                GroundDolanSingularType FuncPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) b
         ) => ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (a -> b) where
    toShimWit = singleDolanShimWit toJMShimWit

instance ( ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a
         , FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) b
         ) => FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) (a -> b) where
    fromShimWit =
        unPosShimWit toJMShimWit $ \ta conva ->
            unNegShimWit fromJMShimWit $ \tb convb ->
                mapNegShimWit (applyCoPolyShim (ccontramap conva) convb) $
                mkShimWit $
                GroundDolanSingularType FuncPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a
         , FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) b
         ) => FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (a -> b) where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- Maybe
instance (ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a) =>
             ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) (Maybe a) where
    toShimWit =
        unPosShimWit toJMShimWit $ \ta conva ->
            mapPosShimWit (cfmap conva) $
            mkShimWit $
            GroundDolanSingularType (EntityPinaforeGroundType (ConsListType Refl NilListType) MaybeEntityGroundType) $
            ConsDolanArguments ta NilDolanArguments

instance (ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a) =>
             ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (Maybe a) where
    toShimWit = singleDolanShimWit toJMShimWit

instance (FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a) =>
             FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) (Maybe a) where
    fromShimWit =
        unNegShimWit fromJMShimWit $ \ta conva ->
            mapNegShimWit (cfmap conva) $
            mkShimWit $
            GroundDolanSingularType (EntityPinaforeGroundType (ConsListType Refl NilListType) MaybeEntityGroundType) $
            ConsDolanArguments ta NilDolanArguments

instance (FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a) =>
             FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (Maybe a) where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- []
instance (ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a) =>
             ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) [a] where
    toShimWit =
        unPosShimWit toJMShimWit $ \ta conva ->
            mapPosShimWit (cfmap conva) $
            mkShimWit $
            GroundDolanSingularType (EntityPinaforeGroundType (ConsListType Refl NilListType) ListEntityGroundType) $
            ConsDolanArguments ta NilDolanArguments

instance (ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a) =>
             ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) [a] where
    toShimWit = singleDolanShimWit toJMShimWit

instance (FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a) =>
             FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) [a] where
    fromShimWit =
        unNegShimWit fromJMShimWit $ \ta conva ->
            mapNegShimWit (cfmap conva) $
            mkShimWit $
            GroundDolanSingularType (EntityPinaforeGroundType (ConsListType Refl NilListType) ListEntityGroundType) $
            ConsDolanArguments ta NilDolanArguments

instance (FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a) =>
             FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) [a] where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- PinaforeAction
instance (ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a) =>
             ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) (PinaforeAction a) where
    toShimWit =
        unPosShimWit toJMShimWit $ \ta conva ->
            mapPosShimWit (cfmap conva) $
            mkShimWit $ GroundDolanSingularType ActionPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a) =>
             ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (PinaforeAction a) where
    toShimWit = singleDolanShimWit toJMShimWit

instance (FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a) =>
             FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) (PinaforeAction a) where
    fromShimWit =
        unNegShimWit fromJMShimWit $ \ta conva ->
            mapNegShimWit (cfmap conva) $
            mkShimWit $ GroundDolanSingularType ActionPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a) =>
             FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (PinaforeAction a) where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- LangWholeRef
instance ( FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) p
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) q
         ) => ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) (LangWholeRef '( p, q)) where
    toShimWit =
        unToRangeShimWit $ \tpq conv ->
            mapPosShimWit (applyPolyShim RangevarianceType cid conv) $
            mkShimWit $ GroundDolanSingularType WholeRefPinaforeGroundType $ ConsDolanArguments tpq NilDolanArguments

instance ( FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) p
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) q
         ) => ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (LangWholeRef '( p, q)) where
    toShimWit = singleDolanShimWit toJMShimWit

instance ( ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) p
         , FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) q
         ) => FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) (LangWholeRef '( p, q)) where
    fromShimWit =
        unFromRangeShimWit $ \tpq conv ->
            mapNegShimWit (applyPolyShim RangevarianceType cid conv) $
            mkShimWit $ GroundDolanSingularType WholeRefPinaforeGroundType $ ConsDolanArguments tpq NilDolanArguments

instance ( ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) p
         , FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) q
         ) => FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (LangWholeRef '( p, q)) where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- PinaforeImmutableWholeRef
instance (FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a) =>
             FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (PinaforeImmutableWholeRef a) where
    fromShimWit = mapNegShimWit (functionToShim "subtype" langWholeRefToImmutable) fromJMShimWit

instance (ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a) =>
             ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (PinaforeImmutableWholeRef a) where
    toShimWit = mapPosShimWit (functionToShim "subtype" pinaforeImmutableToWholeRef) toJMShimWit

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
instance ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) Double where
    toShimWit = mapPosShimWit (functionToShim "subtype" InexactNumber) toJMShimWit

instance FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) Double where
    fromShimWit = mapNegShimWit (functionToShim "subtype" numberToDouble) fromJMShimWit

-- Int
instance ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) Int where
    toShimWit = mapPosShimWit (functionToShim "subtype" toInteger) toJMShimWit

instance FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) Int where
    fromShimWit = mapNegShimWit (functionToShim "subtype" fromInteger) fromJMShimWit

-- Int32
instance ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) Int32 where
    toShimWit = mapPosShimWit (functionToShim "subtype" toInteger) toJMShimWit

instance FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) Int32 where
    fromShimWit = mapNegShimWit (functionToShim "subtype" fromInteger) fromJMShimWit

-- Rational
instance ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) Rational where
    toShimWit = mapPosShimWit (functionToShim "subtype" $ fromRational @SafeRational) toJMShimWit

instance FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) Rational where
    fromShimWit = mapNegShimWit (functionToShim "subtype" $ toRational @SafeRational) fromJMShimWit

-- Fixed
instance HasResolution r => ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (Fixed r) where
    toShimWit = mapPosShimWit (functionToShim "subtype" toRational) toJMShimWit

instance HasResolution r => FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (Fixed r) where
    fromShimWit = mapNegShimWit (functionToShim "subtype" fromRational) fromJMShimWit
