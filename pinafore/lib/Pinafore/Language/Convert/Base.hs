{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Convert.Base
    ( qPositiveTypeDescription
    , qNegativeTypeDescription
    , jmToValue
    , ToPinaforeType
    , FromPinaforeType
    , literalInstances
    ) where

import Data.Shim
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.Shim
import Pinafore.Language.TypeSystem
import Pinafore.Language.TypeSystem.Show
import Pinafore.Language.Value
import Shapes
import Truth.Core

qPositiveTypeDescription ::
       forall t. ToPinaforeType t
    => Text
qPositiveTypeDescription =
    case toShimWit @Type @(PinaforeShim Type) @(PinaforeType 'Positive) @t of
        MkShimWit w _ -> exprShow w

qNegativeTypeDescription ::
       forall t. FromPinaforeType t
    => Text
qNegativeTypeDescription =
    case fromShimWit @Type @(PinaforeShim Type) @(PinaforeType 'Negative) @t of
        MkShimWit w _ -> exprShow w

type ToPinaforeType = ToShimWit (PinaforeShim Type) (PinaforeType 'Positive)

type FromPinaforeType = FromShimWit (PinaforeShim Type) (PinaforeType 'Negative)

-- top, bottom, join, meet
instance ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) BottomType where
    toShimWit = mkShimWit NilDolanType

instance FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) TopType where
    fromShimWit = mkShimWit NilDolanType

instance ( ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) a
         , ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) b
         ) => ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) (JoinType a b) where
    toShimWit = joinMeetDolanShimWit toJMShimWit toJMShimWit

instance ( FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) a
         , FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) b
         ) => FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) (MeetType a b) where
    fromShimWit = joinMeetDolanShimWit fromJMShimWit fromJMShimWit

-- UVar
instance KnownSymbol name => ToShimWit (PinaforeShim Type) (PinaforeSingularType 'Positive) (UVar name) where
    toShimWit = mkShimWit $ VarDolanSingularType MkSymbolType

instance KnownSymbol name => ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) (UVar name) where
    toShimWit = singleDolanShimWit toJMShimWit

instance KnownSymbol name => FromShimWit (PinaforeShim Type) (PinaforeSingularType 'Negative) (UVar name) where
    fromShimWit = mkShimWit $ VarDolanSingularType MkSymbolType

instance KnownSymbol name => FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) (UVar name) where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- (,)
instance ( ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) a
         , ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) b
         ) => ToShimWit (PinaforeShim Type) (PinaforeSingularType 'Positive) (a, b) where
    toShimWit =
        unPosShimWit toJMShimWit $ \ta conva ->
            unPosShimWit toJMShimWit $ \tb convb ->
                mapPosShimWit (applyPolyShim CovarianceType (cfmap conva) convb) $
                mkShimWit $
                GroundDolanSingularType
                    (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) PairEntityGroundType) $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) a
         , ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) b
         ) => ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) (a, b) where
    toShimWit = singleDolanShimWit toJMShimWit

instance ( FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) a
         , FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) b
         ) => FromShimWit (PinaforeShim Type) (PinaforeSingularType 'Negative) (a, b) where
    fromShimWit =
        unNegShimWit fromJMShimWit $ \ta conva ->
            unNegShimWit fromJMShimWit $ \tb convb ->
                mapNegShimWit (applyPolyShim CovarianceType (cfmap conva) convb) $
                mkShimWit $
                GroundDolanSingularType
                    (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) PairEntityGroundType) $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) a
         , FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) b
         ) => FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) (Either a b) where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- Either
instance ( ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) a
         , ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) b
         ) => ToShimWit (PinaforeShim Type) (PinaforeSingularType 'Positive) (Either a b) where
    toShimWit =
        unPosShimWit toJMShimWit $ \ta conva ->
            unPosShimWit toJMShimWit $ \tb convb ->
                mapPosShimWit (applyPolyShim CovarianceType (cfmap conva) convb) $
                mkShimWit $
                GroundDolanSingularType
                    (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) EitherEntityGroundType) $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) a
         , ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) b
         ) => ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) (Either a b) where
    toShimWit = singleDolanShimWit toJMShimWit

instance ( FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) a
         , FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) b
         ) => FromShimWit (PinaforeShim Type) (PinaforeSingularType 'Negative) (Either a b) where
    fromShimWit =
        unNegShimWit fromJMShimWit $ \ta conva ->
            unNegShimWit fromJMShimWit $ \tb convb ->
                mapNegShimWit (applyPolyShim CovarianceType (cfmap conva) convb) $
                mkShimWit $
                GroundDolanSingularType
                    (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) EitherEntityGroundType) $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) a
         , FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) b
         ) => FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) (a, b) where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- (->)
instance ( FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) a
         , ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) b
         ) => ToShimWit (PinaforeShim Type) (PinaforeSingularType 'Positive) (a -> b) where
    toShimWit =
        unNegShimWit fromJMShimWit $ \ta conva ->
            unPosShimWit toJMShimWit $ \tb convb ->
                mapPosShimWit (applyPolyShim CovarianceType (ccontramap conva) convb) $
                mkShimWit $
                GroundDolanSingularType FuncPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) a
         , ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) b
         ) => ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) (a -> b) where
    toShimWit = singleDolanShimWit toJMShimWit

instance ( ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) a
         , FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) b
         ) => FromShimWit (PinaforeShim Type) (PinaforeSingularType 'Negative) (a -> b) where
    fromShimWit =
        unPosShimWit toJMShimWit $ \ta conva ->
            unNegShimWit fromJMShimWit $ \tb convb ->
                mapNegShimWit (applyPolyShim CovarianceType (ccontramap conva) convb) $
                mkShimWit $
                GroundDolanSingularType FuncPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) a
         , FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) b
         ) => FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) (a -> b) where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- Maybe
instance (ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) a) =>
             ToShimWit (PinaforeShim Type) (PinaforeSingularType 'Positive) (Maybe a) where
    toShimWit =
        unPosShimWit toJMShimWit $ \ta conva ->
            mapPosShimWit (cfmap conva) $
            mkShimWit $
            GroundDolanSingularType (EntityPinaforeGroundType (ConsListType Refl NilListType) MaybeEntityGroundType) $
            ConsDolanArguments ta NilDolanArguments

instance (ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) a) =>
             ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) (Maybe a) where
    toShimWit = singleDolanShimWit toJMShimWit

instance (FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) a) =>
             FromShimWit (PinaforeShim Type) (PinaforeSingularType 'Negative) (Maybe a) where
    fromShimWit =
        unNegShimWit fromJMShimWit $ \ta conva ->
            mapNegShimWit (cfmap conva) $
            mkShimWit $
            GroundDolanSingularType (EntityPinaforeGroundType (ConsListType Refl NilListType) MaybeEntityGroundType) $
            ConsDolanArguments ta NilDolanArguments

instance (FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) a) =>
             FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) (Maybe a) where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- []
instance (ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) a) =>
             ToShimWit (PinaforeShim Type) (PinaforeSingularType 'Positive) [a] where
    toShimWit =
        unPosShimWit toJMShimWit $ \ta conva ->
            mapPosShimWit (cfmap conva) $
            mkShimWit $
            GroundDolanSingularType (EntityPinaforeGroundType (ConsListType Refl NilListType) ListEntityGroundType) $
            ConsDolanArguments ta NilDolanArguments

instance (ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) a) =>
             ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) [a] where
    toShimWit = singleDolanShimWit toJMShimWit

instance (FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) a) =>
             FromShimWit (PinaforeShim Type) (PinaforeSingularType 'Negative) [a] where
    fromShimWit =
        unNegShimWit fromJMShimWit $ \ta conva ->
            mapNegShimWit (cfmap conva) $
            mkShimWit $
            GroundDolanSingularType (EntityPinaforeGroundType (ConsListType Refl NilListType) ListEntityGroundType) $
            ConsDolanArguments ta NilDolanArguments

instance (FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) a) =>
             FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) [a] where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- PinaforeAction
instance (ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) a) =>
             ToShimWit (PinaforeShim Type) (PinaforeSingularType 'Positive) (PinaforeAction a) where
    toShimWit =
        unPosShimWit toJMShimWit $ \ta conva ->
            mapPosShimWit (cfmap conva) $
            mkShimWit $ GroundDolanSingularType ActionPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) a) =>
             ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) (PinaforeAction a) where
    toShimWit = singleDolanShimWit toJMShimWit

instance (FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) a) =>
             FromShimWit (PinaforeShim Type) (PinaforeSingularType 'Negative) (PinaforeAction a) where
    fromShimWit =
        unNegShimWit fromJMShimWit $ \ta conva ->
            mapNegShimWit (cfmap conva) $
            mkShimWit $ GroundDolanSingularType ActionPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) a) =>
             FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) (PinaforeAction a) where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- IO
instance (ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) a) =>
             ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) (IO a) where
    toShimWit = mapPosShimWit (toEnhanced "subtype" (liftIO :: IO a -> PinaforeAction a)) toJMShimWit

-- View
instance (ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) a) =>
             ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) (View a) where
    toShimWit = mapPosShimWit (toEnhanced "subtype" viewPinaforeAction) toJMShimWit

-- LangOrder
instance (FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) a) =>
             ToShimWit (PinaforeShim Type) (PinaforeSingularType 'Positive) (LangOrder a) where
    toShimWit =
        unNegShimWit fromJMShimWit $ \ta conva ->
            mapPosShimWit (applyPolyShim ContravarianceType cid $ MkCatDual conva) $
            mkShimWit $ GroundDolanSingularType OrderPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) a) =>
             ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) (LangOrder a) where
    toShimWit = singleDolanShimWit toJMShimWit

instance (ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) a) =>
             FromShimWit (PinaforeShim Type) (PinaforeSingularType 'Negative) (LangOrder a) where
    fromShimWit =
        unPosShimWit toJMShimWit $ \ta conva ->
            mapNegShimWit (applyPolyShim ContravarianceType cid $ MkCatDual conva) $
            mkShimWit $ GroundDolanSingularType OrderPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) a) =>
             FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) (LangOrder a) where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- LangNotifier
instance (FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) a) =>
             ToShimWit (PinaforeShim Type) (PinaforeSingularType 'Positive) (LangNotifier a) where
    toShimWit =
        unNegShimWit fromJMShimWit $ \ta conva ->
            mapPosShimWit (applyPolyShim ContravarianceType cid $ MkCatDual conva) $
            mkShimWit $ GroundDolanSingularType NotifierPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) a) =>
             ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) (LangNotifier a) where
    toShimWit = singleDolanShimWit toJMShimWit

instance (ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) a) =>
             FromShimWit (PinaforeShim Type) (PinaforeSingularType 'Negative) (LangNotifier a) where
    fromShimWit =
        unPosShimWit toJMShimWit $ \ta conva ->
            mapNegShimWit (applyPolyShim ContravarianceType cid $ MkCatDual conva) $
            mkShimWit $ GroundDolanSingularType NotifierPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) a) =>
             FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) (LangNotifier a) where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- LangUI
instance ToShimWit (PinaforeShim Type) (PinaforeSingularType 'Positive) LangUI where
    toShimWit = mkShimWit $ GroundDolanSingularType UserInterfacePinaforeGroundType NilDolanArguments

instance ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) LangUI where
    toShimWit = singleDolanShimWit toJMShimWit

instance FromShimWit (PinaforeShim Type) (PinaforeSingularType 'Negative) LangUI where
    fromShimWit = mkShimWit $ GroundDolanSingularType UserInterfacePinaforeGroundType NilDolanArguments

instance FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) LangUI where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- PinaforeWindow
instance ToShimWit (PinaforeShim Type) (PinaforeSingularType 'Positive) PinaforeWindow where
    toShimWit = mkShimWit $ GroundDolanSingularType WindowPinaforeGroundType NilDolanArguments

instance ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) PinaforeWindow where
    toShimWit = singleDolanShimWit toJMShimWit

instance FromShimWit (PinaforeShim Type) (PinaforeSingularType 'Negative) PinaforeWindow where
    fromShimWit = mkShimWit $ GroundDolanSingularType WindowPinaforeGroundType NilDolanArguments

instance FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) PinaforeWindow where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- MenuEntry
instance ToShimWit (PinaforeShim Type) (PinaforeSingularType 'Positive) MenuEntry where
    toShimWit = mkShimWit $ GroundDolanSingularType MenuItemPinaforeGroundType NilDolanArguments

instance ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) MenuEntry where
    toShimWit = singleDolanShimWit toJMShimWit

instance FromShimWit (PinaforeShim Type) (PinaforeSingularType 'Negative) MenuEntry where
    fromShimWit = mkShimWit $ GroundDolanSingularType MenuItemPinaforeGroundType NilDolanArguments

instance FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) MenuEntry where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- LangRef
instance ( FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) p
         , ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) q
         ) => ToShimWit (PinaforeShim Type) (PinaforeSingularType 'Positive) (LangRef '( p, q)) where
    toShimWit =
        unToRangeShimWit $ \tpq conv ->
            mapPosShimWit (applyPolyShim RangevarianceType cid conv) $
            mkShimWit $ GroundDolanSingularType RefPinaforeGroundType $ ConsDolanArguments tpq NilDolanArguments

instance ( FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) p
         , ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) q
         ) => ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) (LangRef '( p, q)) where
    toShimWit = singleDolanShimWit toJMShimWit

instance ( ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) p
         , FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) q
         ) => FromShimWit (PinaforeShim Type) (PinaforeSingularType 'Negative) (LangRef '( p, q)) where
    fromShimWit =
        unFromRangeShimWit $ \tpq conv ->
            mapNegShimWit (applyPolyShim RangevarianceType cid conv) $
            mkShimWit $ GroundDolanSingularType RefPinaforeGroundType $ ConsDolanArguments tpq NilDolanArguments

instance ( ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) p
         , FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) q
         ) => FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) (LangRef '( p, q)) where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- PinaforeRef
instance ( FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) t
         , ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) t
         ) => ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) (PinaforeRef (WholeUpdate (Know t))) where
    toShimWit = mapPosShimWit (toEnhanced "subtype" pinaforeRefToRef) toJMShimWit

instance ( FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) t
         , ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) t
         ) => FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) (PinaforeRef (WholeUpdate (Know t))) where
    fromShimWit = mapNegShimWit (toEnhanced "subtype" langRefToValue) fromJMShimWit

-- PinaforeImmutableRef
instance (FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) a) =>
             FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) (PinaforeImmutableRef a) where
    fromShimWit = mapNegShimWit (toEnhanced "subtype" langRefToImmutable) fromJMShimWit

instance (ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) a) =>
             ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) (PinaforeImmutableRef a) where
    toShimWit = mapPosShimWit (toEnhanced "subtype" pinaforeImmutableToRef) toJMShimWit

-- PinaforeROWRef
instance (FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) t) =>
             FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) (PinaforeROWRef (Know t)) where
    fromShimWit = mapNegShimWit (toEnhanced "subtype" langRefToReadOnlyValue) fromJMShimWit

instance (ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) t) =>
             ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) (PinaforeROWRef (Know t)) where
    toShimWit = mapPosShimWit (toEnhanced "subtype" pinaforeROWRefToRef) toJMShimWit

-- LangSetRef
instance (FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) a) =>
             ToShimWit (PinaforeShim Type) (PinaforeSingularType 'Positive) (LangSetRef a) where
    toShimWit =
        unNegShimWit fromJMShimWit $ \ta conva ->
            mapPosShimWit (applyPolyShim ContravarianceType cid $ MkCatDual conva) $
            mkShimWit $ GroundDolanSingularType SetRefPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) a) =>
             ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) (LangSetRef a) where
    toShimWit = singleDolanShimWit toJMShimWit

instance (ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) a) =>
             FromShimWit (PinaforeShim Type) (PinaforeSingularType 'Negative) (LangSetRef a) where
    fromShimWit =
        unPosShimWit toJMShimWit $ \ta conva ->
            mapNegShimWit (applyPolyShim ContravarianceType cid $ MkCatDual conva) $
            mkShimWit $ GroundDolanSingularType SetRefPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) a) =>
             FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) (LangSetRef a) where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- LangFiniteSetRef
instance ( FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) p
         , ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) q
         ) => ToShimWit (PinaforeShim Type) (PinaforeSingularType 'Positive) (LangFiniteSetRef '( p, q)) where
    toShimWit =
        unToRangeShimWit $ \tpq conv ->
            mapPosShimWit (applyPolyShim RangevarianceType cid conv) $
            mkShimWit $
            GroundDolanSingularType FiniteSetRefPinaforeGroundType $ ConsDolanArguments tpq NilDolanArguments

instance ( FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) p
         , ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) q
         ) => ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) (LangFiniteSetRef '( p, q)) where
    toShimWit = singleDolanShimWit toJMShimWit

instance ( ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) p
         , FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) q
         ) => FromShimWit (PinaforeShim Type) (PinaforeSingularType 'Negative) (LangFiniteSetRef '( p, q)) where
    fromShimWit =
        unFromRangeShimWit $ \tpq conv ->
            mapNegShimWit (applyPolyShim RangevarianceType cid conv) $
            mkShimWit $
            GroundDolanSingularType FiniteSetRefPinaforeGroundType $ ConsDolanArguments tpq NilDolanArguments

instance ( ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) p
         , FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) q
         ) => FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) (LangFiniteSetRef '( p, q)) where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- PinaforeRef FiniteSetUpdate
instance ( ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) t
         , FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) t
         ) => FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) (PinaforeRef (FiniteSetUpdate t)) where
    fromShimWit = mapNegShimWit (toEnhanced "subtype" unLangFiniteSetRef) fromJMShimWit

instance ( Eq t
         , ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) t
         , FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) t
         ) => ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) (PinaforeRef (FiniteSetUpdate t)) where
    toShimWit = mapPosShimWit (toEnhanced "subtype" $ MkLangFiniteSetRef identityRange) toJMShimWit

-- LangMorphism
instance ( FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) pa
         , ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) qa
         , FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) pb
         , ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) qb
         ) => ToShimWit (PinaforeShim Type) (PinaforeSingularType 'Positive) (LangMorphism '( pa, qa) '( pb, qb)) where
    toShimWit =
        unToRangeShimWit @_ @_ @pa @qa $ \ta conva ->
            unToRangeShimWit @_ @_ @pb @qb $ \tb convb ->
                mapPosShimWit (applyPolyShim RangevarianceType (applyPolyShim RangevarianceType cid conva) convb) $
                mkShimWit $
                GroundDolanSingularType MorphismPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) pa
         , ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) qa
         , FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) pb
         , ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) qb
         ) => ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) (LangMorphism '( pa, qa) '( pb, qb)) where
    toShimWit = singleDolanShimWit toJMShimWit

instance ( ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) pa
         , FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) qa
         , ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) pb
         , FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) qb
         ) => FromShimWit (PinaforeShim Type) (PinaforeSingularType 'Negative) (LangMorphism '( pa, qa) '( pb, qb)) where
    fromShimWit =
        unFromRangeShimWit $ \ta conva ->
            unFromRangeShimWit $ \tb convb ->
                mapNegShimWit (applyPolyShim RangevarianceType (applyPolyShim RangevarianceType cid conva) convb) $
                mkShimWit $
                GroundDolanSingularType MorphismPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) pa
         , FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) qa
         , ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) pb
         , FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) qb
         ) => FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) (LangMorphism '( pa, qa) '( pb, qb)) where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- Entity
instance ToShimWit (PinaforeShim Type) (PinaforeSingularType 'Positive) Entity where
    toShimWit =
        mkShimWit $ GroundDolanSingularType (EntityPinaforeGroundType NilListType TopEntityGroundType) NilDolanArguments

instance ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) Entity where
    toShimWit = singleDolanShimWit toJMShimWit

instance FromShimWit (PinaforeShim Type) (PinaforeSingularType 'Negative) Entity where
    fromShimWit =
        mkShimWit $ GroundDolanSingularType (EntityPinaforeGroundType NilListType TopEntityGroundType) NilDolanArguments

instance FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) Entity where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- NewEntity
instance ToShimWit (PinaforeShim Type) (PinaforeSingularType 'Positive) NewEntity where
    toShimWit =
        mkShimWit $ GroundDolanSingularType (EntityPinaforeGroundType NilListType NewEntityGroundType) NilDolanArguments

instance ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) NewEntity where
    toShimWit = singleDolanShimWit toJMShimWit

instance FromShimWit (PinaforeShim Type) (PinaforeSingularType 'Negative) NewEntity where
    fromShimWit =
        mkShimWit $ GroundDolanSingularType (EntityPinaforeGroundType NilListType NewEntityGroundType) NilDolanArguments

instance FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) NewEntity where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- Literal
literalInstances :: _ -> _
literalInstances t =
    [d|
  
  instance ToShimWit (PinaforeShim Type)
             (PinaforeSingularType 'Positive)
             $( t )
           where
          toShimWit
            = mkShimWit $
                GroundDolanSingularType
                  (EntityPinaforeGroundType NilListType $
                     LiteralEntityGroundType representative)
                  NilDolanArguments
  
  instance ToShimWit (PinaforeShim Type) (PinaforeType 'Positive)
             $( t )
           where
          toShimWit = singleDolanShimWit toJMShimWit
  
  instance FromShimWit (PinaforeShim Type)
             (PinaforeSingularType 'Negative)
             $( t )
           where
          fromShimWit
            = mkShimWit $
                GroundDolanSingularType
                  (EntityPinaforeGroundType NilListType $
                     LiteralEntityGroundType representative)
                  NilDolanArguments
  
  instance FromShimWit (PinaforeShim Type) (PinaforeType 'Negative)
             $( t )
           where
          fromShimWit = singleDolanShimWit fromJMShimWit
  |]
