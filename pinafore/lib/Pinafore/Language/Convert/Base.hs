{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Convert.Base
    ( qPositiveTypeDescription
    , qNegativeTypeDescription
    , jmToValue
    , ToPinaforeType
    , FromPinaforeType
    , literalInstances
    ) where

import Changes.Core
import Pinafore.Base
import Pinafore.Language.Shim
import Pinafore.Language.Type
import Pinafore.Language.Value
import Pinafore.Language.Var
import Shapes

qPositiveTypeDescription ::
       forall t. ToPinaforeType t
    => Text
qPositiveTypeDescription =
    case toShimWit @Type @(PinaforePolyShim Type) @(PinaforeType 'Positive) @t of
        MkShimWit w _ -> exprShow w

qNegativeTypeDescription ::
       forall t. FromPinaforeType t
    => Text
qNegativeTypeDescription =
    case fromShimWit @Type @(PinaforePolyShim Type) @(PinaforeType 'Negative) @t of
        MkShimWit w _ -> exprShow w

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
                mapPosShimWit (applyPolyShim CovarianceType (cfmap conva) convb) $
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
                mapNegShimWit (applyPolyShim CovarianceType (cfmap conva) convb) $
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
                mapPosShimWit (applyPolyShim CovarianceType (cfmap conva) convb) $
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
                mapNegShimWit (applyPolyShim CovarianceType (cfmap conva) convb) $
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
                mapPosShimWit (applyPolyShim CovarianceType (ccontramap conva) convb) $
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
                mapNegShimWit (applyPolyShim CovarianceType (ccontramap conva) convb) $
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

-- IO
instance (ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a) =>
             ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (IO a) where
    toShimWit = mapPosShimWit (functionToShim "subtype" (liftIO :: IO a -> PinaforeAction a)) toJMShimWit

-- View
instance (ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a) =>
             ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (View a) where
    toShimWit = mapPosShimWit (functionToShim "subtype" viewPinaforeAction) toJMShimWit

-- LangOrder
instance (FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a) =>
             ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) (LangOrder a) where
    toShimWit =
        unNegShimWit fromJMShimWit $ \ta conva ->
            mapPosShimWit (applyPolyShim ContravarianceType cid $ MkCatDual conva) $
            mkShimWit $ GroundDolanSingularType OrderPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a) =>
             ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (LangOrder a) where
    toShimWit = singleDolanShimWit toJMShimWit

instance (ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a) =>
             FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) (LangOrder a) where
    fromShimWit =
        unPosShimWit toJMShimWit $ \ta conva ->
            mapNegShimWit (applyPolyShim ContravarianceType cid $ MkCatDual conva) $
            mkShimWit $ GroundDolanSingularType OrderPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a) =>
             FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (LangOrder a) where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- LangUI
instance ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) LangUI where
    toShimWit = mkShimWit $ GroundDolanSingularType UserInterfacePinaforeGroundType NilDolanArguments

instance ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) LangUI where
    toShimWit = singleDolanShimWit toJMShimWit

instance FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) LangUI where
    fromShimWit = mkShimWit $ GroundDolanSingularType UserInterfacePinaforeGroundType NilDolanArguments

instance FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) LangUI where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- LangWindow
instance ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) LangWindow where
    toShimWit = mkShimWit $ GroundDolanSingularType WindowPinaforeGroundType NilDolanArguments

instance ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) LangWindow where
    toShimWit = singleDolanShimWit toJMShimWit

instance FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) LangWindow where
    fromShimWit = mkShimWit $ GroundDolanSingularType WindowPinaforeGroundType NilDolanArguments

instance FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) LangWindow where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- LangMenuEntry
instance ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) LangMenuEntry where
    toShimWit = mkShimWit $ GroundDolanSingularType MenuItemPinaforeGroundType NilDolanArguments

instance ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) LangMenuEntry where
    toShimWit = singleDolanShimWit toJMShimWit

instance FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) LangMenuEntry where
    fromShimWit = mkShimWit $ GroundDolanSingularType MenuItemPinaforeGroundType NilDolanArguments

instance FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) LangMenuEntry where
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

-- WModel
instance ( FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) t
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) t
         ) => ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (WModel (WholeUpdate (Know t))) where
    toShimWit = mapPosShimWit (functionToShim "subtype" pinaforeRefToWholeRef) toJMShimWit

instance ( FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) t
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) t
         ) => FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (WModel (WholeUpdate (Know t))) where
    fromShimWit = mapNegShimWit (functionToShim "subtype" langWholeRefToValue) fromJMShimWit

-- PinaforeImmutableWholeRef
instance (FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a) =>
             FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (PinaforeImmutableWholeRef a) where
    fromShimWit = mapNegShimWit (functionToShim "subtype" langWholeRefToImmutable) fromJMShimWit

instance (ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a) =>
             ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (PinaforeImmutableWholeRef a) where
    toShimWit = mapPosShimWit (functionToShim "subtype" pinaforeImmutableToWholeRef) toJMShimWit

-- PinaforeROWRef
instance (FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) t) =>
             FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (PinaforeROWRef (Know t)) where
    fromShimWit = mapNegShimWit (functionToShim "subtype" langWholeRefToReadOnlyValue) fromJMShimWit

instance (ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) t) =>
             ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (PinaforeROWRef (Know t)) where
    toShimWit = mapPosShimWit (functionToShim "subtype" pinaforeROWRefToWholeRef) toJMShimWit

-- LangSetRef
instance (FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a) =>
             ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) (LangSetRef a) where
    toShimWit =
        unNegShimWit fromJMShimWit $ \ta conva ->
            mapPosShimWit (applyPolyShim ContravarianceType cid $ MkCatDual conva) $
            mkShimWit $ GroundDolanSingularType SetRefPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a) =>
             ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (LangSetRef a) where
    toShimWit = singleDolanShimWit toJMShimWit

instance (ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a) =>
             FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) (LangSetRef a) where
    fromShimWit =
        unPosShimWit toJMShimWit $ \ta conva ->
            mapNegShimWit (applyPolyShim ContravarianceType cid $ MkCatDual conva) $
            mkShimWit $ GroundDolanSingularType SetRefPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a) =>
             FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (LangSetRef a) where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- LangFiniteSetRef
instance ( FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) p
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) q
         ) => ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) (LangFiniteSetRef '( p, q)) where
    toShimWit =
        unToRangeShimWit $ \tpq conv ->
            mapPosShimWit (applyPolyShim RangevarianceType cid conv) $
            mkShimWit $
            GroundDolanSingularType FiniteSetRefPinaforeGroundType $ ConsDolanArguments tpq NilDolanArguments

instance ( FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) p
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) q
         ) => ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (LangFiniteSetRef '( p, q)) where
    toShimWit = singleDolanShimWit toJMShimWit

instance ( ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) p
         , FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) q
         ) => FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) (LangFiniteSetRef '( p, q)) where
    fromShimWit =
        unFromRangeShimWit $ \tpq conv ->
            mapNegShimWit (applyPolyShim RangevarianceType cid conv) $
            mkShimWit $
            GroundDolanSingularType FiniteSetRefPinaforeGroundType $ ConsDolanArguments tpq NilDolanArguments

instance ( ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) p
         , FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) q
         ) => FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (LangFiniteSetRef '( p, q)) where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- WModel FiniteSetUpdate
instance ( ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) t
         , FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) t
         ) => FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (WModel (FiniteSetUpdate t)) where
    fromShimWit = mapNegShimWit (functionToShim "subtype" unLangFiniteSetRef) fromJMShimWit

instance ( Eq t
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) t
         , FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) t
         ) => ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (WModel (FiniteSetUpdate t)) where
    toShimWit = mapPosShimWit (functionToShim "subtype" $ MkLangFiniteSetRef identityRange) toJMShimWit

-- LangMorphism
instance ( FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) pa
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) qa
         , FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) pb
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) qb
         ) => ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) (LangMorphism '( pa, qa) '( pb, qb)) where
    toShimWit =
        unToRangeShimWit @_ @_ @pa @qa $ \ta conva ->
            unToRangeShimWit @_ @_ @pb @qb $ \tb convb ->
                mapPosShimWit (applyPolyShim RangevarianceType (applyPolyShim RangevarianceType cid conva) convb) $
                mkShimWit $
                GroundDolanSingularType MorphismPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) pa
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) qa
         , FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) pb
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) qb
         ) => ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (LangMorphism '( pa, qa) '( pb, qb)) where
    toShimWit = singleDolanShimWit toJMShimWit

instance ( ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) pa
         , FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) qa
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) pb
         , FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) qb
         ) => FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) (LangMorphism '( pa, qa) '( pb, qb)) where
    fromShimWit =
        unFromRangeShimWit $ \ta conva ->
            unFromRangeShimWit $ \tb convb ->
                mapNegShimWit (applyPolyShim RangevarianceType (applyPolyShim RangevarianceType cid conva) convb) $
                mkShimWit $
                GroundDolanSingularType MorphismPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) pa
         , FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) qa
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) pb
         , FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) qb
         ) => FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (LangMorphism '( pa, qa) '( pb, qb)) where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- Entity
instance ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) Entity where
    toShimWit =
        mkShimWit $ GroundDolanSingularType (EntityPinaforeGroundType NilListType TopEntityGroundType) NilDolanArguments

instance ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) Entity where
    toShimWit = singleDolanShimWit toJMShimWit

instance FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) Entity where
    fromShimWit =
        mkShimWit $ GroundDolanSingularType (EntityPinaforeGroundType NilListType TopEntityGroundType) NilDolanArguments

instance FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) Entity where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- Literal
literalInstances :: _ -> _
literalInstances t =
    [d|
  
  instance ToShimWit (PinaforePolyShim Type)
             (PinaforeSingularType 'Positive)
             $( t )
           where
          toShimWit
            = mkShimWit $
                GroundDolanSingularType
                  (EntityPinaforeGroundType NilListType $
                     LiteralEntityGroundType representative)
                  NilDolanArguments
  
  instance ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive)
             $( t )
           where
          toShimWit = singleDolanShimWit toJMShimWit
  
  instance FromShimWit (PinaforePolyShim Type)
             (PinaforeSingularType 'Negative)
             $( t )
           where
          fromShimWit
            = mkShimWit $
                GroundDolanSingularType
                  (EntityPinaforeGroundType NilListType $
                     LiteralEntityGroundType representative)
                  NilDolanArguments
  
  instance FromShimWit (PinaforePolyShim Type)
             (PinaforeType 'Negative)
             $( t )
           where
          fromShimWit = singleDolanShimWit fromJMShimWit
  |]
