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
import Pinafore.Language.TypeSystem
import Pinafore.Language.Value
import Shapes
import Truth.Core

qPositiveTypeDescription ::
       forall baseupdate t. ToPinaforeType baseupdate t
    => Text
qPositiveTypeDescription =
    case toJMShimWit @(PinaforeType baseupdate 'Positive) @t of
        MkShimWit w _ -> pack $ show w

qNegativeTypeDescription ::
       forall baseupdate t. FromPinaforeType baseupdate t
    => Text
qNegativeTypeDescription =
    case fromJMShimWit @(PinaforeType baseupdate 'Negative) @t of
        MkShimWit w _ -> pack $ show w

type ToPinaforeType baseupdate = ToShimWit JMShim (PinaforeType baseupdate 'Positive)

type FromPinaforeType baseupdate = FromShimWit JMShim (PinaforeType baseupdate 'Negative)

-- top, bottom, join, meet
instance ToShimWit JMShim (PinaforeType baseupdate 'Positive) BottomType where
    toShimWit = mkPJMShimWit NilPinaforeType

instance FromShimWit JMShim (PinaforeType baseupdate 'Negative) TopType where
    fromShimWit = mkPJMShimWit NilPinaforeType

instance ( ToShimWit JMShim (PinaforeType baseupdate 'Positive) a
         , ToShimWit JMShim (PinaforeType baseupdate 'Positive) b
         ) => ToShimWit JMShim (PinaforeType baseupdate 'Positive) (JoinType a b) where
    toShimWit = joinPinaforeShimWit toJMShimWit toJMShimWit

instance ( FromShimWit JMShim (PinaforeType baseupdate 'Negative) a
         , FromShimWit JMShim (PinaforeType baseupdate 'Negative) b
         ) => FromShimWit JMShim (PinaforeType baseupdate 'Negative) (MeetType a b) where
    fromShimWit = meetPinaforeShimWit fromJMShimWit fromJMShimWit

-- UVar
instance KnownSymbol name => ToShimWit JMShim (PinaforeSingularType baseupdate 'Positive) (UVar name) where
    toShimWit = mkPJMShimWit $ VarPinaforeSingularType MkSymbolType

instance KnownSymbol name => ToShimWit JMShim (PinaforeType baseupdate 'Positive) (UVar name) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance KnownSymbol name => FromShimWit JMShim (PinaforeSingularType baseupdate 'Negative) (UVar name) where
    fromShimWit = mkPJMShimWit $ VarPinaforeSingularType MkSymbolType

instance KnownSymbol name => FromShimWit JMShim (PinaforeType baseupdate 'Negative) (UVar name) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- (,)
instance ( ToShimWit JMShim (PinaforeType baseupdate 'Positive) a
         , ToShimWit JMShim (PinaforeType baseupdate 'Positive) b
         ) => ToShimWit JMShim (PinaforeSingularType baseupdate 'Positive) (a, b) where
    toShimWit =
        unShimWit toJMShimWit $ \ta conva ->
            unShimWit toJMShimWit $ \tb convb ->
                mapShimWit (consShimFunc CovarianceType (cfmap conva) convb) $
                mkPJMShimWit $
                GroundPinaforeSingularType
                    (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) PairEntityGroundType) $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( ToShimWit JMShim (PinaforeType baseupdate 'Positive) a
         , ToShimWit JMShim (PinaforeType baseupdate 'Positive) b
         ) => ToShimWit JMShim (PinaforeType baseupdate 'Positive) (a, b) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance ( FromShimWit JMShim (PinaforeType baseupdate 'Negative) a
         , FromShimWit JMShim (PinaforeType baseupdate 'Negative) b
         ) => FromShimWit JMShim (PinaforeSingularType baseupdate 'Negative) (a, b) where
    fromShimWit =
        unShimWit fromJMShimWit $ \ta conva ->
            unShimWit fromJMShimWit $ \tb convb ->
                mapShimWit (consShimFunc CovarianceType (cfmap conva) convb) $
                mkPJMShimWit $
                GroundPinaforeSingularType
                    (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) PairEntityGroundType) $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( FromShimWit JMShim (PinaforeType baseupdate 'Negative) a
         , FromShimWit JMShim (PinaforeType baseupdate 'Negative) b
         ) => FromShimWit JMShim (PinaforeType baseupdate 'Negative) (Either a b) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- Either
instance ( ToShimWit JMShim (PinaforeType baseupdate 'Positive) a
         , ToShimWit JMShim (PinaforeType baseupdate 'Positive) b
         ) => ToShimWit JMShim (PinaforeSingularType baseupdate 'Positive) (Either a b) where
    toShimWit =
        unShimWit toJMShimWit $ \ta conva ->
            unShimWit toJMShimWit $ \tb convb ->
                mapShimWit (consShimFunc CovarianceType (cfmap conva) convb) $
                mkPJMShimWit $
                GroundPinaforeSingularType
                    (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) EitherEntityGroundType) $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( ToShimWit JMShim (PinaforeType baseupdate 'Positive) a
         , ToShimWit JMShim (PinaforeType baseupdate 'Positive) b
         ) => ToShimWit JMShim (PinaforeType baseupdate 'Positive) (Either a b) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance ( FromShimWit JMShim (PinaforeType baseupdate 'Negative) a
         , FromShimWit JMShim (PinaforeType baseupdate 'Negative) b
         ) => FromShimWit JMShim (PinaforeSingularType baseupdate 'Negative) (Either a b) where
    fromShimWit =
        unShimWit fromJMShimWit $ \ta conva ->
            unShimWit fromJMShimWit $ \tb convb ->
                mapShimWit (consShimFunc CovarianceType (cfmap conva) convb) $
                mkPJMShimWit $
                GroundPinaforeSingularType
                    (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) EitherEntityGroundType) $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( FromShimWit JMShim (PinaforeType baseupdate 'Negative) a
         , FromShimWit JMShim (PinaforeType baseupdate 'Negative) b
         ) => FromShimWit JMShim (PinaforeType baseupdate 'Negative) (a, b) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- (->)
instance ( FromShimWit JMShim (PinaforeType baseupdate 'Negative) a
         , ToShimWit JMShim (PinaforeType baseupdate 'Positive) b
         ) => ToShimWit JMShim (PinaforeSingularType baseupdate 'Positive) (a -> b) where
    toShimWit =
        unShimWit fromJMShimWit $ \ta conva ->
            unShimWit toJMShimWit $ \tb convb ->
                mapShimWit (consShimFunc CovarianceType (ccontramap conva) convb) $
                mkPJMShimWit $
                GroundPinaforeSingularType FuncPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( FromShimWit JMShim (PinaforeType baseupdate 'Negative) a
         , ToShimWit JMShim (PinaforeType baseupdate 'Positive) b
         ) => ToShimWit JMShim (PinaforeType baseupdate 'Positive) (a -> b) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance ( ToShimWit JMShim (PinaforeType baseupdate 'Positive) a
         , FromShimWit JMShim (PinaforeType baseupdate 'Negative) b
         ) => FromShimWit JMShim (PinaforeSingularType baseupdate 'Negative) (a -> b) where
    fromShimWit =
        unShimWit toJMShimWit $ \ta conva ->
            unShimWit fromJMShimWit $ \tb convb ->
                mapShimWit (consShimFunc CovarianceType (ccontramap conva) convb) $
                mkPJMShimWit $
                GroundPinaforeSingularType FuncPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( ToShimWit JMShim (PinaforeType baseupdate 'Positive) a
         , FromShimWit JMShim (PinaforeType baseupdate 'Negative) b
         ) => FromShimWit JMShim (PinaforeType baseupdate 'Negative) (a -> b) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- Maybe
instance (ToShimWit JMShim (PinaforeType baseupdate 'Positive) a) =>
             ToShimWit JMShim (PinaforeSingularType baseupdate 'Positive) (Maybe a) where
    toShimWit =
        unShimWit toJMShimWit $ \ta conva ->
            mapShimWit (cfmap conva) $
            mkPJMShimWit $
            GroundPinaforeSingularType (EntityPinaforeGroundType (ConsListType Refl NilListType) MaybeEntityGroundType) $
            ConsDolanArguments ta NilDolanArguments

instance (ToShimWit JMShim (PinaforeType baseupdate 'Positive) a) =>
             ToShimWit JMShim (PinaforeType baseupdate 'Positive) (Maybe a) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance (FromShimWit JMShim (PinaforeType baseupdate 'Negative) a) =>
             FromShimWit JMShim (PinaforeSingularType baseupdate 'Negative) (Maybe a) where
    fromShimWit =
        unShimWit fromJMShimWit $ \ta conva ->
            mapShimWit (cfmap conva) $
            mkPJMShimWit $
            GroundPinaforeSingularType (EntityPinaforeGroundType (ConsListType Refl NilListType) MaybeEntityGroundType) $
            ConsDolanArguments ta NilDolanArguments

instance (FromShimWit JMShim (PinaforeType baseupdate 'Negative) a) =>
             FromShimWit JMShim (PinaforeType baseupdate 'Negative) (Maybe a) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- []
instance (ToShimWit JMShim (PinaforeType baseupdate 'Positive) a) =>
             ToShimWit JMShim (PinaforeSingularType baseupdate 'Positive) [a] where
    toShimWit =
        unShimWit toJMShimWit $ \ta conva ->
            mapShimWit (cfmap conva) $
            mkPJMShimWit $
            GroundPinaforeSingularType (EntityPinaforeGroundType (ConsListType Refl NilListType) ListEntityGroundType) $
            ConsDolanArguments ta NilDolanArguments

instance (ToShimWit JMShim (PinaforeType baseupdate 'Positive) a) =>
             ToShimWit JMShim (PinaforeType baseupdate 'Positive) [a] where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance (FromShimWit JMShim (PinaforeType baseupdate 'Negative) a) =>
             FromShimWit JMShim (PinaforeSingularType baseupdate 'Negative) [a] where
    fromShimWit =
        unShimWit fromJMShimWit $ \ta conva ->
            mapShimWit (cfmap conva) $
            mkPJMShimWit $
            GroundPinaforeSingularType (EntityPinaforeGroundType (ConsListType Refl NilListType) ListEntityGroundType) $
            ConsDolanArguments ta NilDolanArguments

instance (FromShimWit JMShim (PinaforeType baseupdate 'Negative) a) =>
             FromShimWit JMShim (PinaforeType baseupdate 'Negative) [a] where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- PinaforeAction
instance (ToShimWit JMShim (PinaforeType baseupdate 'Positive) a) =>
             ToShimWit JMShim (PinaforeSingularType baseupdate 'Positive) (PinaforeAction a) where
    toShimWit =
        unShimWit toJMShimWit $ \ta conva ->
            mapShimWit (cfmap conva) $
            mkPJMShimWit $ GroundPinaforeSingularType ActionPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (ToShimWit JMShim (PinaforeType baseupdate 'Positive) a) =>
             ToShimWit JMShim (PinaforeType baseupdate 'Positive) (PinaforeAction a) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance (FromShimWit JMShim (PinaforeType baseupdate 'Negative) a) =>
             FromShimWit JMShim (PinaforeSingularType baseupdate 'Negative) (PinaforeAction a) where
    fromShimWit =
        unShimWit fromJMShimWit $ \ta conva ->
            mapShimWit (cfmap conva) $
            mkPJMShimWit $ GroundPinaforeSingularType ActionPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (FromShimWit JMShim (PinaforeType baseupdate 'Negative) a) =>
             FromShimWit JMShim (PinaforeType baseupdate 'Negative) (PinaforeAction a) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- IO
instance (ToShimWit JMShim (PinaforeType baseupdate 'Positive) a) =>
             ToShimWit JMShim (PinaforeType baseupdate 'Positive) (IO a) where
    toShimWit = mapShimWit (toEnhanced "subtype" (liftIO :: IO a -> PinaforeAction a)) toJMShimWit

-- View
instance (ToShimWit JMShim (PinaforeType baseupdate 'Positive) a) =>
             ToShimWit JMShim (PinaforeType baseupdate 'Positive) (View a) where
    toShimWit = mapShimWit (toEnhanced "subtype" viewPinaforeAction) toJMShimWit

-- LangOrder
instance (FromShimWit JMShim (PinaforeType baseupdate 'Negative) a) =>
             ToShimWit JMShim (PinaforeSingularType baseupdate 'Positive) (LangOrder a) where
    toShimWit =
        unShimWit fromJMShimWit $ \ta conva ->
            mapShimWit (consShimFunc ContravarianceType cid $ MkCatDual conva) $
            mkPJMShimWit $ GroundPinaforeSingularType OrderPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (FromShimWit JMShim (PinaforeType baseupdate 'Negative) a) =>
             ToShimWit JMShim (PinaforeType baseupdate 'Positive) (LangOrder a) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance (ToShimWit JMShim (PinaforeType baseupdate 'Positive) a) =>
             FromShimWit JMShim (PinaforeSingularType baseupdate 'Negative) (LangOrder a) where
    fromShimWit =
        unShimWit toJMShimWit $ \ta conva ->
            mapShimWit (consShimFunc ContravarianceType cid $ MkCatDual conva) $
            mkPJMShimWit $ GroundPinaforeSingularType OrderPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (ToShimWit JMShim (PinaforeType baseupdate 'Positive) a) =>
             FromShimWit JMShim (PinaforeType baseupdate 'Negative) (LangOrder a) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- LangNotifier
instance (FromShimWit JMShim (PinaforeType baseupdate 'Negative) a) =>
             ToShimWit JMShim (PinaforeSingularType baseupdate 'Positive) (LangNotifier a) where
    toShimWit =
        unShimWit fromJMShimWit $ \ta conva ->
            mapShimWit (consShimFunc ContravarianceType cid $ MkCatDual conva) $
            mkPJMShimWit $
            GroundPinaforeSingularType NotifierPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (FromShimWit JMShim (PinaforeType baseupdate 'Negative) a) =>
             ToShimWit JMShim (PinaforeType baseupdate 'Positive) (LangNotifier a) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance (ToShimWit JMShim (PinaforeType baseupdate 'Positive) a) =>
             FromShimWit JMShim (PinaforeSingularType baseupdate 'Negative) (LangNotifier a) where
    fromShimWit =
        unShimWit toJMShimWit $ \ta conva ->
            mapShimWit (consShimFunc ContravarianceType cid $ MkCatDual conva) $
            mkPJMShimWit $
            GroundPinaforeSingularType NotifierPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (ToShimWit JMShim (PinaforeType baseupdate 'Positive) a) =>
             FromShimWit JMShim (PinaforeType baseupdate 'Negative) (LangNotifier a) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- LangUI
instance ToShimWit JMShim (PinaforeSingularType baseupdate 'Positive) LangUI where
    toShimWit = mkPJMShimWit $ GroundPinaforeSingularType UserInterfacePinaforeGroundType NilDolanArguments

instance ToShimWit JMShim (PinaforeType baseupdate 'Positive) LangUI where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance FromShimWit JMShim (PinaforeSingularType baseupdate 'Negative) LangUI where
    fromShimWit = mkPJMShimWit $ GroundPinaforeSingularType UserInterfacePinaforeGroundType NilDolanArguments

instance FromShimWit JMShim (PinaforeType baseupdate 'Negative) LangUI where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- PinaforeWindow
instance ToShimWit JMShim (PinaforeSingularType baseupdate 'Positive) PinaforeWindow where
    toShimWit = mkPJMShimWit $ GroundPinaforeSingularType WindowPinaforeGroundType NilDolanArguments

instance ToShimWit JMShim (PinaforeType baseupdate 'Positive) PinaforeWindow where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance FromShimWit JMShim (PinaforeSingularType baseupdate 'Negative) PinaforeWindow where
    fromShimWit = mkPJMShimWit $ GroundPinaforeSingularType WindowPinaforeGroundType NilDolanArguments

instance FromShimWit JMShim (PinaforeType baseupdate 'Negative) PinaforeWindow where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- MenuEntry
instance ToShimWit JMShim (PinaforeSingularType baseupdate 'Positive) MenuEntry where
    toShimWit = mkPJMShimWit $ GroundPinaforeSingularType MenuItemPinaforeGroundType NilDolanArguments

instance ToShimWit JMShim (PinaforeType baseupdate 'Positive) MenuEntry where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance FromShimWit JMShim (PinaforeSingularType baseupdate 'Negative) MenuEntry where
    fromShimWit = mkPJMShimWit $ GroundPinaforeSingularType MenuItemPinaforeGroundType NilDolanArguments

instance FromShimWit JMShim (PinaforeType baseupdate 'Negative) MenuEntry where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- LangRef
instance ( FromShimWit JMShim (PinaforeType baseupdate 'Negative) p
         , ToShimWit JMShim (PinaforeType baseupdate 'Positive) q
         ) => ToShimWit JMShim (PinaforeSingularType baseupdate 'Positive) (LangRef '( p, q)) where
    toShimWit =
        unToRangeShimWit $ \tpq conv ->
            mapShimWit (consShimFunc RangevarianceType cid conv) $
            mkPJMShimWit $ GroundPinaforeSingularType RefPinaforeGroundType $ ConsDolanArguments tpq NilDolanArguments

instance ( FromShimWit JMShim (PinaforeType baseupdate 'Negative) p
         , ToShimWit JMShim (PinaforeType baseupdate 'Positive) q
         ) => ToShimWit JMShim (PinaforeType baseupdate 'Positive) (LangRef '( p, q)) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance ( ToShimWit JMShim (PinaforeType baseupdate 'Positive) p
         , FromShimWit JMShim (PinaforeType baseupdate 'Negative) q
         ) => FromShimWit JMShim (PinaforeSingularType baseupdate 'Negative) (LangRef '( p, q)) where
    fromShimWit =
        unFromRangeShimWit $ \tpq conv ->
            mapShimWit (consShimFunc RangevarianceType cid conv) $
            mkPJMShimWit $ GroundPinaforeSingularType RefPinaforeGroundType $ ConsDolanArguments tpq NilDolanArguments

instance ( ToShimWit JMShim (PinaforeType baseupdate 'Positive) p
         , FromShimWit JMShim (PinaforeType baseupdate 'Negative) q
         ) => FromShimWit JMShim (PinaforeType baseupdate 'Negative) (LangRef '( p, q)) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- PinaforeRef
instance ( FromShimWit JMShim (PinaforeType baseupdate 'Negative) t
         , ToShimWit JMShim (PinaforeType baseupdate 'Positive) t
         ) => ToShimWit JMShim (PinaforeType baseupdate 'Positive) (PinaforeRef (WholeUpdate (Know t))) where
    toShimWit = mapShimWit (toEnhanced "subtype" pinaforeRefToRef) toJMShimWit

instance ( FromShimWit JMShim (PinaforeType baseupdate 'Negative) t
         , ToShimWit JMShim (PinaforeType baseupdate 'Positive) t
         ) => FromShimWit JMShim (PinaforeType baseupdate 'Negative) (PinaforeRef (WholeUpdate (Know t))) where
    fromShimWit = mapShimWit (toEnhanced "subtype" langRefToValue) fromJMShimWit

-- PinaforeImmutableRef
instance (FromShimWit JMShim (PinaforeType baseupdate 'Negative) a) =>
             FromShimWit JMShim (PinaforeType baseupdate 'Negative) (PinaforeImmutableRef a) where
    fromShimWit = mapShimWit (toEnhanced "subtype" langRefToImmutable) fromJMShimWit

instance (ToShimWit JMShim (PinaforeType baseupdate 'Positive) a) =>
             ToShimWit JMShim (PinaforeType baseupdate 'Positive) (PinaforeImmutableRef a) where
    toShimWit = mapShimWit (toEnhanced "subtype" pinaforeImmutableToRef) toJMShimWit

-- PinaforeROWRef
instance (FromShimWit JMShim (PinaforeType baseupdate 'Negative) t) =>
             FromShimWit JMShim (PinaforeType baseupdate 'Negative) (PinaforeROWRef (Know t)) where
    fromShimWit = mapShimWit (toEnhanced "subtype" langRefToReadOnlyValue) fromJMShimWit

instance (ToShimWit JMShim (PinaforeType baseupdate 'Positive) t) =>
             ToShimWit JMShim (PinaforeType baseupdate 'Positive) (PinaforeROWRef (Know t)) where
    toShimWit = mapShimWit (toEnhanced "subtype" pinaforeROWRefToRef) toJMShimWit

-- LangSetRef
instance (FromShimWit JMShim (PinaforeType baseupdate 'Negative) a) =>
             ToShimWit JMShim (PinaforeSingularType baseupdate 'Positive) (LangSetRef a) where
    toShimWit =
        unShimWit fromJMShimWit $ \ta conva ->
            mapShimWit (consShimFunc ContravarianceType cid $ MkCatDual conva) $
            mkPJMShimWit $ GroundPinaforeSingularType SetRefPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (FromShimWit JMShim (PinaforeType baseupdate 'Negative) a) =>
             ToShimWit JMShim (PinaforeType baseupdate 'Positive) (LangSetRef a) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance (ToShimWit JMShim (PinaforeType baseupdate 'Positive) a) =>
             FromShimWit JMShim (PinaforeSingularType baseupdate 'Negative) (LangSetRef a) where
    fromShimWit =
        unShimWit toJMShimWit $ \ta conva ->
            mapShimWit (consShimFunc ContravarianceType cid $ MkCatDual conva) $
            mkPJMShimWit $ GroundPinaforeSingularType SetRefPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (ToShimWit JMShim (PinaforeType baseupdate 'Positive) a) =>
             FromShimWit JMShim (PinaforeType baseupdate 'Negative) (LangSetRef a) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- LangFiniteSetRef
instance ( FromShimWit JMShim (PinaforeType baseupdate 'Negative) p
         , ToShimWit JMShim (PinaforeType baseupdate 'Positive) q
         ) => ToShimWit JMShim (PinaforeSingularType baseupdate 'Positive) (LangFiniteSetRef '( p, q)) where
    toShimWit =
        unToRangeShimWit $ \tpq conv ->
            mapShimWit (consShimFunc RangevarianceType cid conv) $
            mkPJMShimWit $
            GroundPinaforeSingularType FiniteSetRefPinaforeGroundType $ ConsDolanArguments tpq NilDolanArguments

instance ( FromShimWit JMShim (PinaforeType baseupdate 'Negative) p
         , ToShimWit JMShim (PinaforeType baseupdate 'Positive) q
         ) => ToShimWit JMShim (PinaforeType baseupdate 'Positive) (LangFiniteSetRef '( p, q)) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance ( ToShimWit JMShim (PinaforeType baseupdate 'Positive) p
         , FromShimWit JMShim (PinaforeType baseupdate 'Negative) q
         ) => FromShimWit JMShim (PinaforeSingularType baseupdate 'Negative) (LangFiniteSetRef '( p, q)) where
    fromShimWit =
        unFromRangeShimWit $ \tpq conv ->
            mapShimWit (consShimFunc RangevarianceType cid conv) $
            mkPJMShimWit $
            GroundPinaforeSingularType FiniteSetRefPinaforeGroundType $ ConsDolanArguments tpq NilDolanArguments

instance ( ToShimWit JMShim (PinaforeType baseupdate 'Positive) p
         , FromShimWit JMShim (PinaforeType baseupdate 'Negative) q
         ) => FromShimWit JMShim (PinaforeType baseupdate 'Negative) (LangFiniteSetRef '( p, q)) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- PinaforeRef FiniteSetUpdate
instance ( ToShimWit JMShim (PinaforeType baseupdate 'Positive) t
         , FromShimWit JMShim (PinaforeType baseupdate 'Negative) t
         ) => FromShimWit JMShim (PinaforeType baseupdate 'Negative) (PinaforeRef (FiniteSetUpdate t)) where
    fromShimWit = mapShimWit (toEnhanced "subtype" unLangFiniteSetRef) fromJMShimWit

instance ( Eq t
         , ToShimWit JMShim (PinaforeType baseupdate 'Positive) t
         , FromShimWit JMShim (PinaforeType baseupdate 'Negative) t
         ) => ToShimWit JMShim (PinaforeType baseupdate 'Positive) (PinaforeRef (FiniteSetUpdate t)) where
    toShimWit = mapShimWit (toEnhanced "subtype" $ MkLangFiniteSetRef identityRange) toJMShimWit

-- LangMorphism
instance ( FromShimWit JMShim (PinaforeType baseupdate 'Negative) pa
         , ToShimWit JMShim (PinaforeType baseupdate 'Positive) qa
         , FromShimWit JMShim (PinaforeType baseupdate 'Negative) pb
         , ToShimWit JMShim (PinaforeType baseupdate 'Positive) qb
         ) => ToShimWit JMShim (PinaforeSingularType baseupdate 'Positive) (LangMorphism '( pa, qa) '( pb, qb)) where
    toShimWit =
        unToRangeShimWit @_ @pa @qa $ \ta conva ->
            unToRangeShimWit @_ @pb @qb $ \tb convb ->
                mapShimWit (consShimFunc RangevarianceType (consShimFunc RangevarianceType cid conva) convb) $
                mkPJMShimWit $
                GroundPinaforeSingularType MorphismPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( FromShimWit JMShim (PinaforeType baseupdate 'Negative) pa
         , ToShimWit JMShim (PinaforeType baseupdate 'Positive) qa
         , FromShimWit JMShim (PinaforeType baseupdate 'Negative) pb
         , ToShimWit JMShim (PinaforeType baseupdate 'Positive) qb
         ) => ToShimWit JMShim (PinaforeType baseupdate 'Positive) (LangMorphism '( pa, qa) '( pb, qb)) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance ( ToShimWit JMShim (PinaforeType baseupdate 'Positive) pa
         , FromShimWit JMShim (PinaforeType baseupdate 'Negative) qa
         , ToShimWit JMShim (PinaforeType baseupdate 'Positive) pb
         , FromShimWit JMShim (PinaforeType baseupdate 'Negative) qb
         ) => FromShimWit JMShim (PinaforeSingularType baseupdate 'Negative) (LangMorphism '( pa, qa) '( pb, qb)) where
    fromShimWit =
        unFromRangeShimWit $ \ta conva ->
            unFromRangeShimWit $ \tb convb ->
                mapShimWit (consShimFunc RangevarianceType (consShimFunc RangevarianceType cid conva) convb) $
                mkPJMShimWit $
                GroundPinaforeSingularType MorphismPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( ToShimWit JMShim (PinaforeType baseupdate 'Positive) pa
         , FromShimWit JMShim (PinaforeType baseupdate 'Negative) qa
         , ToShimWit JMShim (PinaforeType baseupdate 'Positive) pb
         , FromShimWit JMShim (PinaforeType baseupdate 'Negative) qb
         ) => FromShimWit JMShim (PinaforeType baseupdate 'Negative) (LangMorphism '( pa, qa) '( pb, qb)) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- Entity
instance ToShimWit JMShim (PinaforeSingularType baseupdate 'Positive) Entity where
    toShimWit =
        mkPJMShimWit $
        GroundPinaforeSingularType (EntityPinaforeGroundType NilListType TopEntityGroundType) NilDolanArguments

instance ToShimWit JMShim (PinaforeType baseupdate 'Positive) Entity where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance FromShimWit JMShim (PinaforeSingularType baseupdate 'Negative) Entity where
    fromShimWit =
        mkPJMShimWit $
        GroundPinaforeSingularType (EntityPinaforeGroundType NilListType TopEntityGroundType) NilDolanArguments

instance FromShimWit JMShim (PinaforeType baseupdate 'Negative) Entity where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- NewEntity
instance ToShimWit JMShim (PinaforeSingularType baseupdate 'Positive) NewEntity where
    toShimWit =
        mkPJMShimWit $
        GroundPinaforeSingularType (EntityPinaforeGroundType NilListType NewEntityGroundType) NilDolanArguments

instance ToShimWit JMShim (PinaforeType baseupdate 'Positive) NewEntity where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance FromShimWit JMShim (PinaforeSingularType baseupdate 'Negative) NewEntity where
    fromShimWit =
        mkPJMShimWit $
        GroundPinaforeSingularType (EntityPinaforeGroundType NilListType NewEntityGroundType) NilDolanArguments

instance FromShimWit JMShim (PinaforeType baseupdate 'Negative) NewEntity where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- Literal
literalInstances :: _ -> _
literalInstances t =
    [d|
  
  instance ToShimWit JMShim
             (PinaforeSingularType baseupdate 'Positive)
             $( t )
           where
          toShimWit
            = mkPJMShimWit $
                GroundPinaforeSingularType
                  (EntityPinaforeGroundType NilListType $
                     LiteralEntityGroundType representative)
                  NilDolanArguments
  
  instance ToShimWit JMShim (PinaforeType baseupdate 'Positive)
             $( t )
           where
          toShimWit = singlePinaforeShimWit toJMShimWit
  
  instance FromShimWit JMShim
             (PinaforeSingularType baseupdate 'Negative)
             $( t )
           where
          fromShimWit
            = mkPJMShimWit $
                GroundPinaforeSingularType
                  (EntityPinaforeGroundType NilListType $
                     LiteralEntityGroundType representative)
                  NilDolanArguments
  
  instance FromShimWit JMShim (PinaforeType baseupdate 'Negative)
             $( t )
           where
          fromShimWit = singlePinaforeShimWit fromJMShimWit
  |]
