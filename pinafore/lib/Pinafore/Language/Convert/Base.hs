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
       forall t. ToPinaforeType t
    => Text
qPositiveTypeDescription =
    case toJMShimWit @(PinaforeType 'Positive) @t of
        MkShimWit w _ -> pack $ show w

qNegativeTypeDescription ::
       forall t. FromPinaforeType t
    => Text
qNegativeTypeDescription =
    case fromJMShimWit @(PinaforeType 'Negative) @t of
        MkShimWit w _ -> pack $ show w

type ToPinaforeType = ToShimWit JMShim (PinaforeType 'Positive)

type FromPinaforeType = FromShimWit JMShim (PinaforeType 'Negative)

-- top, bottom, join, meet
instance ToShimWit JMShim (PinaforeType 'Positive) BottomType where
    toShimWit = mkPJMShimWit NilPinaforeType

instance FromShimWit JMShim (PinaforeType 'Negative) TopType where
    fromShimWit = mkPJMShimWit NilPinaforeType

instance (ToShimWit JMShim (PinaforeType 'Positive) a, ToShimWit JMShim (PinaforeType 'Positive) b) =>
             ToShimWit JMShim (PinaforeType 'Positive) (JoinType a b) where
    toShimWit = joinPinaforeShimWit toJMShimWit toJMShimWit

instance (FromShimWit JMShim (PinaforeType 'Negative) a, FromShimWit JMShim (PinaforeType 'Negative) b) =>
             FromShimWit JMShim (PinaforeType 'Negative) (MeetType a b) where
    fromShimWit = meetPinaforeShimWit fromJMShimWit fromJMShimWit

-- UVar
instance KnownSymbol name => ToShimWit JMShim (PinaforeSingularType 'Positive) (UVar name) where
    toShimWit = mkPJMShimWit $ VarPinaforeSingularType MkSymbolType

instance KnownSymbol name => ToShimWit JMShim (PinaforeType 'Positive) (UVar name) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance KnownSymbol name => FromShimWit JMShim (PinaforeSingularType 'Negative) (UVar name) where
    fromShimWit = mkPJMShimWit $ VarPinaforeSingularType MkSymbolType

instance KnownSymbol name => FromShimWit JMShim (PinaforeType 'Negative) (UVar name) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- (,)
instance (ToShimWit JMShim (PinaforeType 'Positive) a, ToShimWit JMShim (PinaforeType 'Positive) b) =>
             ToShimWit JMShim (PinaforeSingularType 'Positive) (a, b) where
    toShimWit =
        unShimWit toJMShimWit $ \ta conva ->
            unShimWit toJMShimWit $ \tb convb ->
                mapShimWit (consShimFunc CovarianceType (cfmap conva) convb) $
                mkPJMShimWit $
                GroundPinaforeSingularType
                    (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) PairEntityGroundType) $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance (ToShimWit JMShim (PinaforeType 'Positive) a, ToShimWit JMShim (PinaforeType 'Positive) b) =>
             ToShimWit JMShim (PinaforeType 'Positive) (a, b) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance (FromShimWit JMShim (PinaforeType 'Negative) a, FromShimWit JMShim (PinaforeType 'Negative) b) =>
             FromShimWit JMShim (PinaforeSingularType 'Negative) (a, b) where
    fromShimWit =
        unShimWit fromJMShimWit $ \ta conva ->
            unShimWit fromJMShimWit $ \tb convb ->
                mapShimWit (consShimFunc CovarianceType (cfmap conva) convb) $
                mkPJMShimWit $
                GroundPinaforeSingularType
                    (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) PairEntityGroundType) $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance (FromShimWit JMShim (PinaforeType 'Negative) a, FromShimWit JMShim (PinaforeType 'Negative) b) =>
             FromShimWit JMShim (PinaforeType 'Negative) (Either a b) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- Either
instance (ToShimWit JMShim (PinaforeType 'Positive) a, ToShimWit JMShim (PinaforeType 'Positive) b) =>
             ToShimWit JMShim (PinaforeSingularType 'Positive) (Either a b) where
    toShimWit =
        unShimWit toJMShimWit $ \ta conva ->
            unShimWit toJMShimWit $ \tb convb ->
                mapShimWit (consShimFunc CovarianceType (cfmap conva) convb) $
                mkPJMShimWit $
                GroundPinaforeSingularType
                    (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) EitherEntityGroundType) $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance (ToShimWit JMShim (PinaforeType 'Positive) a, ToShimWit JMShim (PinaforeType 'Positive) b) =>
             ToShimWit JMShim (PinaforeType 'Positive) (Either a b) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance (FromShimWit JMShim (PinaforeType 'Negative) a, FromShimWit JMShim (PinaforeType 'Negative) b) =>
             FromShimWit JMShim (PinaforeSingularType 'Negative) (Either a b) where
    fromShimWit =
        unShimWit fromJMShimWit $ \ta conva ->
            unShimWit fromJMShimWit $ \tb convb ->
                mapShimWit (consShimFunc CovarianceType (cfmap conva) convb) $
                mkPJMShimWit $
                GroundPinaforeSingularType
                    (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) EitherEntityGroundType) $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance (FromShimWit JMShim (PinaforeType 'Negative) a, FromShimWit JMShim (PinaforeType 'Negative) b) =>
             FromShimWit JMShim (PinaforeType 'Negative) (a, b) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- (->)
instance (FromShimWit JMShim (PinaforeType 'Negative) a, ToShimWit JMShim (PinaforeType 'Positive) b) =>
             ToShimWit JMShim (PinaforeSingularType 'Positive) (a -> b) where
    toShimWit =
        unShimWit fromJMShimWit $ \ta conva ->
            unShimWit toJMShimWit $ \tb convb ->
                mapShimWit (consShimFunc CovarianceType (ccontramap conva) convb) $
                mkPJMShimWit $
                GroundPinaforeSingularType FuncPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance (FromShimWit JMShim (PinaforeType 'Negative) a, ToShimWit JMShim (PinaforeType 'Positive) b) =>
             ToShimWit JMShim (PinaforeType 'Positive) (a -> b) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance (ToShimWit JMShim (PinaforeType 'Positive) a, FromShimWit JMShim (PinaforeType 'Negative) b) =>
             FromShimWit JMShim (PinaforeSingularType 'Negative) (a -> b) where
    fromShimWit =
        unShimWit toJMShimWit $ \ta conva ->
            unShimWit fromJMShimWit $ \tb convb ->
                mapShimWit (consShimFunc CovarianceType (ccontramap conva) convb) $
                mkPJMShimWit $
                GroundPinaforeSingularType FuncPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance (ToShimWit JMShim (PinaforeType 'Positive) a, FromShimWit JMShim (PinaforeType 'Negative) b) =>
             FromShimWit JMShim (PinaforeType 'Negative) (a -> b) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- Maybe
instance (ToShimWit JMShim (PinaforeType 'Positive) a) => ToShimWit JMShim (PinaforeSingularType 'Positive) (Maybe a) where
    toShimWit =
        unShimWit toJMShimWit $ \ta conva ->
            mapShimWit (cfmap conva) $
            mkPJMShimWit $
            GroundPinaforeSingularType (EntityPinaforeGroundType (ConsListType Refl NilListType) MaybeEntityGroundType) $
            ConsDolanArguments ta NilDolanArguments

instance (ToShimWit JMShim (PinaforeType 'Positive) a) => ToShimWit JMShim (PinaforeType 'Positive) (Maybe a) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance (FromShimWit JMShim (PinaforeType 'Negative) a) =>
             FromShimWit JMShim (PinaforeSingularType 'Negative) (Maybe a) where
    fromShimWit =
        unShimWit fromJMShimWit $ \ta conva ->
            mapShimWit (cfmap conva) $
            mkPJMShimWit $
            GroundPinaforeSingularType (EntityPinaforeGroundType (ConsListType Refl NilListType) MaybeEntityGroundType) $
            ConsDolanArguments ta NilDolanArguments

instance (FromShimWit JMShim (PinaforeType 'Negative) a) => FromShimWit JMShim (PinaforeType 'Negative) (Maybe a) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- []
instance (ToShimWit JMShim (PinaforeType 'Positive) a) => ToShimWit JMShim (PinaforeSingularType 'Positive) [a] where
    toShimWit =
        unShimWit toJMShimWit $ \ta conva ->
            mapShimWit (cfmap conva) $
            mkPJMShimWit $
            GroundPinaforeSingularType (EntityPinaforeGroundType (ConsListType Refl NilListType) ListEntityGroundType) $
            ConsDolanArguments ta NilDolanArguments

instance (ToShimWit JMShim (PinaforeType 'Positive) a) => ToShimWit JMShim (PinaforeType 'Positive) [a] where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance (FromShimWit JMShim (PinaforeType 'Negative) a) => FromShimWit JMShim (PinaforeSingularType 'Negative) [a] where
    fromShimWit =
        unShimWit fromJMShimWit $ \ta conva ->
            mapShimWit (cfmap conva) $
            mkPJMShimWit $
            GroundPinaforeSingularType (EntityPinaforeGroundType (ConsListType Refl NilListType) ListEntityGroundType) $
            ConsDolanArguments ta NilDolanArguments

instance (FromShimWit JMShim (PinaforeType 'Negative) a) => FromShimWit JMShim (PinaforeType 'Negative) [a] where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- PinaforeAction
instance (ToShimWit JMShim (PinaforeType 'Positive) a) =>
             ToShimWit JMShim (PinaforeSingularType 'Positive) (PinaforeAction a) where
    toShimWit =
        unShimWit toJMShimWit $ \ta conva ->
            mapShimWit (cfmap conva) $
            mkPJMShimWit $ GroundPinaforeSingularType ActionPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (ToShimWit JMShim (PinaforeType 'Positive) a) => ToShimWit JMShim (PinaforeType 'Positive) (PinaforeAction a) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance (FromShimWit JMShim (PinaforeType 'Negative) a) =>
             FromShimWit JMShim (PinaforeSingularType 'Negative) (PinaforeAction a) where
    fromShimWit =
        unShimWit fromJMShimWit $ \ta conva ->
            mapShimWit (cfmap conva) $
            mkPJMShimWit $ GroundPinaforeSingularType ActionPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (FromShimWit JMShim (PinaforeType 'Negative) a) =>
             FromShimWit JMShim (PinaforeType 'Negative) (PinaforeAction a) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- IO
instance (ToShimWit JMShim (PinaforeType 'Positive) a) => ToShimWit JMShim (PinaforeType 'Positive) (IO a) where
    toShimWit = mapShimWit (toEnhanced "subtype" (liftIO :: IO a -> PinaforeAction a)) toJMShimWit

-- View
instance (ToShimWit JMShim (PinaforeType 'Positive) a) => ToShimWit JMShim (PinaforeType 'Positive) (View a) where
    toShimWit = mapShimWit (toEnhanced "subtype" viewPinaforeAction) toJMShimWit

-- LangOrder
instance (FromShimWit JMShim (PinaforeType 'Negative) a) =>
             ToShimWit JMShim (PinaforeSingularType 'Positive) (LangOrder a) where
    toShimWit =
        unShimWit fromJMShimWit $ \ta conva ->
            mapShimWit (consShimFunc ContravarianceType cid $ MkCatDual conva) $
            mkPJMShimWit $ GroundPinaforeSingularType OrderPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (FromShimWit JMShim (PinaforeType 'Negative) a) => ToShimWit JMShim (PinaforeType 'Positive) (LangOrder a) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance (ToShimWit JMShim (PinaforeType 'Positive) a) =>
             FromShimWit JMShim (PinaforeSingularType 'Negative) (LangOrder a) where
    fromShimWit =
        unShimWit toJMShimWit $ \ta conva ->
            mapShimWit (consShimFunc ContravarianceType cid $ MkCatDual conva) $
            mkPJMShimWit $ GroundPinaforeSingularType OrderPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (ToShimWit JMShim (PinaforeType 'Positive) a) => FromShimWit JMShim (PinaforeType 'Negative) (LangOrder a) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- LangNotifier
instance (FromShimWit JMShim (PinaforeType 'Negative) a) =>
             ToShimWit JMShim (PinaforeSingularType 'Positive) (LangNotifier a) where
    toShimWit =
        unShimWit fromJMShimWit $ \ta conva ->
            mapShimWit (consShimFunc ContravarianceType cid $ MkCatDual conva) $
            mkPJMShimWit $
            GroundPinaforeSingularType NotifierPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (FromShimWit JMShim (PinaforeType 'Negative) a) => ToShimWit JMShim (PinaforeType 'Positive) (LangNotifier a) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance (ToShimWit JMShim (PinaforeType 'Positive) a) =>
             FromShimWit JMShim (PinaforeSingularType 'Negative) (LangNotifier a) where
    fromShimWit =
        unShimWit toJMShimWit $ \ta conva ->
            mapShimWit (consShimFunc ContravarianceType cid $ MkCatDual conva) $
            mkPJMShimWit $
            GroundPinaforeSingularType NotifierPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (ToShimWit JMShim (PinaforeType 'Positive) a) => FromShimWit JMShim (PinaforeType 'Negative) (LangNotifier a) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- LangUI
instance ToShimWit JMShim (PinaforeSingularType 'Positive) LangUI where
    toShimWit = mkPJMShimWit $ GroundPinaforeSingularType UserInterfacePinaforeGroundType NilDolanArguments

instance ToShimWit JMShim (PinaforeType 'Positive) LangUI where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance FromShimWit JMShim (PinaforeSingularType 'Negative) LangUI where
    fromShimWit = mkPJMShimWit $ GroundPinaforeSingularType UserInterfacePinaforeGroundType NilDolanArguments

instance FromShimWit JMShim (PinaforeType 'Negative) LangUI where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- PinaforeWindow
instance ToShimWit JMShim (PinaforeSingularType 'Positive) PinaforeWindow where
    toShimWit = mkPJMShimWit $ GroundPinaforeSingularType WindowPinaforeGroundType NilDolanArguments

instance ToShimWit JMShim (PinaforeType 'Positive) PinaforeWindow where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance FromShimWit JMShim (PinaforeSingularType 'Negative) PinaforeWindow where
    fromShimWit = mkPJMShimWit $ GroundPinaforeSingularType WindowPinaforeGroundType NilDolanArguments

instance FromShimWit JMShim (PinaforeType 'Negative) PinaforeWindow where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- MenuEntry
instance ToShimWit JMShim (PinaforeSingularType 'Positive) MenuEntry where
    toShimWit = mkPJMShimWit $ GroundPinaforeSingularType MenuItemPinaforeGroundType NilDolanArguments

instance ToShimWit JMShim (PinaforeType 'Positive) MenuEntry where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance FromShimWit JMShim (PinaforeSingularType 'Negative) MenuEntry where
    fromShimWit = mkPJMShimWit $ GroundPinaforeSingularType MenuItemPinaforeGroundType NilDolanArguments

instance FromShimWit JMShim (PinaforeType 'Negative) MenuEntry where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- LangRef
instance (FromShimWit JMShim (PinaforeType 'Negative) p, ToShimWit JMShim (PinaforeType 'Positive) q) =>
             ToShimWit JMShim (PinaforeSingularType 'Positive) (LangRef '( p, q)) where
    toShimWit =
        unToRangeShimWit $ \tpq conv ->
            mapShimWit (consShimFunc RangevarianceType cid conv) $
            mkPJMShimWit $ GroundPinaforeSingularType RefPinaforeGroundType $ ConsDolanArguments tpq NilDolanArguments

instance (FromShimWit JMShim (PinaforeType 'Negative) p, ToShimWit JMShim (PinaforeType 'Positive) q) =>
             ToShimWit JMShim (PinaforeType 'Positive) (LangRef '( p, q)) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance (ToShimWit JMShim (PinaforeType 'Positive) p, FromShimWit JMShim (PinaforeType 'Negative) q) =>
             FromShimWit JMShim (PinaforeSingularType 'Negative) (LangRef '( p, q)) where
    fromShimWit =
        unFromRangeShimWit $ \tpq conv ->
            mapShimWit (consShimFunc RangevarianceType cid conv) $
            mkPJMShimWit $ GroundPinaforeSingularType RefPinaforeGroundType $ ConsDolanArguments tpq NilDolanArguments

instance (ToShimWit JMShim (PinaforeType 'Positive) p, FromShimWit JMShim (PinaforeType 'Negative) q) =>
             FromShimWit JMShim (PinaforeType 'Negative) (LangRef '( p, q)) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- PinaforeRef
instance (FromShimWit JMShim (PinaforeType 'Negative) t, ToShimWit JMShim (PinaforeType 'Positive) t) =>
             ToShimWit JMShim (PinaforeType 'Positive) (PinaforeRef (WholeUpdate (Know t))) where
    toShimWit = mapShimWit (toEnhanced "subtype" pinaforeRefToRef) toJMShimWit

instance (FromShimWit JMShim (PinaforeType 'Negative) t, ToShimWit JMShim (PinaforeType 'Positive) t) =>
             FromShimWit JMShim (PinaforeType 'Negative) (PinaforeRef (WholeUpdate (Know t))) where
    fromShimWit = mapShimWit (toEnhanced "subtype" langRefToValue) fromJMShimWit

-- PinaforeImmutableRef
instance (FromShimWit JMShim (PinaforeType 'Negative) a) =>
             FromShimWit JMShim (PinaforeType 'Negative) (PinaforeImmutableRef a) where
    fromShimWit = mapShimWit (toEnhanced "subtype" langRefToImmutable) fromJMShimWit

instance (ToShimWit JMShim (PinaforeType 'Positive) a) =>
             ToShimWit JMShim (PinaforeType 'Positive) (PinaforeImmutableRef a) where
    toShimWit = mapShimWit (toEnhanced "subtype" pinaforeImmutableToRef) toJMShimWit

-- PinaforeROWRef
instance (FromShimWit JMShim (PinaforeType 'Negative) t) =>
             FromShimWit JMShim (PinaforeType 'Negative) (PinaforeROWRef (Know t)) where
    fromShimWit = mapShimWit (toEnhanced "subtype" langRefToReadOnlyValue) fromJMShimWit

instance (ToShimWit JMShim (PinaforeType 'Positive) t) =>
             ToShimWit JMShim (PinaforeType 'Positive) (PinaforeROWRef (Know t)) where
    toShimWit = mapShimWit (toEnhanced "subtype" pinaforeROWRefToRef) toJMShimWit

-- LangSetRef
instance (FromShimWit JMShim (PinaforeType 'Negative) a) =>
             ToShimWit JMShim (PinaforeSingularType 'Positive) (LangSetRef a) where
    toShimWit =
        unShimWit fromJMShimWit $ \ta conva ->
            mapShimWit (consShimFunc ContravarianceType cid $ MkCatDual conva) $
            mkPJMShimWit $ GroundPinaforeSingularType SetRefPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (FromShimWit JMShim (PinaforeType 'Negative) a) => ToShimWit JMShim (PinaforeType 'Positive) (LangSetRef a) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance (ToShimWit JMShim (PinaforeType 'Positive) a) =>
             FromShimWit JMShim (PinaforeSingularType 'Negative) (LangSetRef a) where
    fromShimWit =
        unShimWit toJMShimWit $ \ta conva ->
            mapShimWit (consShimFunc ContravarianceType cid $ MkCatDual conva) $
            mkPJMShimWit $ GroundPinaforeSingularType SetRefPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (ToShimWit JMShim (PinaforeType 'Positive) a) => FromShimWit JMShim (PinaforeType 'Negative) (LangSetRef a) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- LangFiniteSetRef
instance (FromShimWit JMShim (PinaforeType 'Negative) p, ToShimWit JMShim (PinaforeType 'Positive) q) =>
             ToShimWit JMShim (PinaforeSingularType 'Positive) (LangFiniteSetRef '( p, q)) where
    toShimWit =
        unToRangeShimWit $ \tpq conv ->
            mapShimWit (consShimFunc RangevarianceType cid conv) $
            mkPJMShimWit $
            GroundPinaforeSingularType FiniteSetRefPinaforeGroundType $ ConsDolanArguments tpq NilDolanArguments

instance (FromShimWit JMShim (PinaforeType 'Negative) p, ToShimWit JMShim (PinaforeType 'Positive) q) =>
             ToShimWit JMShim (PinaforeType 'Positive) (LangFiniteSetRef '( p, q)) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance (ToShimWit JMShim (PinaforeType 'Positive) p, FromShimWit JMShim (PinaforeType 'Negative) q) =>
             FromShimWit JMShim (PinaforeSingularType 'Negative) (LangFiniteSetRef '( p, q)) where
    fromShimWit =
        unFromRangeShimWit $ \tpq conv ->
            mapShimWit (consShimFunc RangevarianceType cid conv) $
            mkPJMShimWit $
            GroundPinaforeSingularType FiniteSetRefPinaforeGroundType $ ConsDolanArguments tpq NilDolanArguments

instance (ToShimWit JMShim (PinaforeType 'Positive) p, FromShimWit JMShim (PinaforeType 'Negative) q) =>
             FromShimWit JMShim (PinaforeType 'Negative) (LangFiniteSetRef '( p, q)) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- PinaforeRef FiniteSetUpdate
instance (ToShimWit JMShim (PinaforeType 'Positive) t, FromShimWit JMShim (PinaforeType 'Negative) t) =>
             FromShimWit JMShim (PinaforeType 'Negative) (PinaforeRef (FiniteSetUpdate t)) where
    fromShimWit = mapShimWit (toEnhanced "subtype" unLangFiniteSetRef) fromJMShimWit

instance (Eq t, ToShimWit JMShim (PinaforeType 'Positive) t, FromShimWit JMShim (PinaforeType 'Negative) t) =>
             ToShimWit JMShim (PinaforeType 'Positive) (PinaforeRef (FiniteSetUpdate t)) where
    toShimWit = mapShimWit (toEnhanced "subtype" $ MkLangFiniteSetRef identityRange) toJMShimWit

-- LangMorphism
instance ( FromShimWit JMShim (PinaforeType 'Negative) pa
         , ToShimWit JMShim (PinaforeType 'Positive) qa
         , FromShimWit JMShim (PinaforeType 'Negative) pb
         , ToShimWit JMShim (PinaforeType 'Positive) qb
         ) => ToShimWit JMShim (PinaforeSingularType 'Positive) (LangMorphism '( pa, qa) '( pb, qb)) where
    toShimWit =
        unToRangeShimWit @_ @pa @qa $ \ta conva ->
            unToRangeShimWit @_ @pb @qb $ \tb convb ->
                mapShimWit (consShimFunc RangevarianceType (consShimFunc RangevarianceType cid conva) convb) $
                mkPJMShimWit $
                GroundPinaforeSingularType MorphismPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( FromShimWit JMShim (PinaforeType 'Negative) pa
         , ToShimWit JMShim (PinaforeType 'Positive) qa
         , FromShimWit JMShim (PinaforeType 'Negative) pb
         , ToShimWit JMShim (PinaforeType 'Positive) qb
         ) => ToShimWit JMShim (PinaforeType 'Positive) (LangMorphism '( pa, qa) '( pb, qb)) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance ( ToShimWit JMShim (PinaforeType 'Positive) pa
         , FromShimWit JMShim (PinaforeType 'Negative) qa
         , ToShimWit JMShim (PinaforeType 'Positive) pb
         , FromShimWit JMShim (PinaforeType 'Negative) qb
         ) => FromShimWit JMShim (PinaforeSingularType 'Negative) (LangMorphism '( pa, qa) '( pb, qb)) where
    fromShimWit =
        unFromRangeShimWit $ \ta conva ->
            unFromRangeShimWit $ \tb convb ->
                mapShimWit (consShimFunc RangevarianceType (consShimFunc RangevarianceType cid conva) convb) $
                mkPJMShimWit $
                GroundPinaforeSingularType MorphismPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( ToShimWit JMShim (PinaforeType 'Positive) pa
         , FromShimWit JMShim (PinaforeType 'Negative) qa
         , ToShimWit JMShim (PinaforeType 'Positive) pb
         , FromShimWit JMShim (PinaforeType 'Negative) qb
         ) => FromShimWit JMShim (PinaforeType 'Negative) (LangMorphism '( pa, qa) '( pb, qb)) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- Entity
instance ToShimWit JMShim (PinaforeSingularType 'Positive) Entity where
    toShimWit =
        mkPJMShimWit $
        GroundPinaforeSingularType (EntityPinaforeGroundType NilListType TopEntityGroundType) NilDolanArguments

instance ToShimWit JMShim (PinaforeType 'Positive) Entity where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance FromShimWit JMShim (PinaforeSingularType 'Negative) Entity where
    fromShimWit =
        mkPJMShimWit $
        GroundPinaforeSingularType (EntityPinaforeGroundType NilListType TopEntityGroundType) NilDolanArguments

instance FromShimWit JMShim (PinaforeType 'Negative) Entity where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- NewEntity
instance ToShimWit JMShim (PinaforeSingularType 'Positive) NewEntity where
    toShimWit =
        mkPJMShimWit $
        GroundPinaforeSingularType (EntityPinaforeGroundType NilListType NewEntityGroundType) NilDolanArguments

instance ToShimWit JMShim (PinaforeType 'Positive) NewEntity where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance FromShimWit JMShim (PinaforeSingularType 'Negative) NewEntity where
    fromShimWit =
        mkPJMShimWit $
        GroundPinaforeSingularType (EntityPinaforeGroundType NilListType NewEntityGroundType) NilDolanArguments

instance FromShimWit JMShim (PinaforeType 'Negative) NewEntity where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- Literal
literalInstances :: _ -> _
literalInstances t =
    [d|
  
  instance ToShimWit JMShim (PinaforeSingularType 'Positive) $( t )
           where
          toShimWit
            = mkPJMShimWit $
                GroundPinaforeSingularType
                  (EntityPinaforeGroundType NilListType $
                     LiteralEntityGroundType representative)
                  NilDolanArguments
  
  instance ToShimWit JMShim (PinaforeType 'Positive) $( t ) where
          toShimWit = singlePinaforeShimWit toJMShimWit
  
  instance FromShimWit JMShim (PinaforeSingularType 'Negative) $( t )
           where
          fromShimWit
            = mkPJMShimWit $
                GroundPinaforeSingularType
                  (EntityPinaforeGroundType NilListType $
                     LiteralEntityGroundType representative)
                  NilDolanArguments
  
  instance FromShimWit JMShim (PinaforeType 'Negative) $( t ) where
          fromShimWit = singlePinaforeShimWit fromJMShimWit
  |]
