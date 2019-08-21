{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Convert.Base
    ( qTypeDescription
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

qTypeDescription ::
       forall baseedit t. ToPinaforeType baseedit t
    => Text
qTypeDescription =
    case toJMShimWit @(PinaforeType baseedit 'Positive) @t of
        MkShimWit w _ -> pack $ show w

type ToPinaforeType baseedit = ToShimWit JMShim (PinaforeType baseedit 'Positive)

type FromPinaforeType baseedit = FromShimWit JMShim (PinaforeType baseedit 'Negative)

-- top, bottom, join, meet
instance ToShimWit JMShim (PinaforeType baseedit 'Positive) BottomType where
    toShimWit = mkPJMShimWit NilPinaforeType

instance FromShimWit JMShim (PinaforeType baseedit 'Negative) TopType where
    fromShimWit = mkPJMShimWit NilPinaforeType

instance (ToShimWit JMShim (PinaforeType baseedit 'Positive) a, ToShimWit JMShim (PinaforeType baseedit 'Positive) b) =>
             ToShimWit JMShim (PinaforeType baseedit 'Positive) (JoinType a b) where
    toShimWit = joinPinaforeShimWit toJMShimWit toJMShimWit

instance ( FromShimWit JMShim (PinaforeType baseedit 'Negative) a
         , FromShimWit JMShim (PinaforeType baseedit 'Negative) b
         ) => FromShimWit JMShim (PinaforeType baseedit 'Negative) (MeetType a b) where
    fromShimWit = meetPinaforeShimWit fromJMShimWit fromJMShimWit

-- UVar
instance KnownSymbol name => ToShimWit JMShim (PinaforeSingularType baseedit 'Positive) (UVar name) where
    toShimWit = mkPJMShimWit $ VarPinaforeSingularType MkSymbolType

instance KnownSymbol name => ToShimWit JMShim (PinaforeType baseedit 'Positive) (UVar name) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance KnownSymbol name => FromShimWit JMShim (PinaforeSingularType baseedit 'Negative) (UVar name) where
    fromShimWit = mkPJMShimWit $ VarPinaforeSingularType MkSymbolType

instance KnownSymbol name => FromShimWit JMShim (PinaforeType baseedit 'Negative) (UVar name) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- (,)
instance (ToShimWit JMShim (PinaforeType baseedit 'Positive) a, ToShimWit JMShim (PinaforeType baseedit 'Positive) b) =>
             ToShimWit JMShim (PinaforeSingularType baseedit 'Positive) (a, b) where
    toShimWit =
        unShimWit toJMShimWit $ \ta conva ->
            unShimWit toJMShimWit $ \tb convb ->
                mapShimWit (consShimFunc CovarianceType (cfmap conva) convb) $
                mkPJMShimWit $
                GroundPinaforeSingularType
                    (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) PairEntityGroundType) $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance (ToShimWit JMShim (PinaforeType baseedit 'Positive) a, ToShimWit JMShim (PinaforeType baseedit 'Positive) b) =>
             ToShimWit JMShim (PinaforeType baseedit 'Positive) (a, b) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance ( FromShimWit JMShim (PinaforeType baseedit 'Negative) a
         , FromShimWit JMShim (PinaforeType baseedit 'Negative) b
         ) => FromShimWit JMShim (PinaforeSingularType baseedit 'Negative) (a, b) where
    fromShimWit =
        unShimWit fromJMShimWit $ \ta conva ->
            unShimWit fromJMShimWit $ \tb convb ->
                mapShimWit (consShimFunc CovarianceType (cfmap conva) convb) $
                mkPJMShimWit $
                GroundPinaforeSingularType
                    (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) PairEntityGroundType) $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( FromShimWit JMShim (PinaforeType baseedit 'Negative) a
         , FromShimWit JMShim (PinaforeType baseedit 'Negative) b
         ) => FromShimWit JMShim (PinaforeType baseedit 'Negative) (Either a b) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- Either
instance (ToShimWit JMShim (PinaforeType baseedit 'Positive) a, ToShimWit JMShim (PinaforeType baseedit 'Positive) b) =>
             ToShimWit JMShim (PinaforeSingularType baseedit 'Positive) (Either a b) where
    toShimWit =
        unShimWit toJMShimWit $ \ta conva ->
            unShimWit toJMShimWit $ \tb convb ->
                mapShimWit (consShimFunc CovarianceType (cfmap conva) convb) $
                mkPJMShimWit $
                GroundPinaforeSingularType
                    (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) EitherEntityGroundType) $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance (ToShimWit JMShim (PinaforeType baseedit 'Positive) a, ToShimWit JMShim (PinaforeType baseedit 'Positive) b) =>
             ToShimWit JMShim (PinaforeType baseedit 'Positive) (Either a b) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance ( FromShimWit JMShim (PinaforeType baseedit 'Negative) a
         , FromShimWit JMShim (PinaforeType baseedit 'Negative) b
         ) => FromShimWit JMShim (PinaforeSingularType baseedit 'Negative) (Either a b) where
    fromShimWit =
        unShimWit fromJMShimWit $ \ta conva ->
            unShimWit fromJMShimWit $ \tb convb ->
                mapShimWit (consShimFunc CovarianceType (cfmap conva) convb) $
                mkPJMShimWit $
                GroundPinaforeSingularType
                    (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) EitherEntityGroundType) $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( FromShimWit JMShim (PinaforeType baseedit 'Negative) a
         , FromShimWit JMShim (PinaforeType baseedit 'Negative) b
         ) => FromShimWit JMShim (PinaforeType baseedit 'Negative) (a, b) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- (->)
instance (FromShimWit JMShim (PinaforeType baseedit 'Negative) a, ToShimWit JMShim (PinaforeType baseedit 'Positive) b) =>
             ToShimWit JMShim (PinaforeSingularType baseedit 'Positive) (a -> b) where
    toShimWit =
        unShimWit fromJMShimWit $ \ta conva ->
            unShimWit toJMShimWit $ \tb convb ->
                mapShimWit (consShimFunc CovarianceType (ccontramap conva) convb) $
                mkPJMShimWit $
                GroundPinaforeSingularType FuncPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance (FromShimWit JMShim (PinaforeType baseedit 'Negative) a, ToShimWit JMShim (PinaforeType baseedit 'Positive) b) =>
             ToShimWit JMShim (PinaforeType baseedit 'Positive) (a -> b) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance (ToShimWit JMShim (PinaforeType baseedit 'Positive) a, FromShimWit JMShim (PinaforeType baseedit 'Negative) b) =>
             FromShimWit JMShim (PinaforeSingularType baseedit 'Negative) (a -> b) where
    fromShimWit =
        unShimWit toJMShimWit $ \ta conva ->
            unShimWit fromJMShimWit $ \tb convb ->
                mapShimWit (consShimFunc CovarianceType (ccontramap conva) convb) $
                mkPJMShimWit $
                GroundPinaforeSingularType FuncPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance (ToShimWit JMShim (PinaforeType baseedit 'Positive) a, FromShimWit JMShim (PinaforeType baseedit 'Negative) b) =>
             FromShimWit JMShim (PinaforeType baseedit 'Negative) (a -> b) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- Maybe
instance (ToShimWit JMShim (PinaforeType baseedit 'Positive) a) =>
             ToShimWit JMShim (PinaforeSingularType baseedit 'Positive) (Maybe a) where
    toShimWit =
        unShimWit toJMShimWit $ \ta conva ->
            mapShimWit (cfmap conva) $
            mkPJMShimWit $
            GroundPinaforeSingularType (EntityPinaforeGroundType (ConsListType Refl NilListType) MaybeEntityGroundType) $
            ConsDolanArguments ta NilDolanArguments

instance (ToShimWit JMShim (PinaforeType baseedit 'Positive) a) =>
             ToShimWit JMShim (PinaforeType baseedit 'Positive) (Maybe a) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance (FromShimWit JMShim (PinaforeType baseedit 'Negative) a) =>
             FromShimWit JMShim (PinaforeSingularType baseedit 'Negative) (Maybe a) where
    fromShimWit =
        unShimWit fromJMShimWit $ \ta conva ->
            mapShimWit (cfmap conva) $
            mkPJMShimWit $
            GroundPinaforeSingularType (EntityPinaforeGroundType (ConsListType Refl NilListType) MaybeEntityGroundType) $
            ConsDolanArguments ta NilDolanArguments

instance (FromShimWit JMShim (PinaforeType baseedit 'Negative) a) =>
             FromShimWit JMShim (PinaforeType baseedit 'Negative) (Maybe a) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- []
instance (ToShimWit JMShim (PinaforeType baseedit 'Positive) a) =>
             ToShimWit JMShim (PinaforeSingularType baseedit 'Positive) [a] where
    toShimWit =
        unShimWit toJMShimWit $ \ta conva ->
            mapShimWit (cfmap conva) $
            mkPJMShimWit $
            GroundPinaforeSingularType (EntityPinaforeGroundType (ConsListType Refl NilListType) ListEntityGroundType) $
            ConsDolanArguments ta NilDolanArguments

instance (ToShimWit JMShim (PinaforeType baseedit 'Positive) a) =>
             ToShimWit JMShim (PinaforeType baseedit 'Positive) [a] where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance (FromShimWit JMShim (PinaforeType baseedit 'Negative) a) =>
             FromShimWit JMShim (PinaforeSingularType baseedit 'Negative) [a] where
    fromShimWit =
        unShimWit fromJMShimWit $ \ta conva ->
            mapShimWit (cfmap conva) $
            mkPJMShimWit $
            GroundPinaforeSingularType (EntityPinaforeGroundType (ConsListType Refl NilListType) ListEntityGroundType) $
            ConsDolanArguments ta NilDolanArguments

instance (FromShimWit JMShim (PinaforeType baseedit 'Negative) a) =>
             FromShimWit JMShim (PinaforeType baseedit 'Negative) [a] where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- PinaforeAction
instance (baseedit ~ edit, ToShimWit JMShim (PinaforeType edit 'Positive) a) =>
             ToShimWit JMShim (PinaforeSingularType baseedit 'Positive) (PinaforeAction edit a) where
    toShimWit =
        unShimWit toJMShimWit $ \ta conva ->
            mapShimWit (cfmap conva) $
            mkPJMShimWit $ GroundPinaforeSingularType ActionPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (baseedit ~ edit, ToShimWit JMShim (PinaforeType edit 'Positive) a) =>
             ToShimWit JMShim (PinaforeType baseedit 'Positive) (PinaforeAction edit a) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance (baseedit ~ edit, FromShimWit JMShim (PinaforeType edit 'Negative) a) =>
             FromShimWit JMShim (PinaforeSingularType baseedit 'Negative) (PinaforeAction edit a) where
    fromShimWit =
        unShimWit fromJMShimWit $ \ta conva ->
            mapShimWit (cfmap conva) $
            mkPJMShimWit $ GroundPinaforeSingularType ActionPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (baseedit ~ edit, FromShimWit JMShim (PinaforeType edit 'Negative) a) =>
             FromShimWit JMShim (PinaforeType baseedit 'Negative) (PinaforeAction edit a) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- IO
instance (ToShimWit JMShim (PinaforeType baseedit 'Positive) a) =>
             ToShimWit JMShim (PinaforeType baseedit 'Positive) (IO a) where
    toShimWit = mapShimWit (toEnhanced "subtype" (liftIO :: IO a -> PinaforeAction baseedit a)) toJMShimWit

-- PinaforeOrder
instance (baseedit ~ edit, FromShimWit JMShim (PinaforeType edit 'Negative) a) =>
             ToShimWit JMShim (PinaforeSingularType baseedit 'Positive) (PinaforeOrder edit a) where
    toShimWit =
        unShimWit fromJMShimWit $ \ta conva ->
            mapShimWit (consShimFunc ContravarianceType cid $ MkCatDual conva) $
            mkPJMShimWit $ GroundPinaforeSingularType OrderPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (baseedit ~ edit, FromShimWit JMShim (PinaforeType edit 'Negative) a) =>
             ToShimWit JMShim (PinaforeType baseedit 'Positive) (PinaforeOrder edit a) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance (baseedit ~ edit, ToShimWit JMShim (PinaforeType edit 'Positive) a) =>
             FromShimWit JMShim (PinaforeSingularType baseedit 'Negative) (PinaforeOrder edit a) where
    fromShimWit =
        unShimWit toJMShimWit $ \ta conva ->
            mapShimWit (consShimFunc ContravarianceType cid $ MkCatDual conva) $
            mkPJMShimWit $ GroundPinaforeSingularType OrderPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (baseedit ~ edit, ToShimWit JMShim (PinaforeType edit 'Positive) a) =>
             FromShimWit JMShim (PinaforeType baseedit 'Negative) (PinaforeOrder edit a) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- PinaforeUI
instance (baseedit ~ edit, ToShimWit JMShim (PinaforeType edit 'Positive) a) =>
             ToShimWit JMShim (PinaforeSingularType baseedit 'Positive) (PinaforeUI edit a) where
    toShimWit =
        unShimWit toJMShimWit $ \ta conva ->
            mapShimWit (consShimFunc CovarianceType cid conva) $
            mkPJMShimWit $
            GroundPinaforeSingularType UserInterfacePinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (baseedit ~ edit, ToShimWit JMShim (PinaforeType edit 'Positive) a) =>
             ToShimWit JMShim (PinaforeType baseedit 'Positive) (PinaforeUI edit a) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance (baseedit ~ edit, FromShimWit JMShim (PinaforeType edit 'Negative) a) =>
             FromShimWit JMShim (PinaforeSingularType baseedit 'Negative) (PinaforeUI edit a) where
    fromShimWit =
        unShimWit fromJMShimWit $ \ta conva ->
            mapShimWit (consShimFunc CovarianceType cid conva) $
            mkPJMShimWit $
            GroundPinaforeSingularType UserInterfacePinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (baseedit ~ edit, FromShimWit JMShim (PinaforeType edit 'Negative) a) =>
             FromShimWit JMShim (PinaforeType baseedit 'Negative) (PinaforeUI edit a) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- PinaforeWindow
instance ToShimWit JMShim (PinaforeSingularType baseedit 'Positive) PinaforeWindow where
    toShimWit = mkPJMShimWit $ GroundPinaforeSingularType WindowPinaforeGroundType NilDolanArguments

instance ToShimWit JMShim (PinaforeType baseedit 'Positive) PinaforeWindow where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance FromShimWit JMShim (PinaforeSingularType baseedit 'Negative) PinaforeWindow where
    fromShimWit = mkPJMShimWit $ GroundPinaforeSingularType WindowPinaforeGroundType NilDolanArguments

instance FromShimWit JMShim (PinaforeType baseedit 'Negative) PinaforeWindow where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- UISpec
instance (baseedit ~ edit, ToShimWit JMShim (PinaforeType edit 'Positive) a) =>
             ToShimWit JMShim (PinaforeType baseedit 'Positive) (UISpec a edit) where
    toShimWit =
        mapShimWit
            (coerceEnhanced "subtype")
            (toJMShimWit :: PinaforeShimWit baseedit 'Positive (PinaforeUI baseedit a))

instance (baseedit ~ edit, FromShimWit JMShim (PinaforeType edit 'Negative) a) =>
             FromShimWit JMShim (PinaforeType baseedit 'Negative) (UISpec a edit) where
    fromShimWit =
        mapShimWit
            (coerceEnhanced "subtype")
            (fromJMShimWit :: PinaforeShimWit baseedit 'Negative (PinaforeUI baseedit a))

-- MenuEntry
instance (baseedit ~ edit) => ToShimWit JMShim (PinaforeSingularType baseedit 'Positive) (MenuEntry edit) where
    toShimWit = mkPJMShimWit $ GroundPinaforeSingularType MenuItemPinaforeGroundType NilDolanArguments

instance (baseedit ~ edit) => ToShimWit JMShim (PinaforeType baseedit 'Positive) (MenuEntry edit) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance (baseedit ~ edit) => FromShimWit JMShim (PinaforeSingularType baseedit 'Negative) (MenuEntry edit) where
    fromShimWit = mkPJMShimWit $ GroundPinaforeSingularType MenuItemPinaforeGroundType NilDolanArguments

instance (baseedit ~ edit) => FromShimWit JMShim (PinaforeType baseedit 'Negative) (MenuEntry edit) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- PinaforeRef
instance ( baseedit ~ edit
         , FromShimWit JMShim (PinaforeType baseedit 'Negative) p
         , ToShimWit JMShim (PinaforeType baseedit 'Positive) q
         ) => ToShimWit JMShim (PinaforeSingularType baseedit 'Positive) (PinaforeRef edit '( p, q)) where
    toShimWit =
        unToRangeShimWit $ \tpq conv ->
            mapShimWit (consShimFunc RangevarianceType cid conv) $
            mkPJMShimWit $ GroundPinaforeSingularType RefPinaforeGroundType $ ConsDolanArguments tpq NilDolanArguments

instance ( baseedit ~ edit
         , FromShimWit JMShim (PinaforeType baseedit 'Negative) p
         , ToShimWit JMShim (PinaforeType baseedit 'Positive) q
         ) => ToShimWit JMShim (PinaforeType baseedit 'Positive) (PinaforeRef edit '( p, q)) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance ( baseedit ~ edit
         , ToShimWit JMShim (PinaforeType baseedit 'Positive) p
         , FromShimWit JMShim (PinaforeType baseedit 'Negative) q
         ) => FromShimWit JMShim (PinaforeSingularType baseedit 'Negative) (PinaforeRef edit '( p, q)) where
    fromShimWit =
        unFromRangeShimWit $ \tpq conv ->
            mapShimWit (consShimFunc RangevarianceType cid conv) $
            mkPJMShimWit $ GroundPinaforeSingularType RefPinaforeGroundType $ ConsDolanArguments tpq NilDolanArguments

instance ( baseedit ~ edit
         , ToShimWit JMShim (PinaforeType baseedit 'Positive) p
         , FromShimWit JMShim (PinaforeType baseedit 'Negative) q
         ) => FromShimWit JMShim (PinaforeType baseedit 'Negative) (PinaforeRef edit '( p, q)) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- PinaforeLensValue
instance ( baseedit ~ edit
         , FromShimWit JMShim (PinaforeType edit 'Negative) t
         , ToShimWit JMShim (PinaforeType edit 'Positive) t
         ) => ToShimWit JMShim (PinaforeType baseedit 'Positive) (PinaforeLensValue edit (WholeEdit (Know t))) where
    toShimWit = mapShimWit (toEnhanced "subtype" pinaforeLensToRef) toJMShimWit

instance ( baseedit ~ edit
         , FromShimWit JMShim (PinaforeType edit 'Negative) t
         , ToShimWit JMShim (PinaforeType edit 'Positive) t
         ) => FromShimWit JMShim (PinaforeType baseedit 'Negative) (PinaforeLensValue edit (WholeEdit (Know t))) where
    fromShimWit = mapShimWit (toEnhanced "subtype" pinaforeRefToLens) fromJMShimWit

-- PinaforeImmutableReference
instance (baseedit ~ edit, FromShimWit JMShim (PinaforeType edit 'Negative) a) =>
             FromShimWit JMShim (PinaforeType baseedit 'Negative) (PinaforeImmutableReference baseedit a) where
    fromShimWit = mapShimWit (toEnhanced "subtype" pinaforeRefToImmutable) fromJMShimWit

instance (baseedit ~ edit, ToShimWit JMShim (PinaforeType edit 'Positive) a) =>
             ToShimWit JMShim (PinaforeType baseedit 'Positive) (PinaforeImmutableReference baseedit a) where
    toShimWit = mapShimWit (toEnhanced "subtype" pinaforeImmutableToRef) toJMShimWit

-- PinaforeFunctionValue
instance (baseedit ~ edit, FromShimWit JMShim (PinaforeType edit 'Negative) t) =>
             FromShimWit JMShim (PinaforeType baseedit 'Negative) (PinaforeFunctionValue edit (Know t)) where
    fromShimWit = mapShimWit (toEnhanced "subtype" pinaforeRefToFunction) fromJMShimWit

instance (baseedit ~ edit, ToShimWit JMShim (PinaforeType edit 'Positive) t) =>
             ToShimWit JMShim (PinaforeType baseedit 'Positive) (PinaforeFunctionValue edit (Know t)) where
    toShimWit = mapShimWit (toEnhanced "subtype" pinaforeFunctionToRef) toJMShimWit

-- PinaforeSetRef
instance ( baseedit ~ edit
         , FromShimWit JMShim (PinaforeType baseedit 'Negative) p
         , ToShimWit JMShim (PinaforeType baseedit 'Positive) q
         ) => ToShimWit JMShim (PinaforeSingularType baseedit 'Positive) (PinaforeSetRef edit '( p, q)) where
    toShimWit =
        unToRangeShimWit $ \tpq conv ->
            mapShimWit (consShimFunc RangevarianceType cid conv) $
            mkPJMShimWit $
            GroundPinaforeSingularType SetRefPinaforeGroundType $ ConsDolanArguments tpq NilDolanArguments

instance ( baseedit ~ edit
         , FromShimWit JMShim (PinaforeType baseedit 'Negative) p
         , ToShimWit JMShim (PinaforeType baseedit 'Positive) q
         ) => ToShimWit JMShim (PinaforeType baseedit 'Positive) (PinaforeSetRef edit '( p, q)) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance ( baseedit ~ edit
         , ToShimWit JMShim (PinaforeType baseedit 'Positive) p
         , FromShimWit JMShim (PinaforeType baseedit 'Negative) q
         ) => FromShimWit JMShim (PinaforeSingularType baseedit 'Negative) (PinaforeSetRef edit '( p, q)) where
    fromShimWit =
        unFromRangeShimWit $ \tpq conv ->
            mapShimWit (consShimFunc RangevarianceType cid conv) $
            mkPJMShimWit $
            GroundPinaforeSingularType SetRefPinaforeGroundType $ ConsDolanArguments tpq NilDolanArguments

instance ( baseedit ~ edit
         , ToShimWit JMShim (PinaforeType baseedit 'Positive) p
         , FromShimWit JMShim (PinaforeType baseedit 'Negative) q
         ) => FromShimWit JMShim (PinaforeType baseedit 'Negative) (PinaforeSetRef edit '( p, q)) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- PinaforeLensValue FiniteSetEdit
instance ( baseedit ~ edit
         , ToShimWit JMShim (PinaforeType edit 'Positive) t
         , FromShimWit JMShim (PinaforeType baseedit 'Negative) t
         ) => FromShimWit JMShim (PinaforeType baseedit 'Negative) (PinaforeLensValue edit (FiniteSetEdit t)) where
    fromShimWit = mapShimWit (toEnhanced "subtype" unPinaforeSetRef) fromJMShimWit

instance ( baseedit ~ edit
         , Eq t
         , ToShimWit JMShim (PinaforeType baseedit 'Positive) t
         , FromShimWit JMShim (PinaforeType baseedit 'Negative) t
         ) => ToShimWit JMShim (PinaforeType baseedit 'Positive) (PinaforeLensValue edit (FiniteSetEdit t)) where
    toShimWit = mapShimWit (toEnhanced "subtype" $ MkPinaforeSetRef identityRange) toJMShimWit

-- PinaforeMorphism
instance ( baseedit ~ edit
         , FromShimWit JMShim (PinaforeType baseedit 'Negative) pa
         , ToShimWit JMShim (PinaforeType baseedit 'Positive) qa
         , FromShimWit JMShim (PinaforeType baseedit 'Negative) pb
         , ToShimWit JMShim (PinaforeType baseedit 'Positive) qb
         ) => ToShimWit JMShim (PinaforeSingularType baseedit 'Positive) (PinaforeMorphism edit '( pa, qa) '( pb, qb)) where
    toShimWit =
        unToRangeShimWit @_ @pa @qa $ \ta conva ->
            unToRangeShimWit @_ @pb @qb $ \tb convb ->
                mapShimWit (consShimFunc RangevarianceType (consShimFunc RangevarianceType cid conva) convb) $
                mkPJMShimWit $
                GroundPinaforeSingularType MorphismPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( baseedit ~ edit
         , FromShimWit JMShim (PinaforeType baseedit 'Negative) pa
         , ToShimWit JMShim (PinaforeType baseedit 'Positive) qa
         , FromShimWit JMShim (PinaforeType baseedit 'Negative) pb
         , ToShimWit JMShim (PinaforeType baseedit 'Positive) qb
         ) => ToShimWit JMShim (PinaforeType baseedit 'Positive) (PinaforeMorphism edit '( pa, qa) '( pb, qb)) where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance ( baseedit ~ edit
         , ToShimWit JMShim (PinaforeType baseedit 'Positive) pa
         , FromShimWit JMShim (PinaforeType baseedit 'Negative) qa
         , ToShimWit JMShim (PinaforeType baseedit 'Positive) pb
         , FromShimWit JMShim (PinaforeType baseedit 'Negative) qb
         ) => FromShimWit JMShim (PinaforeSingularType baseedit 'Negative) (PinaforeMorphism edit '( pa, qa) '( pb, qb)) where
    fromShimWit =
        unFromRangeShimWit $ \ta conva ->
            unFromRangeShimWit $ \tb convb ->
                mapShimWit (consShimFunc RangevarianceType (consShimFunc RangevarianceType cid conva) convb) $
                mkPJMShimWit $
                GroundPinaforeSingularType MorphismPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( baseedit ~ edit
         , ToShimWit JMShim (PinaforeType baseedit 'Positive) pa
         , FromShimWit JMShim (PinaforeType baseedit 'Negative) qa
         , ToShimWit JMShim (PinaforeType baseedit 'Positive) pb
         , FromShimWit JMShim (PinaforeType baseedit 'Negative) qb
         ) => FromShimWit JMShim (PinaforeType baseedit 'Negative) (PinaforeMorphism edit '( pa, qa) '( pb, qb)) where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- Entity
instance ToShimWit JMShim (PinaforeSingularType baseedit 'Positive) Entity where
    toShimWit =
        mkPJMShimWit $
        GroundPinaforeSingularType (EntityPinaforeGroundType NilListType TopEntityGroundType) NilDolanArguments

instance ToShimWit JMShim (PinaforeType baseedit 'Positive) Entity where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance FromShimWit JMShim (PinaforeSingularType baseedit 'Negative) Entity where
    fromShimWit =
        mkPJMShimWit $
        GroundPinaforeSingularType (EntityPinaforeGroundType NilListType TopEntityGroundType) NilDolanArguments

instance FromShimWit JMShim (PinaforeType baseedit 'Negative) Entity where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- NewEntity
instance ToShimWit JMShim (PinaforeSingularType baseedit 'Positive) NewEntity where
    toShimWit =
        mkPJMShimWit $
        GroundPinaforeSingularType (EntityPinaforeGroundType NilListType NewEntityGroundType) NilDolanArguments

instance ToShimWit JMShim (PinaforeType baseedit 'Positive) NewEntity where
    toShimWit = singlePinaforeShimWit toJMShimWit

instance FromShimWit JMShim (PinaforeSingularType baseedit 'Negative) NewEntity where
    fromShimWit =
        mkPJMShimWit $
        GroundPinaforeSingularType (EntityPinaforeGroundType NilListType NewEntityGroundType) NilDolanArguments

instance FromShimWit JMShim (PinaforeType baseedit 'Negative) NewEntity where
    fromShimWit = singlePinaforeShimWit fromJMShimWit

-- Literal
literalInstances :: _ -> _
literalInstances t =
    [d|
  
  instance ToShimWit JMShim (PinaforeSingularType baseedit 'Positive)
             $( t )
           where
          toShimWit
            = mkPJMShimWit $
                GroundPinaforeSingularType
                  (EntityPinaforeGroundType NilListType $
                     LiteralEntityGroundType representative)
                  NilDolanArguments
  
  instance ToShimWit JMShim (PinaforeType baseedit 'Positive) $( t )
           where
          toShimWit = singlePinaforeShimWit toJMShimWit
  
  instance FromShimWit JMShim
             (PinaforeSingularType baseedit 'Negative)
             $( t )
           where
          fromShimWit
            = mkPJMShimWit $
                GroundPinaforeSingularType
                  (EntityPinaforeGroundType NilListType $
                     LiteralEntityGroundType representative)
                  NilDolanArguments
  
  instance FromShimWit JMShim (PinaforeType baseedit 'Negative)
             $( t )
           where
          fromShimWit = singlePinaforeShimWit fromJMShimWit
  |]
