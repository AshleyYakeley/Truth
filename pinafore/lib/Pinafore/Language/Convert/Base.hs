{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Convert.Base
    ( qTypeDescription
    , ToTypeF(..)
    , FromTypeF(..)
    , toValue
    , ToPinaforeType
    , FromPinaforeType
    , literalInstances
    ) where

import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.Morphism
import Pinafore.Language.NamedEntity
import Pinafore.Language.Order
import Pinafore.Language.Reference
import Pinafore.Language.Set
import Pinafore.Language.Type
import Pinafore.Language.UI
import Shapes
import Truth.Core

qTypeDescription ::
       forall baseedit t. ToPinaforeType baseedit t
    => Text
qTypeDescription =
    case toTypeF @(PinaforeType baseedit 'Positive) @t of
        MkTypeF w _ -> pack $ show w

type ToPinaforeType baseedit = ToTypeF (PinaforeType baseedit 'Positive)

type FromPinaforeType baseedit = FromTypeF (PinaforeType baseedit 'Negative)

-- top, bottom, join, meet
instance ToTypeF (PinaforeType baseedit 'Positive) BottomType where
    toTypeF = mkPTypeF NilPinaforeType

instance FromTypeF (PinaforeType baseedit 'Negative) TopType where
    fromTypeF = mkPTypeF NilPinaforeType

instance (ToTypeF (PinaforeType baseedit 'Positive) a, ToTypeF (PinaforeType baseedit 'Positive) b) =>
             ToTypeF (PinaforeType baseedit 'Positive) (JoinType a b) where
    toTypeF = joinPinaforeTypeF toTypeF toTypeF

instance (FromTypeF (PinaforeType baseedit 'Negative) a, FromTypeF (PinaforeType baseedit 'Negative) b) =>
             FromTypeF (PinaforeType baseedit 'Negative) (MeetType a b) where
    fromTypeF = meetPinaforeTypeF fromTypeF fromTypeF

-- UVar
instance KnownSymbol name => ToTypeF (PinaforeSingularType baseedit 'Positive) (UVar name) where
    toTypeF = mkPTypeF $ VarPinaforeSingularType MkSymbolType

instance KnownSymbol name => ToTypeF (PinaforeType baseedit 'Positive) (UVar name) where
    toTypeF = singlePinaforeTypeF toTypeF

instance KnownSymbol name => FromTypeF (PinaforeSingularType baseedit 'Negative) (UVar name) where
    fromTypeF = mkPTypeF $ VarPinaforeSingularType MkSymbolType

instance KnownSymbol name => FromTypeF (PinaforeType baseedit 'Negative) (UVar name) where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- (,)
instance (ToTypeF (PinaforeType baseedit 'Positive) a, ToTypeF (PinaforeType baseedit 'Positive) b) =>
             ToTypeF (PinaforeSingularType baseedit 'Positive) (a, b) where
    toTypeF =
        unTypeF toTypeF $ \ta conva ->
            unTypeF toTypeF $ \tb convb ->
                contramap (\(a, b) -> (conva a, convb b)) $
                mkPTypeF $
                GroundPinaforeSingularType
                    (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) PairEntityGroundType) $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance (ToTypeF (PinaforeType baseedit 'Positive) a, ToTypeF (PinaforeType baseedit 'Positive) b) =>
             ToTypeF (PinaforeType baseedit 'Positive) (a, b) where
    toTypeF = singlePinaforeTypeF toTypeF

instance (FromTypeF (PinaforeType baseedit 'Negative) a, FromTypeF (PinaforeType baseedit 'Negative) b) =>
             FromTypeF (PinaforeSingularType baseedit 'Negative) (a, b) where
    fromTypeF =
        unTypeF fromTypeF $ \ta conva ->
            unTypeF fromTypeF $ \tb convb ->
                fmap (\(a, b) -> (conva a, convb b)) $
                mkPTypeF $
                GroundPinaforeSingularType
                    (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) PairEntityGroundType) $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance (FromTypeF (PinaforeType baseedit 'Negative) a, FromTypeF (PinaforeType baseedit 'Negative) b) =>
             FromTypeF (PinaforeType baseedit 'Negative) (Either a b) where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- Either
instance (ToTypeF (PinaforeType baseedit 'Positive) a, ToTypeF (PinaforeType baseedit 'Positive) b) =>
             ToTypeF (PinaforeSingularType baseedit 'Positive) (Either a b) where
    toTypeF =
        unTypeF toTypeF $ \ta conva ->
            unTypeF toTypeF $ \tb convb ->
                contramap (either (Left . conva) (Right . convb)) $
                mkPTypeF $
                GroundPinaforeSingularType
                    (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) EitherEntityGroundType) $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance (ToTypeF (PinaforeType baseedit 'Positive) a, ToTypeF (PinaforeType baseedit 'Positive) b) =>
             ToTypeF (PinaforeType baseedit 'Positive) (Either a b) where
    toTypeF = singlePinaforeTypeF toTypeF

instance (FromTypeF (PinaforeType baseedit 'Negative) a, FromTypeF (PinaforeType baseedit 'Negative) b) =>
             FromTypeF (PinaforeSingularType baseedit 'Negative) (Either a b) where
    fromTypeF =
        unTypeF fromTypeF $ \ta conva ->
            unTypeF fromTypeF $ \tb convb ->
                fmap (either (Left . conva) (Right . convb)) $
                mkPTypeF $
                GroundPinaforeSingularType
                    (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) EitherEntityGroundType) $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance (FromTypeF (PinaforeType baseedit 'Negative) a, FromTypeF (PinaforeType baseedit 'Negative) b) =>
             FromTypeF (PinaforeType baseedit 'Negative) (a, b) where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- (->)
instance (FromTypeF (PinaforeType baseedit 'Negative) a, ToTypeF (PinaforeType baseedit 'Positive) b) =>
             ToTypeF (PinaforeSingularType baseedit 'Positive) (a -> b) where
    toTypeF =
        unTypeF fromTypeF $ \ta conva ->
            unTypeF toTypeF $ \tb convb ->
                contramap (\ab -> convb . ab . conva) $
                mkPTypeF $
                GroundPinaforeSingularType FuncPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance (FromTypeF (PinaforeType baseedit 'Negative) a, ToTypeF (PinaforeType baseedit 'Positive) b) =>
             ToTypeF (PinaforeType baseedit 'Positive) (a -> b) where
    toTypeF = singlePinaforeTypeF toTypeF

instance (ToTypeF (PinaforeType baseedit 'Positive) a, FromTypeF (PinaforeType baseedit 'Negative) b) =>
             FromTypeF (PinaforeSingularType baseedit 'Negative) (a -> b) where
    fromTypeF =
        unTypeF toTypeF $ \ta conva ->
            unTypeF fromTypeF $ \tb convb ->
                fmap (\ab -> convb . ab . conva) $
                mkPTypeF $
                GroundPinaforeSingularType FuncPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance (ToTypeF (PinaforeType baseedit 'Positive) a, FromTypeF (PinaforeType baseedit 'Negative) b) =>
             FromTypeF (PinaforeType baseedit 'Negative) (a -> b) where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- Maybe
instance (ToTypeF (PinaforeType baseedit 'Positive) a) => ToTypeF (PinaforeSingularType baseedit 'Positive) (Maybe a) where
    toTypeF =
        unTypeF toTypeF $ \ta conva ->
            contramap (fmap conva) $
            mkPTypeF $
            GroundPinaforeSingularType (EntityPinaforeGroundType (ConsListType Refl NilListType) MaybeEntityGroundType) $
            ConsDolanArguments ta NilDolanArguments

instance (ToTypeF (PinaforeType baseedit 'Positive) a) => ToTypeF (PinaforeType baseedit 'Positive) (Maybe a) where
    toTypeF = singlePinaforeTypeF toTypeF

instance (FromTypeF (PinaforeType baseedit 'Negative) a) =>
             FromTypeF (PinaforeSingularType baseedit 'Negative) (Maybe a) where
    fromTypeF =
        unTypeF fromTypeF $ \ta conva ->
            fmap (fmap conva) $
            mkPTypeF $
            GroundPinaforeSingularType (EntityPinaforeGroundType (ConsListType Refl NilListType) MaybeEntityGroundType) $
            ConsDolanArguments ta NilDolanArguments

instance (FromTypeF (PinaforeType baseedit 'Negative) a) => FromTypeF (PinaforeType baseedit 'Negative) (Maybe a) where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- []
instance (ToTypeF (PinaforeType baseedit 'Positive) a) => ToTypeF (PinaforeSingularType baseedit 'Positive) [a] where
    toTypeF =
        unTypeF toTypeF $ \ta conva ->
            contramap (fmap conva) $
            mkPTypeF $
            GroundPinaforeSingularType (EntityPinaforeGroundType (ConsListType Refl NilListType) ListEntityGroundType) $
            ConsDolanArguments ta NilDolanArguments

instance (ToTypeF (PinaforeType baseedit 'Positive) a) => ToTypeF (PinaforeType baseedit 'Positive) [a] where
    toTypeF = singlePinaforeTypeF toTypeF

instance (FromTypeF (PinaforeType baseedit 'Negative) a) => FromTypeF (PinaforeSingularType baseedit 'Negative) [a] where
    fromTypeF =
        unTypeF fromTypeF $ \ta conva ->
            fmap (fmap conva) $
            mkPTypeF $
            GroundPinaforeSingularType (EntityPinaforeGroundType (ConsListType Refl NilListType) ListEntityGroundType) $
            ConsDolanArguments ta NilDolanArguments

instance (FromTypeF (PinaforeType baseedit 'Negative) a) => FromTypeF (PinaforeType baseedit 'Negative) [a] where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- PinaforeAction
instance (baseedit ~ edit, ToTypeF (PinaforeType edit 'Positive) a) =>
             ToTypeF (PinaforeSingularType baseedit 'Positive) (PinaforeAction edit a) where
    toTypeF =
        unTypeF toTypeF $ \ta conv ->
            contramap (fmap conv) $
            mkPTypeF $ GroundPinaforeSingularType ActionPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (baseedit ~ edit, ToTypeF (PinaforeType edit 'Positive) a) =>
             ToTypeF (PinaforeType baseedit 'Positive) (PinaforeAction edit a) where
    toTypeF = singlePinaforeTypeF toTypeF

instance (baseedit ~ edit, FromTypeF (PinaforeType edit 'Negative) a) =>
             FromTypeF (PinaforeSingularType baseedit 'Negative) (PinaforeAction edit a) where
    fromTypeF =
        unTypeF fromTypeF $ \ta conv ->
            fmap (fmap conv) $
            mkPTypeF $ GroundPinaforeSingularType ActionPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (baseedit ~ edit, FromTypeF (PinaforeType edit 'Negative) a) =>
             FromTypeF (PinaforeType baseedit 'Negative) (PinaforeAction edit a) where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- IO
instance (ToTypeF (PinaforeType baseedit 'Positive) a) => ToTypeF (PinaforeType baseedit 'Positive) (IO a) where
    toTypeF = contramap (liftIO :: IO a -> PinaforeAction baseedit a) toTypeF

-- PinaforeOrder
instance (baseedit ~ edit, FromTypeF (PinaforeType edit 'Negative) a) =>
             ToTypeF (PinaforeSingularType baseedit 'Positive) (PinaforeOrder edit a) where
    toTypeF =
        unTypeF fromTypeF $ \ta conv ->
            contramap (contramap conv) $
            mkPTypeF $ GroundPinaforeSingularType OrderPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (baseedit ~ edit, FromTypeF (PinaforeType edit 'Negative) a) =>
             ToTypeF (PinaforeType baseedit 'Positive) (PinaforeOrder edit a) where
    toTypeF = singlePinaforeTypeF toTypeF

instance (baseedit ~ edit, ToTypeF (PinaforeType edit 'Positive) a) =>
             FromTypeF (PinaforeSingularType baseedit 'Negative) (PinaforeOrder edit a) where
    fromTypeF =
        unTypeF toTypeF $ \ta conv ->
            fmap (contramap conv) $
            mkPTypeF $ GroundPinaforeSingularType OrderPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (baseedit ~ edit, ToTypeF (PinaforeType edit 'Positive) a) =>
             FromTypeF (PinaforeType baseedit 'Negative) (PinaforeOrder edit a) where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- PinaforeUI
instance (baseedit ~ edit, ToTypeF (PinaforeType edit 'Positive) a) =>
             ToTypeF (PinaforeSingularType baseedit 'Positive) (PinaforeUI edit a) where
    toTypeF =
        unTypeF toTypeF $ \ta conv ->
            contramap (fmap conv) $
            mkPTypeF $
            GroundPinaforeSingularType UserInterfacePinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (baseedit ~ edit, ToTypeF (PinaforeType edit 'Positive) a) =>
             ToTypeF (PinaforeType baseedit 'Positive) (PinaforeUI edit a) where
    toTypeF = singlePinaforeTypeF toTypeF

instance (baseedit ~ edit, FromTypeF (PinaforeType edit 'Negative) a) =>
             FromTypeF (PinaforeSingularType baseedit 'Negative) (PinaforeUI edit a) where
    fromTypeF =
        unTypeF fromTypeF $ \ta conv ->
            fmap (fmap conv) $
            mkPTypeF $
            GroundPinaforeSingularType UserInterfacePinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (baseedit ~ edit, FromTypeF (PinaforeType edit 'Negative) a) =>
             FromTypeF (PinaforeType baseedit 'Negative) (PinaforeUI edit a) where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- UIWindow
instance ToTypeF (PinaforeSingularType baseedit 'Positive) UIWindow where
    toTypeF = mkPTypeF $ GroundPinaforeSingularType WindowPinaforeGroundType NilDolanArguments

instance ToTypeF (PinaforeType baseedit 'Positive) UIWindow where
    toTypeF = singlePinaforeTypeF toTypeF

instance FromTypeF (PinaforeSingularType baseedit 'Negative) UIWindow where
    fromTypeF = mkPTypeF $ GroundPinaforeSingularType WindowPinaforeGroundType NilDolanArguments

instance FromTypeF (PinaforeType baseedit 'Negative) UIWindow where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- UISpec
instance (baseedit ~ edit, ToTypeF (PinaforeType edit 'Positive) a) =>
             ToTypeF (PinaforeType baseedit 'Positive) (UISpec a edit) where
    toTypeF = contramap MkPinaforeUI toTypeF

instance (baseedit ~ edit, FromTypeF (PinaforeType edit 'Negative) a) =>
             FromTypeF (PinaforeType baseedit 'Negative) (UISpec a edit) where
    fromTypeF = fmap unPinaforeUI fromTypeF

-- PinaforeReference
instance (baseedit ~ edit, FromTypeF (PinaforeType baseedit 'Negative) p, ToTypeF (PinaforeType baseedit 'Positive) q) =>
             ToTypeF (PinaforeSingularType baseedit 'Positive) (PinaforeReference edit '( p, q)) where
    toTypeF =
        unToWithTypeF $ \tpq conv ->
            contramap (mapRange conv) $
            mkPTypeF $ GroundPinaforeSingularType ReferencePinaforeGroundType $ ConsDolanArguments tpq NilDolanArguments

instance (baseedit ~ edit, FromTypeF (PinaforeType baseedit 'Negative) p, ToTypeF (PinaforeType baseedit 'Positive) q) =>
             ToTypeF (PinaforeType baseedit 'Positive) (PinaforeReference edit '( p, q)) where
    toTypeF = singlePinaforeTypeF toTypeF

instance (baseedit ~ edit, ToTypeF (PinaforeType baseedit 'Positive) p, FromTypeF (PinaforeType baseedit 'Negative) q) =>
             FromTypeF (PinaforeSingularType baseedit 'Negative) (PinaforeReference edit '( p, q)) where
    fromTypeF =
        unFromWithTypeF $ \tpq conv ->
            fmap (mapRange conv) $
            mkPTypeF $ GroundPinaforeSingularType ReferencePinaforeGroundType $ ConsDolanArguments tpq NilDolanArguments

instance (baseedit ~ edit, ToTypeF (PinaforeType baseedit 'Positive) p, FromTypeF (PinaforeType baseedit 'Negative) q) =>
             FromTypeF (PinaforeType baseedit 'Negative) (PinaforeReference edit '( p, q)) where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- PinaforeLensValue
instance (baseedit ~ edit, FromTypeF (PinaforeType edit 'Negative) t, ToTypeF (PinaforeType edit 'Positive) t) =>
             ToTypeF (PinaforeType baseedit 'Positive) (PinaforeLensValue edit (WholeEdit (Know t))) where
    toTypeF = contramap pinaforeLensToReference toTypeF

instance (baseedit ~ edit, FromTypeF (PinaforeType edit 'Negative) t, ToTypeF (PinaforeType edit 'Positive) t) =>
             FromTypeF (PinaforeType baseedit 'Negative) (PinaforeLensValue edit (WholeEdit (Know t))) where
    fromTypeF = fmap pinaforeReferenceToLens fromTypeF

-- PinaforeImmutableReference
instance (baseedit ~ edit, FromTypeF (PinaforeType edit 'Negative) a) =>
             FromTypeF (PinaforeType baseedit 'Negative) (PinaforeImmutableReference baseedit a) where
    fromTypeF = fmap pinaforeReferenceToImmutable fromTypeF

instance (baseedit ~ edit, ToTypeF (PinaforeType edit 'Positive) a) =>
             ToTypeF (PinaforeType baseedit 'Positive) (PinaforeImmutableReference baseedit a) where
    toTypeF = contramap pinaforeImmutableToReference toTypeF

-- PinaforeFunctionValue
instance (baseedit ~ edit, FromTypeF (PinaforeType edit 'Negative) t) =>
             FromTypeF (PinaforeType baseedit 'Negative) (PinaforeFunctionValue edit (Know t)) where
    fromTypeF = fmap pinaforeReferenceToFunction fromTypeF

instance (baseedit ~ edit, ToTypeF (PinaforeType edit 'Positive) t) =>
             ToTypeF (PinaforeType baseedit 'Positive) (PinaforeFunctionValue edit (Know t)) where
    toTypeF = contramap pinaforeFunctionToReference toTypeF

-- PinaforeSet
instance (baseedit ~ edit, FromTypeF (PinaforeType baseedit 'Negative) p, ToTypeF (PinaforeType baseedit 'Positive) q) =>
             ToTypeF (PinaforeSingularType baseedit 'Positive) (PinaforeSet edit '( p, q)) where
    toTypeF =
        unToWithTypeF $ \tpq conv ->
            contramap (mapRange conv) $
            mkPTypeF $ GroundPinaforeSingularType SetPinaforeGroundType $ ConsDolanArguments tpq NilDolanArguments

instance (baseedit ~ edit, FromTypeF (PinaforeType baseedit 'Negative) p, ToTypeF (PinaforeType baseedit 'Positive) q) =>
             ToTypeF (PinaforeType baseedit 'Positive) (PinaforeSet edit '( p, q)) where
    toTypeF = singlePinaforeTypeF toTypeF

instance (baseedit ~ edit, ToTypeF (PinaforeType baseedit 'Positive) p, FromTypeF (PinaforeType baseedit 'Negative) q) =>
             FromTypeF (PinaforeSingularType baseedit 'Negative) (PinaforeSet edit '( p, q)) where
    fromTypeF =
        unFromWithTypeF $ \tpq conv ->
            fmap (mapRange conv) $
            mkPTypeF $ GroundPinaforeSingularType SetPinaforeGroundType $ ConsDolanArguments tpq NilDolanArguments

instance (baseedit ~ edit, ToTypeF (PinaforeType baseedit 'Positive) p, FromTypeF (PinaforeType baseedit 'Negative) q) =>
             FromTypeF (PinaforeType baseedit 'Negative) (PinaforeSet edit '( p, q)) where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- PinaforeLensValue FiniteSetEdit
instance (baseedit ~ edit, ToTypeF (PinaforeType edit 'Positive) t, FromTypeF (PinaforeType baseedit 'Negative) t) =>
             FromTypeF (PinaforeType baseedit 'Negative) (PinaforeLensValue edit (FiniteSetEdit t)) where
    fromTypeF = fmap unPinaforeSet fromTypeF

instance ( baseedit ~ edit
         , Eq t
         , ToTypeF (PinaforeType baseedit 'Positive) t
         , FromTypeF (PinaforeType baseedit 'Negative) t
         ) => ToTypeF (PinaforeType baseedit 'Positive) (PinaforeLensValue edit (FiniteSetEdit t)) where
    toTypeF = contramap (MkPinaforeSet identityRange) toTypeF

-- PinaforeMorphism
instance ( baseedit ~ edit
         , FromTypeF (PinaforeType baseedit 'Negative) pa
         , ToTypeF (PinaforeType baseedit 'Positive) qa
         , FromTypeF (PinaforeType baseedit 'Negative) pb
         , ToTypeF (PinaforeType baseedit 'Positive) qb
         ) => ToTypeF (PinaforeSingularType baseedit 'Positive) (PinaforeMorphism edit '( pa, qa) '( pb, qb)) where
    toTypeF =
        unToWithTypeF $ \ta conva ->
            unToWithTypeF $ \tb convb ->
                contramap (mapRange' conva . mapRange convb) $
                mkPTypeF $
                GroundPinaforeSingularType MorphismPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( baseedit ~ edit
         , FromTypeF (PinaforeType baseedit 'Negative) pa
         , ToTypeF (PinaforeType baseedit 'Positive) qa
         , FromTypeF (PinaforeType baseedit 'Negative) pb
         , ToTypeF (PinaforeType baseedit 'Positive) qb
         ) => ToTypeF (PinaforeType baseedit 'Positive) (PinaforeMorphism edit '( pa, qa) '( pb, qb)) where
    toTypeF = singlePinaforeTypeF toTypeF

instance ( baseedit ~ edit
         , ToTypeF (PinaforeType baseedit 'Positive) pa
         , FromTypeF (PinaforeType baseedit 'Negative) qa
         , ToTypeF (PinaforeType baseedit 'Positive) pb
         , FromTypeF (PinaforeType baseedit 'Negative) qb
         ) => FromTypeF (PinaforeSingularType baseedit 'Negative) (PinaforeMorphism edit '( pa, qa) '( pb, qb)) where
    fromTypeF =
        unFromWithTypeF $ \ta conva ->
            unFromWithTypeF $ \tb convb ->
                fmap (mapRange' conva . mapRange convb) $
                mkPTypeF $
                GroundPinaforeSingularType MorphismPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( baseedit ~ edit
         , ToTypeF (PinaforeType baseedit 'Positive) pa
         , FromTypeF (PinaforeType baseedit 'Negative) qa
         , ToTypeF (PinaforeType baseedit 'Positive) pb
         , FromTypeF (PinaforeType baseedit 'Negative) qb
         ) => FromTypeF (PinaforeType baseedit 'Negative) (PinaforeMorphism edit '( pa, qa) '( pb, qb)) where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- Entity
instance ToTypeF (PinaforeSingularType baseedit 'Positive) Entity where
    toTypeF =
        mkPTypeF $
        GroundPinaforeSingularType (EntityPinaforeGroundType NilListType TopEntityGroundType) NilDolanArguments

instance ToTypeF (PinaforeType baseedit 'Positive) Entity where
    toTypeF = singlePinaforeTypeF toTypeF

instance FromTypeF (PinaforeSingularType baseedit 'Negative) Entity where
    fromTypeF =
        mkPTypeF $
        GroundPinaforeSingularType (EntityPinaforeGroundType NilListType TopEntityGroundType) NilDolanArguments

instance FromTypeF (PinaforeType baseedit 'Negative) Entity where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- NamedEntity
instance KnownSymbol name => ToTypeF (PinaforeSingularType baseedit 'Positive) (NamedEntity name) where
    toTypeF =
        mkPTypeF $
        GroundPinaforeSingularType
            (EntityPinaforeGroundType NilListType $ NamedEntityGroundType MkSymbolType)
            NilDolanArguments

instance KnownSymbol name => ToTypeF (PinaforeType baseedit 'Positive) (NamedEntity name) where
    toTypeF = singlePinaforeTypeF toTypeF

instance KnownSymbol name => FromTypeF (PinaforeSingularType baseedit 'Negative) (NamedEntity name) where
    fromTypeF =
        mkPTypeF $
        GroundPinaforeSingularType
            (EntityPinaforeGroundType NilListType $ NamedEntityGroundType MkSymbolType)
            NilDolanArguments

instance KnownSymbol name => FromTypeF (PinaforeType baseedit 'Negative) (NamedEntity name) where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- NewEntity
instance ToTypeF (PinaforeSingularType baseedit 'Positive) NewEntity where
    toTypeF =
        mkPTypeF $
        GroundPinaforeSingularType (EntityPinaforeGroundType NilListType NewEntityGroundType) NilDolanArguments

instance ToTypeF (PinaforeType baseedit 'Positive) NewEntity where
    toTypeF = singlePinaforeTypeF toTypeF

instance FromTypeF (PinaforeSingularType baseedit 'Negative) NewEntity where
    fromTypeF =
        mkPTypeF $
        GroundPinaforeSingularType (EntityPinaforeGroundType NilListType NewEntityGroundType) NilDolanArguments

instance FromTypeF (PinaforeType baseedit 'Negative) NewEntity where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- Literal
literalInstances :: _ -> _
literalInstances t =
    [d|
  
  instance ToTypeF (PinaforeSingularType baseedit 'Positive) $( t )
           where
          toTypeF
            = mkPTypeF $
                GroundPinaforeSingularType
                  (EntityPinaforeGroundType NilListType $
                     LiteralEntityGroundType representative)
                  NilDolanArguments
  
  instance ToTypeF (PinaforeType baseedit 'Positive) $( t ) where
          toTypeF = singlePinaforeTypeF toTypeF
  
  instance FromTypeF (PinaforeSingularType baseedit 'Negative) $( t )
           where
          fromTypeF
            = mkPTypeF $
                GroundPinaforeSingularType
                  (EntityPinaforeGroundType NilListType $
                     LiteralEntityGroundType representative)
                  NilDolanArguments
  
  instance FromTypeF (PinaforeType baseedit 'Negative) $( t ) where
          fromTypeF = singlePinaforeTypeF fromTypeF
  |]
