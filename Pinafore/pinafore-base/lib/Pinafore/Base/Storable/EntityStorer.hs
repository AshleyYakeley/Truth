module Pinafore.Base.Storable.EntityStorer
    ( Predicate(..)
    , FieldStorer(..)
    , ConstructorStorer(..)
    , EntityStorer(..)
    , gateEntityStorer
    , StorerMode(..)
    , entityStorerToEntity
    ) where

import Pinafore.Base.Anchor
import Pinafore.Base.Entity
import Pinafore.Base.KnowShim
import Pinafore.Base.Literal.Literal
import Shapes

newtype Predicate =
    MkPredicate Anchor
    deriving newtype (Eq, Ord)

instance Show Predicate where
    show (MkPredicate anchor) = show anchor

type FieldStorer :: StorerMode -> Type -> Type
data FieldStorer mode t where
    MkFieldStorer :: Predicate -> EntityStorer mode t -> FieldStorer mode t

instance TestEquality (FieldStorer 'SingleMode) where
    testEquality (MkFieldStorer p1 d1) (MkFieldStorer p2 d2)
        | p1 == p2
        , Just Refl <- testEquality d1 d2 = Just Refl
    testEquality _ _ = Nothing

data StorerMode
    = SingleMode
    | MultipleMode

instance WitnessConstraint Show (FieldStorer 'SingleMode) where
    witnessConstraint (MkFieldStorer _ t) = witnessConstraint t

type ConstructorStorer :: StorerMode -> Type -> Type
data ConstructorStorer mode t where
    PlainConstructorStorer :: ConstructorStorer mode Entity
    LiteralConstructorStorer :: ConstructorStorer mode Literal
    ConstructorConstructorStorer
        :: forall mode (tt :: [Type]).
           Anchor
        -> ListType (FieldStorer mode) tt
        -> ConstructorStorer mode (ListProduct tt)

instance TestEquality (ConstructorStorer 'SingleMode) where
    testEquality PlainConstructorStorer PlainConstructorStorer = Just Refl
    testEquality LiteralConstructorStorer LiteralConstructorStorer = Just Refl
    testEquality (ConstructorConstructorStorer a1 t1) (ConstructorConstructorStorer a2 t2)
        | a1 == a2 = do
            Refl <- testEquality t1 t2
            return Refl
    testEquality _ _ = Nothing

instance WitnessConstraint Show (ConstructorStorer 'SingleMode) where
    witnessConstraint PlainConstructorStorer = Dict
    witnessConstraint LiteralConstructorStorer = Dict
    witnessConstraint (ConstructorConstructorStorer _ t) =
        case listProductShow witnessConstraint t of
            Dict -> Dict

constructorStorerToEntity :: forall t. ConstructorStorer 'SingleMode t -> t -> Entity
constructorStorerToEntity PlainConstructorStorer t = t
constructorStorerToEntity LiteralConstructorStorer l = literalToEntity l
constructorStorerToEntity (ConstructorConstructorStorer anchor facts) hl =
    hashToEntity $ \call -> call anchor : hashList call facts hl
  where
    hashList ::
           forall tt r.
           (forall a. HasSerializer a => a -> r)
        -> ListType (FieldStorer 'SingleMode) tt
        -> ListProduct tt
        -> [r]
    hashList _call NilListType () = []
    hashList call (ConsListType (MkFieldStorer _ def) lt) (a, l) =
        call (entityStorerToEntity def a) : hashList call lt l

type EntityStorer' :: StorerMode -> Type -> Type
type family EntityStorer' mode t where
    EntityStorer' 'SingleMode t = ConstructorStorer 'SingleMode t
    EntityStorer' 'MultipleMode t = [KnowShim (ConstructorStorer 'MultipleMode) t]

type EntityStorer :: StorerMode -> Type -> Type
newtype EntityStorer mode t =
    MkEntityStorer (EntityStorer' mode t)

instance TestEquality (EntityStorer 'SingleMode) where
    testEquality (MkEntityStorer fca) (MkEntityStorer fcb) = testEquality fca fcb

instance WitnessConstraint Show (EntityStorer 'SingleMode) where
    witnessConstraint (MkEntityStorer fc) = witnessConstraint fc

entityStorerToEntity :: forall t. EntityStorer 'SingleMode t -> t -> Entity
entityStorerToEntity (MkEntityStorer fc) = constructorStorerToEntity fc

instance Functor (EntityStorer 'MultipleMode) where
    fmap ab (MkEntityStorer kss) = MkEntityStorer $ fmap (fmap ab) kss

instance Semigroup (EntityStorer 'MultipleMode t) where
    MkEntityStorer kssa <> MkEntityStorer kssb = MkEntityStorer $ kssa <> kssb

instance Monoid (EntityStorer 'MultipleMode t) where
    mempty = MkEntityStorer mempty

instance Invariant (EntityStorer 'MultipleMode) where
    invmap ab _ = fmap ab

instance Summable (EntityStorer 'MultipleMode) where
    rVoid = mempty
    esa <+++> esb = fmap Left esa <> fmap Right esb

gateEntityStorer :: (t -> Bool) -> EntityStorer 'MultipleMode t -> EntityStorer 'MultipleMode t
gateEntityStorer prd (MkEntityStorer kss) = MkEntityStorer $ fmap (gateKnowShim prd) kss
