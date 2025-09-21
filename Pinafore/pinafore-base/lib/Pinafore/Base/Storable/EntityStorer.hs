module Pinafore.Base.Storable.EntityStorer
    ( Predicate (..)
    , FieldStorer (..)
    , ConstructorStorer (..)
    , MultipleEntityStorer (..)
    , SingleEntityStorer (..)
    , StorerMode (..)
    , entityStorerToEntity
    )
where

import Shapes

import Pinafore.Base.Anchor
import Pinafore.Base.Entity
import Pinafore.Base.KnowShim
import Pinafore.Base.Literal.Literal

newtype Predicate
    = MkPredicate Anchor
    deriving newtype (Eq, Ord)

instance Show Predicate where
    show (MkPredicate anchor) = show anchor

type FieldStorer :: StorerMode -> Type -> Type
data FieldStorer mode t where
    MkFieldStorer :: Predicate -> EntityStorer mode t -> FieldStorer mode t

instance TestEquality (FieldStorer 'SingleMode) where
    testEquality (MkFieldStorer p1 d1) (MkFieldStorer p2 d2)
        | p1 == p2
        , Just Refl <- testEquality d1 d2 =
            Just Refl
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
    ConstructorConstructorStorer ::
        forall mode (tt :: [Type]).
        Anchor ->
        ListType (FieldStorer mode) tt ->
        ConstructorStorer mode (ListProduct tt)

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
            (forall a. HasSerializer a => a -> r) ->
            ListType (FieldStorer 'SingleMode) tt ->
            ListProduct tt ->
            [r]
        hashList _call NilListType () = []
        hashList call (ConsListType (MkFieldStorer _ def) lt) (a, l) =
            call (entityStorerToEntity def a) : hashList call lt l

type EntityStorer :: StorerMode -> Type -> Type
type family EntityStorer mode where
    EntityStorer 'SingleMode = SingleEntityStorer
    EntityStorer 'MultipleMode = MultipleEntityStorer

newtype SingleEntityStorer t = MkSingleEntityStorer (ConstructorStorer 'SingleMode t)

instance TestEquality SingleEntityStorer where
    testEquality (MkSingleEntityStorer fca) (MkSingleEntityStorer fcb) = testEquality fca fcb

instance WitnessConstraint Show SingleEntityStorer where
    witnessConstraint (MkSingleEntityStorer fc) = witnessConstraint fc

entityStorerToEntity :: forall t. SingleEntityStorer t -> t -> Entity
entityStorerToEntity (MkSingleEntityStorer fc) = constructorStorerToEntity fc

type role MultipleEntityStorer representational

newtype MultipleEntityStorer t = MkMultipleEntityStorer [KnowShim (ConstructorStorer 'MultipleMode) t]

instance Functor MultipleEntityStorer where
    fmap ab (MkMultipleEntityStorer kss) = MkMultipleEntityStorer $ fmap (fmap ab) kss

instance Semigroup (MultipleEntityStorer t) where
    MkMultipleEntityStorer kssa <> MkMultipleEntityStorer kssb = MkMultipleEntityStorer $ kssa <> kssb

instance Monoid (MultipleEntityStorer t) where
    mempty = MkMultipleEntityStorer mempty

instance Invariant MultipleEntityStorer where
    invmap ab _ = fmap ab

instance Summable MultipleEntityStorer where
    rVoid = mempty
    esa <+++> esb = fmap Left esa <> fmap Right esb

instance InjectiveFilterable MultipleEntityStorer

instance Filterable MultipleEntityStorer where
    mapMaybe amb (MkMultipleEntityStorer kss) = MkMultipleEntityStorer $ fmap (mapMaybe amb) kss

instance RepresentationalRole MultipleEntityStorer where
    representationalCoercion MkCoercion = MkCoercion
