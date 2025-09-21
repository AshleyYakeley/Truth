module Pinafore.Base.Storable.StoreAdapter
    ( StoreAdapter
    , storeAdapterDefinitions
    , storeAdapterToDefinition
    , nullStoreAdapter
    , storeAdapterConvert
    , plainStoreAdapter
    , literalStoreAdapter
    , asLiteralStoreAdapter
    , constructorStoreAdapter
    )
where

import Shapes

import Pinafore.Base.Anchor
import Pinafore.Base.Entity
import Pinafore.Base.Know
import Pinafore.Base.KnowShim
import Pinafore.Base.Literal.Literal
import Pinafore.Base.Storable.EntityStorer

data StoreAdapter t = MkStoreAdapter
    { storeAdapterDefinitions :: MultipleEntityStorer t
    , storeAdapterToDefinition :: t -> SomeOf SingleEntityStorer
    }

nullStoreAdapter :: StoreAdapter t
nullStoreAdapter =
    MkStoreAdapter{storeAdapterDefinitions = mempty, storeAdapterToDefinition = const $ error "nullStoreAdapter"}

storeAdapterConvert :: StoreAdapter t -> t -> Entity
storeAdapterConvert ea t =
    case storeAdapterToDefinition ea t of
        MkSomeOf def dt -> entityStorerToEntity def dt

instance Invariant StoreAdapter where
    invmap ab ba (MkStoreAdapter defs todef) =
        MkStoreAdapter{storeAdapterDefinitions = fmap ab defs, storeAdapterToDefinition = \b -> todef $ ba b}

instance InjectiveFilterable StoreAdapter where
    injectiveFilter codec (MkStoreAdapter defs todef) =
        MkStoreAdapter{storeAdapterDefinitions = mapMaybe (decode codec) defs, storeAdapterToDefinition = \b -> todef $ encode codec b}

instance Summable StoreAdapter where
    rVoid = MkStoreAdapter{storeAdapterDefinitions = rVoid, storeAdapterToDefinition = never}
    (<+++>) :: forall a b. StoreAdapter a -> StoreAdapter b -> StoreAdapter (Either a b)
    MkStoreAdapter defsa todefa <+++> MkStoreAdapter defsb todefb = let
        defsab = defsa <+++> defsb
        todefab :: Either a b -> SomeOf SingleEntityStorer
        todefab (Left a) = todefa a
        todefab (Right b) = todefb b
        in MkStoreAdapter defsab todefab

unitStoreAdapter :: Entity -> StoreAdapter ()
unitStoreAdapter entity =
    MkStoreAdapter
        { storeAdapterDefinitions =
            MkMultipleEntityStorer
                $ pure
                $ MkKnowShim PlainConstructorStorer
                $ \e ->
                    if entity == e
                        then Known ()
                        else Unknown
        , storeAdapterToDefinition = \() -> MkSomeOf (MkSingleEntityStorer PlainConstructorStorer) entity
        }

plainStoreAdapter :: StoreAdapter Entity
plainStoreAdapter = let
    storeAdapterDefinitions = MkMultipleEntityStorer $ pure $ simpleKnowShim PlainConstructorStorer
    storeAdapterToDefinition e = MkSomeOf (MkSingleEntityStorer PlainConstructorStorer) e
    in MkStoreAdapter{..}

literalStoreAdapter :: forall t. Codec Literal t -> StoreAdapter t
literalStoreAdapter codec = let
    storeAdapterDefinitions :: MultipleEntityStorer t
    storeAdapterDefinitions = MkMultipleEntityStorer $ pure $ MkKnowShim LiteralConstructorStorer $ maybeToKnow . decode codec
    storeAdapterToDefinition :: t -> SomeOf SingleEntityStorer
    storeAdapterToDefinition t = MkSomeOf (MkSingleEntityStorer LiteralConstructorStorer) $ encode codec t
    in MkStoreAdapter{..}

asLiteralStoreAdapter ::
    forall t.
    AsLiteral t =>
    StoreAdapter t
asLiteralStoreAdapter = literalStoreAdapter literalCodec

hashedPredicate :: Anchor -> Int -> Int -> Predicate
hashedPredicate anchor n i = MkPredicate $ hashToAnchor $ \call -> [call anchor, call $ show n, call $ show i]

constructorDefinitions ::
    forall (tt :: [Type]).
    Anchor ->
    Int ->
    Int ->
    ListType StoreAdapter tt ->
    KnowShim (ListProductType (FieldStorer 'MultipleMode)) (ListProduct tt)
constructorDefinitions _anchor _n _i NilListType = simpleKnowShim $ MkListProductType NilListType
constructorDefinitions anchor n i (ConsListType ea lt) = let
    predicate = hashedPredicate anchor n i
    fact1 :: FieldStorer 'MultipleMode _
    fact1 = MkFieldStorer predicate $ storeAdapterDefinitions ea
    in case constructorDefinitions anchor n (succ i) lt of
        MkKnowShim (MkListProductType factr) convr ->
            MkKnowShim (MkListProductType (ConsListType fact1 factr)) $ \(dt1, dtr) -> do
                ar <- convr dtr
                return (dt1, ar)

constructorToDefinition ::
    Anchor ->
    Int ->
    Int ->
    ListType StoreAdapter lt ->
    ListProduct lt ->
    SomeOf (ListProductType (FieldStorer 'SingleMode))
constructorToDefinition _anchor _n _i NilListType () = MkSomeOf (MkListProductType NilListType) ()
constructorToDefinition anchor n i (ConsListType ea lt) (a, l) = let
    predicate = hashedPredicate anchor n i
    in case storeAdapterToDefinition ea a of
        MkSomeOf tt1 v1 -> let
            fact1 :: FieldStorer 'SingleMode _
            fact1 = MkFieldStorer predicate tt1
            in case constructorToDefinition anchor n (succ i) lt l of
                MkSomeOf (MkListProductType factr) vr ->
                    MkSomeOf (MkListProductType (ConsListType fact1 factr)) (v1, vr)

constructorStoreAdapter :: forall lt. Anchor -> ListType StoreAdapter lt -> StoreAdapter (ListProduct lt)
constructorStoreAdapter anchor NilListType = unitStoreAdapter $ MkEntity anchor
constructorStoreAdapter anchor lt = let
    n = listTypeLength lt
    storeAdapterDefinitions :: MultipleEntityStorer (ListProduct lt)
    storeAdapterDefinitions =
        MkMultipleEntityStorer
            $ pure
            $ convertKnowShim (\(MkListProductType flt) -> ConstructorConstructorStorer anchor flt)
            $ constructorDefinitions anchor n 0 lt
    storeAdapterToDefinition :: ListProduct lt -> SomeOf SingleEntityStorer
    storeAdapterToDefinition l =
        case constructorToDefinition anchor n 0 lt l of
            MkSomeOf (MkListProductType lft) v -> MkSomeOf (MkSingleEntityStorer $ ConstructorConstructorStorer anchor lft) v
    in MkStoreAdapter{..}
