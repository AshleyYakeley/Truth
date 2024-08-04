module Pinafore.Base.Storable.StoreAdapter
    ( StoreAdapter(..)
    , sequenceStoreAdapter
    , gateStoreAdapter
    , nullStoreAdapter
    , storeAdapterConvert
    , plainStoreAdapter
    , literalStoreAdapter
    , asLiteralStoreAdapter
    , constructorStoreAdapter
    ) where

import Pinafore.Base.Anchor
import Pinafore.Base.Entity
import Pinafore.Base.Know
import Pinafore.Base.KnowShim
import Pinafore.Base.Literal.Literal
import Pinafore.Base.Storable.EntityStorer
import Shapes

data StoreAdapter f t = MkStoreAdapter
    { storeAdapterDefinitions :: EntityStorer ('MultipleMode f) t
    , storeAdapterToDefinition :: t -> SomeOf (EntityStorer 'SingleMode)
    }

nullStoreAdapter :: StoreAdapter f t
nullStoreAdapter =
    MkStoreAdapter {storeAdapterDefinitions = mempty, storeAdapterToDefinition = const $ error "nullStoreAdapter"}

storeAdapterConvert :: StoreAdapter f t -> t -> Entity
storeAdapterConvert ea t =
    case storeAdapterToDefinition ea t of
        MkSomeOf def dt -> entityStorerToEntity def dt

gateStoreAdapter :: Applicative f => f (t -> Bool) -> StoreAdapter f t -> StoreAdapter f t
gateStoreAdapter prd (MkStoreAdapter defs todef) = MkStoreAdapter (gateEntityStorer prd defs) todef

sequenceStoreAdapter ::
       forall f t. Applicative f
    => StoreAdapter f t
    -> f (StoreAdapter Identity t)
sequenceStoreAdapter (MkStoreAdapter defs todef) = fmap (\defs' -> MkStoreAdapter defs' todef) $ sequenceToIdentity defs

instance Functor f => Invariant (StoreAdapter f) where
    invmap ab ba (MkStoreAdapter defs todef) =
        MkStoreAdapter {storeAdapterDefinitions = fmap ab defs, storeAdapterToDefinition = \b -> todef $ ba b}

instance Functor f => Summable (StoreAdapter f) where
    rVoid = MkStoreAdapter {storeAdapterDefinitions = rVoid, storeAdapterToDefinition = never}
    (<+++>) :: forall a b. StoreAdapter f a -> StoreAdapter f b -> StoreAdapter f (Either a b)
    MkStoreAdapter defsa todefa <+++> MkStoreAdapter defsb todefb = let
        defsab = defsa <+++> defsb
        todefab :: Either a b -> SomeOf (EntityStorer 'SingleMode)
        todefab (Left a) = todefa a
        todefab (Right b) = todefb b
        in MkStoreAdapter defsab todefab

unitStoreAdapter :: Applicative f => Entity -> StoreAdapter f ()
unitStoreAdapter entity =
    MkStoreAdapter
        { storeAdapterDefinitions =
              MkEntityStorer $
              pure $
              MkKnowShim PlainConstructorStorer $
              pure $ \e ->
                  if entity == e
                      then Known ()
                      else Unknown
        , storeAdapterToDefinition = \() -> MkSomeOf (MkEntityStorer PlainConstructorStorer) entity
        }

plainStoreAdapter :: Applicative f => StoreAdapter f Entity
plainStoreAdapter = let
    storeAdapterDefinitions = MkEntityStorer $ pure $ simpleKnowShim PlainConstructorStorer
    storeAdapterToDefinition e = MkSomeOf (MkEntityStorer PlainConstructorStorer) e
    in MkStoreAdapter {..}

literalStoreAdapter ::
       forall f t. Applicative f
    => Codec Literal t
    -> StoreAdapter f t
literalStoreAdapter codec = let
    storeAdapterDefinitions :: EntityStorer ('MultipleMode f) t
    storeAdapterDefinitions =
        MkEntityStorer $ pure $ MkKnowShim LiteralConstructorStorer $ pure $ maybeToKnow . decode codec
    storeAdapterToDefinition :: t -> SomeOf (EntityStorer 'SingleMode)
    storeAdapterToDefinition t = MkSomeOf (MkEntityStorer LiteralConstructorStorer) $ encode codec t
    in MkStoreAdapter {..}

asLiteralStoreAdapter ::
       forall f t. (Applicative f, AsLiteral t)
    => StoreAdapter f t
asLiteralStoreAdapter = literalStoreAdapter literalCodec

hashedPredicate :: Anchor -> Int -> Int -> Predicate
hashedPredicate anchor n i = MkPredicate $ hashToAnchor $ \call -> [call anchor, call $ show n, call $ show i]

constructorDefinitions ::
       forall f (tt :: [Type]). Applicative f
    => Anchor
    -> Int
    -> Int
    -> ListType (StoreAdapter f) tt
    -> KnowShim (ListProductType (FieldStorer ('MultipleMode f))) f (ListProduct tt)
constructorDefinitions _anchor _n _i NilListType = simpleKnowShim $ MkListProductType NilListType
constructorDefinitions anchor n i (ConsListType ea lt) = let
    predicate = hashedPredicate anchor n i
    fact1 = MkFieldStorer predicate $ storeAdapterDefinitions ea
    in case constructorDefinitions anchor n (succ i) lt of
           MkKnowShim (MkListProductType factr) fconvr ->
               MkKnowShim (MkListProductType (ConsListType fact1 factr)) $ let
                   ff convr (dt1, dtr) = do
                       ar <- convr dtr
                       return (dt1, ar)
                   in fmap ff fconvr

constructorToDefinition ::
       Anchor
    -> Int
    -> Int
    -> ListType (StoreAdapter f) lt
    -> ListProduct lt
    -> SomeOf (ListProductType (FieldStorer 'SingleMode))
constructorToDefinition _anchor _n _i NilListType () = MkSomeOf (MkListProductType NilListType) ()
constructorToDefinition anchor n i (ConsListType ea lt) (a, l) = let
    predicate = hashedPredicate anchor n i
    in case storeAdapterToDefinition ea a of
           MkSomeOf tt1 v1 -> let
               fact1 = MkFieldStorer predicate tt1
               in case constructorToDefinition anchor n (succ i) lt l of
                      MkSomeOf (MkListProductType factr) vr ->
                          MkSomeOf (MkListProductType (ConsListType fact1 factr)) (v1, vr)

constructorStoreAdapter ::
       forall f lt. Applicative f
    => Anchor
    -> ListType (StoreAdapter f) lt
    -> StoreAdapter f (ListProduct lt)
constructorStoreAdapter anchor NilListType = unitStoreAdapter $ MkEntity anchor
constructorStoreAdapter anchor lt = let
    n = listTypeLength lt
    storeAdapterDefinitions :: EntityStorer ('MultipleMode f) (ListProduct lt)
    storeAdapterDefinitions =
        MkEntityStorer $
        pure $
        convertKnowShim (\(MkListProductType flt) -> ConstructorConstructorStorer anchor flt) $
        constructorDefinitions anchor n 0 lt
    storeAdapterToDefinition :: ListProduct lt -> SomeOf (EntityStorer 'SingleMode)
    storeAdapterToDefinition l =
        case constructorToDefinition anchor n 0 lt l of
            MkSomeOf (MkListProductType lft) v -> MkSomeOf (MkEntityStorer $ ConstructorConstructorStorer anchor lft) v
    in MkStoreAdapter {..}
