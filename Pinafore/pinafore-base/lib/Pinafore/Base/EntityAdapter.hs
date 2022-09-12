module Pinafore.Base.EntityAdapter
    ( EntityAdapter(..)
    , nullEntityAdapter
    , entityAdapterConvert
    --, entityAdapterType
    , plainEntityAdapter
    , literalEntityAdapter
    , constructorEntityAdapter
    ) where

import Pinafore.Base.Anchor
import Pinafore.Base.Entity
import Pinafore.Base.EntityStorer
import Pinafore.Base.Know
import Pinafore.Base.KnowShim
import Pinafore.Base.Literal
import Shapes

data EntityAdapter t = MkEntityAdapter
    { entityAdapterDefinitions :: EntityStorer 'MultipleMode t
    , entityAdapterToDefinition :: t -> SomeOf (EntityStorer 'SingleMode)
    }

nullEntityAdapter :: EntityAdapter t
nullEntityAdapter =
    MkEntityAdapter {entityAdapterDefinitions = mempty, entityAdapterToDefinition = const $ error "nullEntityAdapter"}

entityAdapterConvert :: EntityAdapter t -> t -> Entity
entityAdapterConvert ea t =
    case entityAdapterToDefinition ea t of
        MkSomeOf def dt -> entityStorerToEntity def dt

instance Invariant EntityAdapter where
    invmap ab ba (MkEntityAdapter defs todef) =
        MkEntityAdapter {entityAdapterDefinitions = fmap ab defs, entityAdapterToDefinition = \b -> todef $ ba b}

instance Summable EntityAdapter where
    rVoid = MkEntityAdapter {entityAdapterDefinitions = rVoid, entityAdapterToDefinition = never}
    (<+++>) :: forall a b. EntityAdapter a -> EntityAdapter b -> EntityAdapter (Either a b)
    MkEntityAdapter defsa todefa <+++> MkEntityAdapter defsb todefb = let
        defsab = defsa <+++> defsb
        todefab :: Either a b -> SomeOf (EntityStorer 'SingleMode)
        todefab (Left a) = todefa a
        todefab (Right b) = todefb b
        in MkEntityAdapter defsab todefab

unitEntityAdapter :: Entity -> EntityAdapter ()
unitEntityAdapter entity =
    MkEntityAdapter
        { entityAdapterDefinitions =
              MkEntityStorer $
              pure $
              MkKnowShim PlainConstructorStorer $ \e ->
                  if entity == e
                      then Known ()
                      else Unknown
        , entityAdapterToDefinition = \() -> MkSomeOf (MkEntityStorer PlainConstructorStorer) entity
        }

plainEntityAdapter :: EntityAdapter Entity
plainEntityAdapter = let
    entityAdapterDefinitions = MkEntityStorer $ pure $ simpleKnowShim PlainConstructorStorer
    entityAdapterToDefinition e = MkSomeOf (MkEntityStorer PlainConstructorStorer) e
    in MkEntityAdapter {..}

literalEntityAdapter :: forall t. Codec Literal t -> EntityAdapter t
literalEntityAdapter codec = let
    entityAdapterDefinitions :: EntityStorer 'MultipleMode t
    entityAdapterDefinitions = MkEntityStorer $ pure $ MkKnowShim LiteralConstructorStorer $ maybeToKnow . decode codec
    entityAdapterToDefinition :: t -> SomeOf (EntityStorer 'SingleMode)
    entityAdapterToDefinition t = MkSomeOf (MkEntityStorer LiteralConstructorStorer) $ encode codec t
    in MkEntityAdapter {..}

hashedPredicate :: Anchor -> Int -> Int -> Predicate
hashedPredicate anchor n i = MkPredicate $ hashToAnchor $ \call -> [call anchor, call $ show n, call $ show i]

constructorDefinitions ::
       forall (tt :: [Type]).
       Anchor
    -> Int
    -> Int
    -> ListType EntityAdapter tt
    -> KnowShim (ListProductType (FieldStorer 'MultipleMode)) (ListProduct tt)
constructorDefinitions _anchor _n _i NilListType = simpleKnowShim $ MkListProductType NilListType
constructorDefinitions anchor n i (ConsListType ea lt) = let
    predicate = hashedPredicate anchor n i
    fact1 = MkFieldStorer predicate $ entityAdapterDefinitions ea
    in case constructorDefinitions anchor n (succ i) lt of
           MkKnowShim (MkListProductType factr) convr ->
               MkKnowShim (MkListProductType (ConsListType fact1 factr)) $ \(dt1, dtr) -> do
                   ar <- convr dtr
                   return (dt1, ar)

constructorToDefinition ::
       Anchor
    -> Int
    -> Int
    -> ListType EntityAdapter lt
    -> ListProduct lt
    -> SomeOf (ListProductType (FieldStorer 'SingleMode))
constructorToDefinition _anchor _n _i NilListType () = MkSomeOf (MkListProductType NilListType) ()
constructorToDefinition anchor n i (ConsListType ea lt) (a, l) = let
    predicate = hashedPredicate anchor n i
    in case entityAdapterToDefinition ea a of
           MkSomeOf tt1 v1 -> let
               fact1 = MkFieldStorer predicate tt1
               in case constructorToDefinition anchor n (succ i) lt l of
                      MkSomeOf (MkListProductType factr) vr ->
                          MkSomeOf (MkListProductType (ConsListType fact1 factr)) (v1, vr)

constructorEntityAdapter :: forall lt. Anchor -> ListType EntityAdapter lt -> EntityAdapter (ListProduct lt)
constructorEntityAdapter anchor NilListType = unitEntityAdapter $ MkEntity anchor
constructorEntityAdapter anchor lt = let
    n = listTypeLength lt
    entityAdapterDefinitions :: EntityStorer 'MultipleMode (ListProduct lt)
    entityAdapterDefinitions =
        MkEntityStorer $
        pure $
        convertKnowShim (\(MkListProductType flt) -> ConstructorConstructorStorer anchor flt) $
        constructorDefinitions anchor n 0 lt
    entityAdapterToDefinition :: ListProduct lt -> SomeOf (EntityStorer 'SingleMode)
    entityAdapterToDefinition l =
        case constructorToDefinition anchor n 0 lt l of
            MkSomeOf (MkListProductType lft) v -> MkSomeOf (MkEntityStorer $ ConstructorConstructorStorer anchor lft) v
    in MkEntityAdapter {..}
