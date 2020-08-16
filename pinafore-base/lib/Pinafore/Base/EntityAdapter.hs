module Pinafore.Base.EntityAdapter
    ( EntityAdapter(..)
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
    , entityAdapterToDefinition :: t -> AnyValue (EntityStorer 'SingleMode)
    }

entityAdapterConvert :: EntityAdapter t -> t -> Entity
entityAdapterConvert ea t =
    case entityAdapterToDefinition ea t of
        MkAnyValue def dt -> entityStorerToEntity def dt

instance IsoVariant EntityAdapter where
    isoMap ab ba (MkEntityAdapter defs todef) =
        MkEntityAdapter {entityAdapterDefinitions = fmap ab defs, entityAdapterToDefinition = \b -> todef $ ba b}

instance Summish EntityAdapter where
    pNone = MkEntityAdapter {entityAdapterDefinitions = pNone, entityAdapterToDefinition = never}
    (<+++>) :: forall a b. EntityAdapter a -> EntityAdapter b -> EntityAdapter (Either a b)
    MkEntityAdapter defsa todefa <+++> MkEntityAdapter defsb todefb = let
        defsab = defsa <+++> defsb
        todefab :: Either a b -> AnyValue (EntityStorer 'SingleMode)
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
        , entityAdapterToDefinition = \() -> MkAnyValue (MkEntityStorer PlainConstructorStorer) entity
        }

plainEntityAdapter :: EntityAdapter Entity
plainEntityAdapter = let
    entityAdapterDefinitions = MkEntityStorer $ pure $ MkKnowShim PlainConstructorStorer Known
    entityAdapterToDefinition e = MkAnyValue (MkEntityStorer PlainConstructorStorer) e
    in MkEntityAdapter {..}

literalEntityAdapter ::
       forall t. AsLiteral t
    => EntityAdapter t
literalEntityAdapter = let
    entityAdapterDefinitions :: EntityStorer 'MultipleMode t
    entityAdapterDefinitions = MkEntityStorer $ pure $ MkKnowShim LiteralConstructorStorer fromLiteral
    entityAdapterToDefinition :: t -> AnyValue (EntityStorer 'SingleMode)
    entityAdapterToDefinition t = MkAnyValue (MkEntityStorer LiteralConstructorStorer) $ toLiteral t
    in MkEntityAdapter {..}

hashedPredicate :: Anchor -> Int -> Int -> Predicate
hashedPredicate anchor n i = MkPredicate $ hashToAnchor $ \call -> [call anchor, call $ show n, call $ show i]

constructorDefinitions ::
       forall (tt :: [Type]).
       Anchor
    -> Int
    -> Int
    -> ListType EntityAdapter tt
    -> KnowShim (HListWit (FieldStorer 'MultipleMode)) (HList tt)
constructorDefinitions _anchor _n _i NilListType = simpleKnowShim $ MkHListWit NilListType
constructorDefinitions anchor n i (ConsListType ea lt) = let
    predicate = hashedPredicate anchor n i
    fact1 = MkFieldStorer predicate $ entityAdapterDefinitions ea
    in case constructorDefinitions anchor n (succ i) lt of
           MkKnowShim (MkHListWit factr) convr ->
               MkKnowShim (MkHListWit (ConsListType fact1 factr)) $ \(dt1, dtr) -> do
                   ar <- convr dtr
                   return (dt1, ar)

constructorToDefinition ::
       Anchor -> Int -> Int -> ListType EntityAdapter lt -> HList lt -> AnyValue (HListWit (FieldStorer 'SingleMode))
constructorToDefinition _anchor _n _i NilListType () = MkAnyValue (MkHListWit NilListType) ()
constructorToDefinition anchor n i (ConsListType ea lt) (a, l) = let
    predicate = hashedPredicate anchor n i
    in case entityAdapterToDefinition ea a of
           MkAnyValue tt1 v1 -> let
               fact1 = MkFieldStorer predicate tt1
               in case constructorToDefinition anchor n (succ i) lt l of
                      MkAnyValue (MkHListWit factr) vr -> MkAnyValue (MkHListWit (ConsListType fact1 factr)) (v1, vr)

constructorEntityAdapter :: forall lt. Anchor -> ListType EntityAdapter lt -> EntityAdapter (HList lt)
constructorEntityAdapter anchor NilListType = unitEntityAdapter $ MkEntity anchor
constructorEntityAdapter anchor lt = let
    n = listTypeLength lt
    entityAdapterDefinitions :: EntityStorer 'MultipleMode (HList lt)
    entityAdapterDefinitions =
        MkEntityStorer $
        pure $
        convertKnowShim (\(MkHListWit flt) -> ConstructorConstructorStorer anchor flt) $
        constructorDefinitions anchor n 0 lt
    entityAdapterToDefinition :: HList lt -> AnyValue (EntityStorer 'SingleMode)
    entityAdapterToDefinition l =
        case constructorToDefinition anchor n 0 lt l of
            MkAnyValue (MkHListWit lft) v -> MkAnyValue (MkEntityStorer $ ConstructorConstructorStorer anchor lft) v
    in MkEntityAdapter {..}
