module Pinafore.Base.EntityAdapter
    ( EntityAdapter(..)
    , entityEntityAdapter
    , constructorEntityAdapter
    ) where

import Pinafore.Base.Anchor
import Pinafore.Base.Edit
import Pinafore.Base.Entity
import Pinafore.Base.Know
import Shapes
import Truth.Core

data EntityAdapter t = MkEntityAdapter
    { entityAdapterConvert :: t -> Entity
    , entityAdapterGet :: Entity -> ReadM PinaforeEntityRead (Know t)
    , entityAdapterPut :: t -> ReadM PinaforeEntityRead [PinaforeEntityEdit]
    }

instance IsoVariant EntityAdapter where
    isoMap ab ba (MkEntityAdapter ca ga pa) =
        MkEntityAdapter
            { entityAdapterConvert = \b -> ca $ ba b
            , entityAdapterGet = \e -> fmap (fmap ab) $ ga e
            , entityAdapterPut = \b -> pa $ ba b
            }

instance Summish EntityAdapter where
    pNone =
        MkEntityAdapter
            { entityAdapterConvert = never
            , entityAdapterGet = \_ -> return Unknown
            , entityAdapterPut = \n -> return $ never n
            }
    (<+++>) :: forall a b. EntityAdapter a -> EntityAdapter b -> EntityAdapter (Either a b)
    MkEntityAdapter ca ga pa <+++> MkEntityAdapter cb gb pb = let
        cab (Left a) = ca a
        cab (Right b) = cb b
        gab :: Entity -> ReadM PinaforeEntityRead (Know (Either a b))
        gab e =
            getComposeM $
            (do
                 a <- MkComposeM $ ga e
                 return $ Left a) <|>
            (do
                 b <- MkComposeM $ gb e
                 return $ Right b)
        pab :: Either a b -> ReadM PinaforeEntityRead [PinaforeEntityEdit]
        pab (Left a) = pa a
        pab (Right b) = pb b
        in MkEntityAdapter cab gab pab

entityEntityAdapter :: EntityAdapter Entity
entityEntityAdapter = let
    entityAdapterConvert = id
    entityAdapterGet :: Entity -> ReadM PinaforeEntityRead (Know Entity)
    entityAdapterGet p = return $ Known p
    entityAdapterPut _ = return []
    in MkEntityAdapter {..}

unitEntityAdapter :: Anchor -> EntityAdapter ()
unitEntityAdapter anchor =
    MkEntityAdapter
        { entityAdapterConvert = \() -> MkEntity anchor
        , entityAdapterGet =
              \e ->
                  return $
                  if MkEntity anchor == e
                      then Known ()
                      else Unknown
        , entityAdapterPut = \() -> return []
        }

hashedPredicate :: Anchor -> Int -> Int -> Predicate
hashedPredicate anchor n i = MkPredicate $ hashToAnchor $ \call -> [call anchor, call $ show n, call $ show i]

constructorGet ::
       Anchor
    -> Int
    -> Int
    -> ListType EntityAdapter tt
    -> Entity
    -> ComposeM Know (ReadM PinaforeEntityRead) (HList tt)
constructorGet _anchor _n _i NilListType _entity = return ()
constructorGet anchor n i (ConsListType (MkEntityAdapter _ ga _) ll) entity = do
    entitya <- MkComposeM $ readM $ PinaforeEntityReadGetPredicate (hashedPredicate anchor n i) entity
    a <- MkComposeM $ ga entitya
    l <- constructorGet anchor n (succ i) ll entity
    return (a, l)

hashList :: (forall t. Serialize t => t -> r) -> ListType EntityAdapter tt -> HList tt -> [r]
hashList _call NilListType () = []
hashList call (ConsListType (MkEntityAdapter ca _ _) lt) (a, l) = call (ca a) : hashList call lt l

getWriter :: Monad m => WriterT w m () -> m w
getWriter = execWriterT

mkWriter :: Functor m => m w -> WriterT w m ()
mkWriter mw = WriterT $ fmap (\w -> ((), w)) mw

constructorPut ::
       Anchor
    -> Int
    -> Int
    -> Entity
    -> ListType EntityAdapter lt
    -> HList lt
    -> WriterT [PinaforeEntityEdit] (ReadM PinaforeEntityRead) ()
constructorPut _anchor _n _i _entity NilListType () = return ()
constructorPut anchor n i entity (ConsListType (MkEntityAdapter ga _ pa) lt) (a, l) = do
    mkWriter $ pa a
    tell $ pure $ PinaforeEntityEditSetPredicate (hashedPredicate anchor n i) entity $ Known $ ga a
    constructorPut anchor n (succ i) entity lt l

constructorEntityAdapter :: forall lt. Anchor -> ListType EntityAdapter lt -> EntityAdapter (HList lt)
constructorEntityAdapter anchor NilListType = unitEntityAdapter anchor
constructorEntityAdapter anchor lt = let
    n = listTypeLength lt
    entityAdapterConvert :: HList lt -> Entity
    entityAdapterConvert l = hashToEntity $ \call -> call anchor : hashList call lt l
    entityAdapterGet :: Entity -> ReadM PinaforeEntityRead (Know (HList lt))
    entityAdapterGet entity = getComposeM $ constructorGet anchor n 0 lt entity
    entityAdapterPut :: HList lt -> ReadM PinaforeEntityRead [PinaforeEntityEdit]
    entityAdapterPut l = getWriter $ constructorPut anchor n 0 (entityAdapterConvert l) lt l
    in MkEntityAdapter {..}
