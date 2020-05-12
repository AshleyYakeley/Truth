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
    , entityAdapterGet :: forall m. MonadIO m => Entity -> Readable m PinaforeEntityRead -> m (Know t)
    , entityAdapterPut :: forall m. MonadIO m => t -> Readable m PinaforeEntityRead -> m [PinaforeEntityEdit]
    }

instance IsoVariant EntityAdapter where
    isoMap ab ba (MkEntityAdapter ca ga pa) =
        MkEntityAdapter
            { entityAdapterConvert = \b -> ca $ ba b
            , entityAdapterGet = \e mr -> fmap (fmap ab) $ ga e mr
            , entityAdapterPut = \b -> pa $ ba b
            }

instance Summish EntityAdapter where
    pNone =
        MkEntityAdapter
            { entityAdapterConvert = never
            , entityAdapterGet = \_ _ -> return Unknown
            , entityAdapterPut = \n _ -> return $ never n
            }
    (<+++>) :: forall a b. EntityAdapter a -> EntityAdapter b -> EntityAdapter (Either a b)
    MkEntityAdapter ca ga pa <+++> MkEntityAdapter cb gb pb = let
        cab (Left a) = ca a
        cab (Right b) = cb b
        gab :: forall m. MonadIO m
            => Entity
            -> Readable m PinaforeEntityRead
            -> m (Know (Either a b))
        gab e mr =
            getComposeM $
            (do
                 a <- MkComposeM $ ga e mr
                 return $ Left a) <|>
            (do
                 b <- MkComposeM $ gb e mr
                 return $ Right b)
        pab :: forall m. MonadIO m
            => Either a b
            -> Readable m PinaforeEntityRead
            -> m [PinaforeEntityEdit]
        pab (Left a) = pa a
        pab (Right b) = pb b
        in MkEntityAdapter cab gab pab

entityEntityAdapter :: EntityAdapter Entity
entityEntityAdapter = let
    entityAdapterConvert = id
    entityAdapterGet ::
           forall m. MonadIO m
        => Entity
        -> Readable m PinaforeEntityRead
        -> m (Know Entity)
    entityAdapterGet p _ = return $ Known p
    entityAdapterPut _ _ = return []
    in MkEntityAdapter {..}

unitEntityAdapter :: Anchor -> EntityAdapter ()
unitEntityAdapter anchor =
    MkEntityAdapter
        { entityAdapterConvert = \() -> MkEntity anchor
        , entityAdapterGet =
              \e _ ->
                  return $
                  if MkEntity anchor == e
                      then Known ()
                      else Unknown
        , entityAdapterPut = \() _ -> return []
        }

hashedPredicate :: Anchor -> Int -> Int -> Predicate
hashedPredicate anchor n i = MkPredicate $ hashToAnchor $ \call -> [call anchor, call $ show n, call $ show i]

constructorGet ::
       MonadIO m
    => Anchor
    -> Int
    -> Int
    -> ListType EntityAdapter tt
    -> Entity
    -> Readable m PinaforeEntityRead
    -> ComposeM Know m (HList tt)
constructorGet _anchor _n _i NilListType _entity _mr = return ()
constructorGet anchor n i (ConsListType (MkEntityAdapter _ ga _) ll) entity mr = do
    entitya <- MkComposeM $ mr $ PinaforeEntityReadGetPredicate (hashedPredicate anchor n i) entity
    a <- MkComposeM $ ga entitya mr
    l <- constructorGet anchor n (succ i) ll entity mr
    return (a, l)

hashList :: (forall t. Serialize t => t -> r) -> ListType EntityAdapter tt -> HList tt -> [r]
hashList _call NilListType () = []
hashList call (ConsListType (MkEntityAdapter ca _ _) lt) (a, l) = call (ca a) : hashList call lt l

getWriter :: Monad m => WriterT w m () -> m w
getWriter = execWriterT

mkWriter :: Functor m => m w -> WriterT w m ()
mkWriter mw = WriterT $ fmap (\w -> ((), w)) mw

constructorPut ::
       MonadIO m
    => Anchor
    -> Int
    -> Int
    -> Entity
    -> ListType EntityAdapter lt
    -> HList lt
    -> Readable m PinaforeEntityRead
    -> WriterT [PinaforeEntityEdit] m ()
constructorPut _anchor _n _i _entity NilListType () _mr = return ()
constructorPut anchor n i entity (ConsListType (MkEntityAdapter ga _ pa) lt) (a, l) mr = do
    mkWriter $ pa a mr
    tell $ pure $ PinaforeEntityEditSetPredicate (hashedPredicate anchor n i) entity $ Known $ ga a
    constructorPut anchor n (succ i) entity lt l mr

constructorEntityAdapter :: forall lt. Anchor -> ListType EntityAdapter lt -> EntityAdapter (HList lt)
constructorEntityAdapter anchor NilListType = unitEntityAdapter anchor
constructorEntityAdapter anchor lt = let
    n = listTypeLength lt
    entityAdapterConvert :: HList lt -> Entity
    entityAdapterConvert l = hashToEntity $ \call -> call anchor : hashList call lt l
    entityAdapterGet ::
           forall m. MonadIO m
        => Entity
        -> Readable m PinaforeEntityRead
        -> m (Know (HList lt))
    entityAdapterGet entity mr = getComposeM $ constructorGet anchor n 0 lt entity mr
    entityAdapterPut ::
           forall m. MonadIO m
        => HList lt
        -> Readable m PinaforeEntityRead
        -> m [PinaforeEntityEdit]
    entityAdapterPut l mr = getWriter $ constructorPut anchor n 0 (entityAdapterConvert l) lt l mr
    in MkEntityAdapter {..}
