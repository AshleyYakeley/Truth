{-# OPTIONS -fno-warn-orphans #-}

module Changes.Core.Types.Whole where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Read
import Changes.Core.Types.None
import Changes.Core.Types.ReadOnly

data WholeReader (a :: Type) (t :: Type) where
    ReadWhole :: forall t. WholeReader t t

instance TestEquality (WholeReader a) where
    testEquality ReadWhole ReadWhole = Just Refl

instance Show (WholeReader a t) where
    show ReadWhole = "whole"

instance AllConstraint Show (WholeReader a) where
    allConstraint = Dict

instance Show a => WitnessConstraint Show (WholeReader a) where
    witnessConstraint ReadWhole = Dict

instance SubjectReader (WholeReader a) where
    type ReaderSubject (WholeReader a) = a
    subjectToRead msubj ReadWhole = msubj

instance FullSubjectReader (WholeReader a) where
    readableToSubject mr = mr ReadWhole

wholeReadable :: m a -> Readable m (WholeReader a)
wholeReadable ma ReadWhole = ma

newtype WholeReaderEdit (reader :: Type -> Type)
    = MkWholeReaderEdit (ReaderSubject reader)

instance forall reader. Show (ReaderSubject reader) => Show (WholeReaderEdit reader) where
    show (MkWholeReaderEdit subj) = "whole " ++ show subj

instance forall reader. FloatingOn (WholeReaderEdit reader) (WholeReaderEdit reader)

type instance forall reader. EditReader (WholeReaderEdit reader) = reader

instance forall reader. FullSubjectReader reader => ApplicableEdit (WholeReaderEdit reader) where
    applyEdit (MkWholeReaderEdit a) _ = subjectToReadable a

instance forall reader. FullSubjectReader reader => InvertibleEdit (WholeReaderEdit reader) where
    invertEdit _ mr = do
        a <- readableToSubject mr
        return [MkWholeReaderEdit a]

instance forall reader. FullSubjectReader reader => SubjectMapEdit (WholeReaderEdit reader)

instance forall reader. FullSubjectReader reader => FullEdit (WholeReaderEdit reader) where
    replaceEdit mr write = do
        a <- readableToSubject mr
        write $ MkWholeReaderEdit a

instance forall reader. (FullSubjectReader reader, TestEquality reader) => CacheableEdit (WholeReaderEdit reader) where
    trimEdits edits =
        case lastM edits of
            Nothing -> []
            Just edit -> [edit]

mapWholeEdit :: (ReaderSubject ra -> ReaderSubject rb) -> WholeReaderEdit ra -> WholeReaderEdit rb
mapWholeEdit = coerce

lastWholeEdit :: forall reader. [WholeReaderEdit reader] -> Maybe (ReaderSubject reader)
lastWholeEdit edits = do
    MkWholeReaderEdit subj <- lastM edits
    return subj

lastWholeUpdate :: forall reader. [WholeReaderUpdate reader] -> Maybe (ReaderSubject reader)
lastWholeUpdate updates = do
    MkWholeReaderUpdate subj <- lastM updates
    return subj

lastReadOnlyWholeUpdate :: forall reader. [ReadOnlyUpdate (WholeReaderUpdate reader)] -> Maybe (ReaderSubject reader)
lastReadOnlyWholeUpdate updates = do
    MkReadOnlyUpdate (MkWholeReaderUpdate subj) <- lastM updates
    return subj

wholePutEdits ::
    forall mm m reader edita.
    Monad mm =>
    (ReaderSubject reader -> Readable m (EditReader edita) -> mm (Maybe [edita])) ->
    [WholeReaderEdit reader] ->
    Readable m (EditReader edita) ->
    mm (Maybe [edita])
wholePutEdits pe edits mr =
    case lastWholeEdit edits of
        Nothing -> return $ Just []
        (Just subj) -> pe subj mr

type WholeEdit a = WholeReaderEdit (WholeReader a)

type WholeReaderUpdate r = EditUpdate (WholeReaderEdit r)

pattern MkWholeReaderUpdate ::
    ReaderSubject r -> WholeReaderUpdate r
pattern MkWholeReaderUpdate subj =
    MkEditUpdate (MkWholeReaderEdit subj)

{-# COMPLETE MkWholeReaderUpdate #-}

type WholeUpdate a = WholeReaderUpdate (WholeReader a)

pattern MkWholeUpdate :: a -> WholeUpdate a
pattern MkWholeUpdate a = MkWholeReaderUpdate a

{-# COMPLETE MkWholeUpdate #-}

type ROWUpdate a = ReadOnlyUpdate (WholeUpdate a)

changeOnlyUpdateFunction ::
    forall a.
    Eq a =>
    FloatingChangeLens (WholeUpdate a) (ROWUpdate a)
changeOnlyUpdateFunction = let
    sclInit :: forall m. Readable m (WholeReader a) -> m a
    sclInit mr = mr ReadWhole
    sclRead :: ReadFunctionT (StateT a) (WholeReader a) (WholeReader a)
    sclRead _ ReadWhole = get
    sclUpdate ::
        forall m.
        MonadIO m =>
        WholeUpdate a ->
        Readable m (WholeReader a) ->
        StateT a m [ROWUpdate a]
    sclUpdate (MkWholeUpdate newa) _ = do
        olda <- get
        if olda == newa
            then return []
            else do
                put newa
                return [MkReadOnlyUpdate $ MkWholeUpdate newa]
    sclPutEdits ::
        forall m.
        MonadIO m =>
        [ConstEdit _] ->
        Readable m NullReader ->
        StateT a m (Maybe [WholeEdit a])
    sclPutEdits = clPutEditsNone
    in makeStateLens @'Linear MkStateChangeLens{..}

liftROWChangeLens ::
    forall f a b.
    Traversable f =>
    ChangeLens (WholeUpdate a) (ROWUpdate b) ->
    ChangeLens (WholeUpdate (f a)) (ROWUpdate (f b))
liftROWChangeLens (MkChangeLens r u _pe) = let
    fr :: ReadFunction (WholeReader (f a)) (WholeReader (f b))
    fr mr ReadWhole = do
        fa <- mr ReadWhole
        for fa $ \a -> r (\ReadWhole -> return a) ReadWhole
    fu ::
        forall m.
        MonadIO m =>
        WholeUpdate (f a) ->
        Readable m (WholeReader (f a)) ->
        m [ROWUpdate (f b)]
    fu (MkWholeUpdate fa) _mr = do
        fmb <-
            for fa $ \a -> do
                ubs <- u (MkWholeUpdate a) (\ReadWhole -> return a)
                return $ lastReadOnlyWholeUpdate ubs
        return
            $ case sequenceA fmb of
                Just fb -> pure $ MkReadOnlyUpdate $ MkWholeUpdate fb
                Nothing -> []
    fpe ::
        forall m.
        MonadIO m =>
        [ConstEdit _] ->
        Readable m (WholeReader (f a)) ->
        m (Maybe [WholeEdit (f a)])
    fpe = clPutEditsNone
    in MkChangeLens fr fu fpe

ioWholeChangeLens :: forall a b. (a -> IO b) -> (b -> a -> IO (Maybe a)) -> ChangeLens (WholeUpdate a) (WholeUpdate b)
ioWholeChangeLens ioget ioput = let
    clRead :: ReadFunction (WholeReader a) (WholeReader b)
    clRead mr ReadWhole = do
        a <- mr ReadWhole
        liftIO $ ioget a
    clUpdate ::
        forall m.
        MonadIO m =>
        WholeUpdate a ->
        Readable m (WholeReader a) ->
        m [WholeUpdate b]
    clUpdate (MkWholeUpdate a) _ = do
        b <- liftIO $ ioget a
        return [MkWholeUpdate b]
    clPutEdits ::
        forall m.
        MonadIO m =>
        [WholeEdit b] ->
        Readable m (WholeReader a) ->
        m (Maybe [WholeEdit a])
    clPutEdits =
        clPutEditsFromPutEdit $ \(MkWholeReaderEdit b) mr -> do
            olda <- mr ReadWhole
            mnewa <- liftIO $ ioput b olda
            return $ fmap (\newa -> [MkWholeReaderEdit newa]) mnewa
    in MkChangeLens{..}

wholeChangeLens ::
    forall mf a b.
    MonadInner mf =>
    Lens' mf a b ->
    ChangeLens (WholeUpdate a) (WholeUpdate b)
wholeChangeLens lens =
    ioWholeChangeLens (\a -> return $ lensGet lens a) (\b olda -> return $ mToMaybe $ lensPutback lens b olda)

bijectionWholeChangeLens ::
    forall lin updateA b.
    FullEdit (UpdateEdit updateA) =>
    Bijection (UpdateSubject updateA) b ->
    GenChangeLens lin updateA (WholeUpdate b)
bijectionWholeChangeLens MkIsomorphism{..} = let
    clRead :: ReadFunction (UpdateReader updateA) (WholeReader b)
    clRead mr ReadWhole = do
        a <- readableToSubject mr
        return $ isoForwards a
    clUpdate ::
        forall m.
        MonadIO m =>
        updateA ->
        Readable m (UpdateReader updateA) ->
        m [WholeUpdate b]
    clUpdate _ mr = do
        a <- readableToSubject mr
        return [MkWholeUpdate $ isoForwards a]
    clPutEdits ::
        forall m.
        MonadIO m =>
        [WholeEdit b] ->
        Readable m _ ->
        m (Maybe [UpdateEdit updateA])
    clPutEdits =
        linearPutEditsFromPutEdit $ \(MkWholeReaderEdit b) -> do
            editas <- getReplaceEditsFromSubject $ isoBackwards b
            return $ Just editas
    in MkChangeLens{..}

instance MonadInner m => IsChangeLens (Lens' m a b) where
    type LensDomain (Lens' m a b) = WholeUpdate a
    type LensRange (Lens' m a b) = WholeUpdate b
    toChangeLens = toChangeLens . wholeChangeLens

instance MonadInner m => IsChangeLens (Injection' m a b) where
    type LensDomain (Injection' m a b) = WholeUpdate a
    type LensRange (Injection' m a b) = WholeUpdate b
    toChangeLens = toChangeLens . injectionLens

instance IsChangeLens (Bijection a b) where
    type LensDomain (Bijection a b) = WholeUpdate a
    type LensRange (Bijection a b) = WholeUpdate b
    toChangeLens = toChangeLens . bijectionInjection

instance IsChangeLens (Codec a b) where
    type LensDomain (Codec a b) = WholeUpdate a
    type LensRange (Codec a b) = WholeUpdate (Maybe b)
    toChangeLens = toChangeLens . codecInjection
