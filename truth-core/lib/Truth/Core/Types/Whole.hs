{-# OPTIONS -fno-warn-orphans #-}

module Truth.Core.Types.Whole where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Lens
import Truth.Core.Read
import Truth.Core.Types.None
import Truth.Core.Types.ReadOnly

data WholeReader (a :: Type) (t :: Type) where
    ReadWhole :: forall t. WholeReader t t

instance TestEquality (WholeReader a) where
    testEquality ReadWhole ReadWhole = Just Refl

instance Show (WholeReader a t) where
    show ReadWhole = "whole"

instance AllWitnessConstraint Show (WholeReader a) where
    allWitnessConstraint = Dict

instance Show a => WitnessConstraint Show (WholeReader a) where
    witnessConstraint ReadWhole = Dict

instance SubjectReader (WholeReader a) where
    type ReaderSubject (WholeReader a) = a
    subjectToRead msubj ReadWhole = msubj

instance FullSubjectReader (WholeReader a) where
    mutableReadToSubject mr = mr ReadWhole

wholeMutableRead :: m a -> MutableRead m (WholeReader a)
wholeMutableRead ma ReadWhole = ma

newtype WholeReaderEdit (reader :: Type -> Type) =
    MkWholeReaderEdit (ReaderSubject reader)

instance Show (ReaderSubject reader) => Show (WholeReaderEdit reader) where
    show (MkWholeReaderEdit subj) = "whole " ++ show subj

instance Floating (WholeReaderEdit reader) (WholeReaderEdit reader)

type instance EditReader (WholeReaderEdit reader) = reader

instance (FullSubjectReader reader) => ApplicableEdit (WholeReaderEdit reader) where
    applyEdit (MkWholeReaderEdit a) _ = subjectToMutableRead a

instance (FullSubjectReader reader) => InvertibleEdit (WholeReaderEdit reader) where
    invertEdit _ mr = do
        a <- mutableReadToSubject mr
        return [MkWholeReaderEdit a]

instance FullSubjectReader reader => SubjectMapEdit (WholeReaderEdit reader)

instance (FullSubjectReader reader) => FullEdit (WholeReaderEdit reader) where
    replaceEdit mr write = do
        a <- mutableReadToSubject mr
        write $ MkWholeReaderEdit a

instance (FullSubjectReader reader, TestEquality reader) => CacheableEdit (WholeReaderEdit reader)

mapWholeEdit :: (ReaderSubject ra -> ReaderSubject rb) -> WholeReaderEdit ra -> WholeReaderEdit rb
mapWholeEdit = coerce

lastWholeEdit :: [WholeReaderEdit reader] -> Maybe (ReaderSubject reader)
lastWholeEdit edits = do
    MkWholeReaderEdit subj <- lastM edits
    return subj

lastWholeUpdate :: [WholeReaderUpdate reader] -> Maybe (ReaderSubject reader)
lastWholeUpdate updates = do
    MkWholeReaderUpdate subj <- lastM updates
    return subj

lastReadOnlyWholeUpdate :: [ReadOnlyUpdate (WholeReaderUpdate reader)] -> Maybe (ReaderSubject reader)
lastReadOnlyWholeUpdate updates = do
    MkReadOnlyUpdate (MkWholeReaderUpdate subj) <- lastM updates
    return subj

wholePutEdits ::
       (Monad mm, Monad m)
    => (ReaderSubject reader -> MutableRead m (EditReader edita) -> mm (Maybe [edita]))
    -> [WholeReaderEdit reader]
    -> MutableRead m (EditReader edita)
    -> mm (Maybe [edita])
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

changeOnlyUpdateFunction ::
       forall a. Eq a
    => FloatingEditLens (WholeUpdate a) (ReadOnlyUpdate (WholeUpdate a))
changeOnlyUpdateFunction = let
    sInit ::
           forall m. MonadIO m
        => MutableRead m (WholeReader a)
        -> m a
    sInit mr = mr ReadWhole
    sGet :: ReadFunctionT (StateT a) (WholeReader a) (WholeReader a)
    sGet _ ReadWhole = get
    sUpdate ::
           forall m. MonadIO m
        => WholeUpdate a
        -> MutableRead m (WholeReader a)
        -> StateT a m [ReadOnlyUpdate (WholeUpdate a)]
    sUpdate (MkWholeUpdate newa) _ = do
        olda <- get
        if olda == newa
            then return []
            else do
                put newa
                return [MkReadOnlyUpdate $ MkWholeUpdate newa]
    sPutEdits ::
           forall m. MonadIO m
        => [NoEdit _]
        -> MutableRead m (WholeReader a)
        -> StateT a m (Maybe [WholeEdit a])
    sPutEdits = elPutEditsNone
    in makeStateLens MkStateEditLens {..}

ioWholeEditLens :: forall a b. (a -> IO b) -> (b -> a -> IO (Maybe a)) -> EditLens (WholeUpdate a) (WholeUpdate b)
ioWholeEditLens ioget ioput = let
    elGet :: ReadFunction (WholeReader a) (WholeReader b)
    elGet mr ReadWhole = do
        a <- mr ReadWhole
        liftIO $ ioget a
    elUpdate ::
           forall m. MonadIO m
        => WholeUpdate a
        -> MutableRead m (WholeReader a)
        -> m [WholeUpdate b]
    elUpdate (MkWholeUpdate a) _ = do
        b <- liftIO $ ioget a
        return [MkWholeUpdate b]
    elPutEdits ::
           forall m. MonadIO m
        => [WholeEdit b]
        -> MutableRead m (WholeReader a)
        -> m (Maybe [WholeEdit a])
    elPutEdits =
        elPutEditsFromPutEdit $ \(MkWholeReaderEdit b) mr -> do
            olda <- mr ReadWhole
            mnewa <- liftIO $ ioput b olda
            return $ fmap (\newa -> [MkWholeReaderEdit newa]) mnewa
    in MkEditLens {..}

wholeEditLens ::
       forall mf a b. (MonadOne mf)
    => Lens' mf a b
    -> EditLens (WholeUpdate a) (WholeUpdate b)
wholeEditLens lens =
    ioWholeEditLens (\a -> return $ lensGet lens a) (\b olda -> return $ getMaybeOne $ lensPutback lens b olda)

bijectionWholeEditLens :: Bijection a b -> EditLens (WholeUpdate a) (WholeUpdate b)
bijectionWholeEditLens = wholeEditLens . bijectionLens

instance MonadOne m => IsEditLens (Lens' m a b) where
    type LensDomain (Lens' m a b) = WholeUpdate a
    type LensRange (Lens' m a b) = WholeUpdate b
    toEditLens = toEditLens . wholeEditLens

instance MonadOne m => IsEditLens (Injection' m a b) where
    type LensDomain (Injection' m a b) = WholeUpdate a
    type LensRange (Injection' m a b) = WholeUpdate b
    toEditLens = toEditLens . injectionLens

instance IsEditLens (Bijection a b) where
    type LensDomain (Bijection a b) = WholeUpdate a
    type LensRange (Bijection a b) = WholeUpdate b
    toEditLens = toEditLens . bijectionInjection

instance IsEditLens (Codec a b) where
    type LensDomain (Codec a b) = WholeUpdate a
    type LensRange (Codec a b) = WholeUpdate (Maybe b)
    toEditLens = toEditLens . codecInjection
