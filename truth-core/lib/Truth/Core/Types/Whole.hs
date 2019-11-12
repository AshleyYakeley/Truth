{-# OPTIONS -fno-warn-orphans #-}

module Truth.Core.Types.Whole where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read

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

wholeUpdateFunction :: forall a b. (a -> b) -> UpdateFunction (WholeUpdate a) (WholeUpdate b)
wholeUpdateFunction ab =
    MkRunnable2 cmEmpty $
    MkAnUpdateFunction
        { ufGet = \mr ReadWhole -> fmap ab $ mr ReadWhole
        , ufUpdate = \(MkWholeUpdate a) _ -> return [MkWholeUpdate $ ab a]
        }

ioWholeUpdateFunction :: forall a b. (a -> IO b) -> UpdateFunction (WholeUpdate a) (WholeUpdate b)
ioWholeUpdateFunction aiob =
    MkRunnable2 cmEmpty $
    MkAnUpdateFunction
        { ufGet =
              \mr ReadWhole -> do
                  a <- mr ReadWhole
                  liftIO $ aiob a
        , ufUpdate =
              \(MkWholeUpdate a) _ -> do
                  b <- liftIO $ aiob a
                  return [MkWholeUpdate b]
        }

changeOnlyUpdateFunction ::
       forall a. Eq a
    => IO (UpdateFunction (WholeUpdate a) (WholeUpdate a))
changeOnlyUpdateFunction = do
    var <- newMVar Nothing
    let
        ufGet :: ReadFunctionT (StateT (Maybe a)) (WholeReader a) (WholeReader a)
        ufGet mr ReadWhole = do
            ma <- get
            case ma of
                Just a -> return a
                Nothing -> do
                    a <- lift $ mr ReadWhole
                    put $ Just a
                    return a
        ufUpdate ::
               forall m. MonadIO m
            => WholeUpdate a
            -> MutableRead m (WholeReader a)
            -> StateT (Maybe a) m [WholeUpdate a]
        ufUpdate (MkWholeUpdate newa) _ = do
            molda <- get
            case molda of
                Just olda
                    | olda == newa -> return []
                _ -> do
                    put $ Just newa
                    return [MkWholeUpdate newa]
    return $ MkRunnable2 (MkTransStackRunner @'[ StateT (Maybe a)] $ mVarRun var) $ MkAnUpdateFunction {..}

ioWholeEditLens :: forall a b. (a -> IO b) -> (b -> a -> IO (Maybe a)) -> EditLens (WholeUpdate a) (WholeUpdate b)
ioWholeEditLens ioget ioput =
    MkRunnable2 cmEmpty $
    MkAnEditLens
        { elFunction =
              MkAnUpdateFunction
                  { ufGet =
                        \mr ReadWhole -> do
                            a <- mr ReadWhole
                            liftIO $ ioget a
                  , ufUpdate =
                        \(MkWholeUpdate a) _ -> do
                            b <- liftIO $ ioget a
                            return [MkWholeUpdate b]
                  }
        , elPutEdits =
              elPutEditsFromPutEdit @'[] $ \(MkWholeReaderEdit b) mr -> do
                  olda <- mr ReadWhole
                  mnewa <- liftIO $ ioput b olda
                  return $ fmap (\newa -> [MkWholeReaderEdit newa]) mnewa
        }

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

unitWholeUpdateFunction :: UpdateFunction edit (WholeUpdate ())
unitWholeUpdateFunction = constUpdateFunction ()

pairWholeUpdateFunction ::
       forall update a b.
       UpdateFunction update (WholeUpdate a)
    -> UpdateFunction update (WholeUpdate b)
    -> UpdateFunction update (WholeUpdate (a, b))
pairWholeUpdateFunction =
    joinRunnable2Maps $ \(MkAnUpdateFunction ga ua :: AnUpdateFunction tt _ _) (MkAnUpdateFunction gb ub) -> let
        gab :: ReadFunctionTT tt (UpdateReader update) (WholeReader (a, b))
        gab (mr :: MutableRead m _) ReadWhole =
            case transStackDict @MonadIO @tt @m of
                Dict -> do
                    a <- ga mr ReadWhole
                    b <- gb mr ReadWhole
                    return (a, b)
        uab :: forall m. MonadIO m
            => update
            -> MutableRead m (UpdateReader update)
            -> ApplyStack tt m [WholeUpdate (a, b)]
        uab update mr =
            case transStackDict @MonadIO @tt @m of
                Dict -> do
                    weas <- ua update mr
                    webs <- ub update mr
                    case (lastWholeUpdate weas, lastWholeUpdate webs) of
                        (Nothing, Nothing) -> return []
                        (ma, mb) -> do
                            a <-
                                case ma of
                                    Just a -> return a
                                    Nothing -> ga mr ReadWhole
                            b <-
                                case mb of
                                    Just b -> return b
                                    Nothing -> gb mr ReadWhole
                            return [MkWholeUpdate (a, b)]
        in MkAnUpdateFunction gab uab
