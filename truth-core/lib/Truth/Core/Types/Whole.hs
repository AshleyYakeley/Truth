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
    MkWholeEdit (ReaderSubject reader)

instance Show (ReaderSubject reader) => Show (WholeReaderEdit reader) where
    show (MkWholeEdit subj) = "whole " ++ show subj

instance Floating (WholeReaderEdit reader) (WholeReaderEdit reader)

type instance EditReader (WholeReaderEdit reader) = reader

instance (FullSubjectReader reader) => ApplicableEdit (WholeReaderEdit reader) where
    applyEdit (MkWholeEdit a) _ = subjectToMutableRead a

instance (FullSubjectReader reader) => InvertibleEdit (WholeReaderEdit reader) where
    invertEdit _ mr = do
        a <- mutableReadToSubject mr
        return [MkWholeEdit a]

instance FullSubjectReader reader => SubjectMapEdit (WholeReaderEdit reader)

instance (FullSubjectReader reader) => FullEdit (WholeReaderEdit reader) where
    replaceEdit mr write = do
        a <- mutableReadToSubject mr
        write $ MkWholeEdit a

instance (FullSubjectReader reader, TestEquality reader) => CacheableEdit (WholeReaderEdit reader)

mapWholeEdit :: (ReaderSubject ra -> ReaderSubject rb) -> WholeReaderEdit ra -> WholeReaderEdit rb
mapWholeEdit = coerce

lastWholeEdit :: [WholeReaderEdit reader] -> Maybe (ReaderSubject reader)
lastWholeEdit edits = do
    MkWholeEdit subj <- lastM edits
    return subj

wholePutEdits ::
       (MonadTrans t, Monad m)
    => (ReaderSubject reader -> MutableRead m (EditReader edita) -> t m (Maybe [edita]))
    -> [WholeReaderEdit reader]
    -> MutableRead m (EditReader edita)
    -> t m (Maybe [edita])
wholePutEdits pe edits mr =
    case lastWholeEdit edits of
        Nothing -> lift $ return $ Just []
        (Just subj) -> pe subj mr

type WholeEdit a = WholeReaderEdit (WholeReader a)

wholeEditFunction :: forall a b. (a -> b) -> EditFunction (WholeEdit a) (WholeEdit b)
wholeEditFunction ab =
    MkCloseUnlift identityUnlift $
    MkAnEditFunction
        { efGet = \mr ReadWhole -> lift $ fmap ab $ mr ReadWhole
        , efUpdate = \(MkWholeEdit a) _ -> return [MkWholeEdit $ ab a]
        }

ioWholeEditFunction :: forall a b. (a -> IO b) -> EditFunction (WholeEdit a) (WholeEdit b)
ioWholeEditFunction aiob =
    MkCloseUnlift identityUnlift $
    MkAnEditFunction
        { efGet =
              \mr ReadWhole ->
                  lift $ do
                      a <- mr ReadWhole
                      liftIO $ aiob a
        , efUpdate =
              \(MkWholeEdit a) _ -> do
                  b <- liftIO $ aiob a
                  return [MkWholeEdit b]
        }

changeOnlyEditFunction ::
       forall a. Eq a
    => IO (EditFunction (WholeEdit a) (WholeEdit a))
changeOnlyEditFunction = do
    var <- newMVar Nothing
    let
        efGet :: ReadFunctionT (StateT (Maybe a)) (WholeReader a) (WholeReader a)
        efGet mr ReadWhole = do
            ma <- get
            case ma of
                Just a -> return a
                Nothing -> do
                    a <- lift $ mr ReadWhole
                    put $ Just a
                    return a
        efUpdate ::
               forall m. MonadIO m
            => WholeEdit a
            -> MutableRead m (EditReader (WholeEdit a))
            -> StateT (Maybe a) m [WholeEdit a]
        efUpdate (MkWholeEdit newa) _ = do
            molda <- get
            case molda of
                Just olda
                    | olda == newa -> return []
                _ -> do
                    put $ Just newa
                    return [MkWholeEdit newa]
    return $ MkCloseUnlift (mvarUnlift var) $ MkAnEditFunction {..}

ioWholeEditLens :: forall a b. (a -> IO b) -> (b -> a -> IO (Maybe a)) -> EditLens (WholeEdit a) (WholeEdit b)
ioWholeEditLens ioget ioput =
    MkCloseUnlift identityUnlift $
    MkAnEditLens
        { elFunction =
              MkAnEditFunction
                  { efGet =
                        \mr ReadWhole ->
                            lift $ do
                                a <- mr ReadWhole
                                liftIO $ ioget a
                  , efUpdate =
                        \(MkWholeEdit a) _ -> do
                            b <- liftIO $ ioget a
                            return [MkWholeEdit b]
                  }
        , elPutEdits =
              elPutEditsFromPutEdit $ \(MkWholeEdit b) mr ->
                  lift $ do
                      olda <- mr ReadWhole
                      mnewa <- liftIO $ ioput b olda
                      return $ fmap (\newa -> [MkWholeEdit newa]) mnewa
        }

wholeEditLens ::
       forall mf a b. (MonadOne mf)
    => Lens' mf a b
    -> EditLens (WholeEdit a) (WholeEdit b)
wholeEditLens lens =
    ioWholeEditLens (\a -> return $ lensGet lens a) (\b olda -> return $ getMaybeOne $ lensPutback lens b olda)

bijectionWholeEditLens :: Bijection a b -> EditLens (WholeEdit a) (WholeEdit b)
bijectionWholeEditLens = wholeEditLens . bijectionLens

instance MonadOne m => IsEditLens (Lens' m a b) where
    type LensDomain (Lens' m a b) = WholeEdit a
    type LensRange (Lens' m a b) = WholeEdit b
    toEditLens = toEditLens . wholeEditLens

instance MonadOne m => IsEditLens (Injection' m a b) where
    type LensDomain (Injection' m a b) = WholeEdit a
    type LensRange (Injection' m a b) = WholeEdit b
    toEditLens = toEditLens . injectionLens

instance IsEditLens (Bijection a b) where
    type LensDomain (Bijection a b) = WholeEdit a
    type LensRange (Bijection a b) = WholeEdit b
    toEditLens = toEditLens . bijectionInjection

instance IsEditLens (Codec a b) where
    type LensDomain (Codec a b) = WholeEdit a
    type LensRange (Codec a b) = WholeEdit (Maybe b)
    toEditLens = toEditLens . codecInjection

unitWholeEditFunction :: EditFunction edit (WholeEdit ())
unitWholeEditFunction = constEditFunction ()

pairWholeEditFunction ::
       forall edit a b.
       EditFunction edit (WholeEdit a)
    -> EditFunction edit (WholeEdit b)
    -> EditFunction edit (WholeEdit (a, b))
pairWholeEditFunction (MkCloseUnlift (ula :: Unlift ta) (MkAnEditFunction ga ua)) (MkCloseUnlift (ulb :: Unlift tb) (MkAnEditFunction gb ub)) = let
    gab :: ReadFunctionT (ComposeT ta tb) (EditReader edit) (WholeReader (a, b))
    gab mr ReadWhole =
        withTransConstraintTM @MonadIO $ do
            a <- lift1ComposeT $ ga mr ReadWhole
            b <- lift2ComposeT'' $ gb mr ReadWhole
            return (a, b)
    uab :: forall m. MonadIO m
        => edit
        -> MutableRead m (EditReader edit)
        -> ComposeT ta tb m [WholeEdit (a, b)]
    uab edit mr =
        case hasTransConstraint @MonadIO @tb @m of
            Dict ->
                case hasTransConstraint @MonadIO @ta @(tb m) of
                    Dict -> do
                        weas <- lift1ComposeT $ ua edit mr
                        webs <- lift2ComposeT $ ub edit mr
                        case (lastWholeEdit weas, lastWholeEdit webs) of
                            (Nothing, Nothing) -> return []
                            (ma, mb) -> do
                                a <-
                                    case ma of
                                        Just a -> return a
                                        Nothing -> lift1ComposeT $ ga mr ReadWhole
                                b <-
                                    case mb of
                                        Just b -> return b
                                        Nothing -> lift2ComposeT $ gb mr ReadWhole
                                return [MkWholeEdit (a, b)]
    in MkCloseUnlift (composeUnlift ula ulb) $ MkAnEditFunction gab uab
