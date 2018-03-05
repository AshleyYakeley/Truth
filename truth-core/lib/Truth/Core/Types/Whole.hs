{-# OPTIONS -fno-warn-orphans #-}

module Truth.Core.Types.Whole where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read

data WholeReader (a :: *) (t :: *) where
    ReadWhole :: forall t. WholeReader t t

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

newtype WholeReaderEdit (reader :: * -> *) =
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

instance (FullSubjectReader reader) => FullEdit (WholeReaderEdit reader) where
    replaceEdit mr write = do
        a <- mutableReadToSubject mr
        write $ MkWholeEdit a

lastWholeEdit :: [WholeReaderEdit reader] -> Maybe (ReaderSubject reader)
lastWholeEdit edits = do
    MkWholeEdit subj <- lastM edits
    return subj

type WholeEdit a = WholeReaderEdit (WholeReader a)

wholeEditFunction :: forall a b. (a -> b) -> EditFunction (WholeEdit a) (WholeEdit b)
wholeEditFunction ab =
    MkCloseUnlift identityUnlift $
    MkAnEditFunction
        { efGet = \mr ReadWhole -> lift $ fmap ab $ mr ReadWhole
        , efUpdate = \(MkWholeEdit a) _ -> return [MkWholeEdit $ ab a]
        }

wholeEditLens ::
       forall mf a b. (MonadOne mf)
    => Lens' mf a b
    -> EditLens (WholeEdit a) (WholeEdit b)
wholeEditLens lens =
    MkCloseUnlift identityUnlift $
    MkAnEditLens
        { elFunction =
              MkAnEditFunction
                  { efGet = \mr ReadWhole -> lift $ fmap (lensGet lens) $ mr ReadWhole
                  , efUpdate = \(MkWholeEdit a) _ -> return [MkWholeEdit $ lensGet lens a]
                  }
        , elPutEdits =
              elPutEditsFromPutEdit $ \(MkWholeEdit b) mr ->
                  lift $ do
                      olda <- mr ReadWhole
                      return $ fmap (\newa -> [MkWholeEdit newa]) $ getMaybeOne $ lensPutback lens b olda
        }

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
