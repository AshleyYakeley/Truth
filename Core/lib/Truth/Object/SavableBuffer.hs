module Truth.Object.SavableBuffer where
{
    --import Truth.Object.Object;
    import Truth.Edit;
    import Truth.Edit.Import;

    data SavableVersion = SavableOriginal | SavableCurrent deriving Eq;

    instance Countable SavableVersion where
    {
        countPrevious = finiteCountPrevious;
        countMaybeNext = finiteCountMaybeNext;
    };

    instance Searchable SavableVersion where
    {
        search = finiteSearch;
    };

    instance Finite SavableVersion where
    {
        allValues = [SavableOriginal,SavableCurrent];
    };

    type Savable a = SavableVersion -> a;

    mkSavable :: a -> a -> Savable a;
    mkSavable a _ SavableOriginal = a;
    mkSavable _ a SavableCurrent = a;

    savableVersionLens :: SavableVersion -> Lens' Identity (Savable a) a;
    savableVersionLens = pickLens;

    data SavableReader reader t = MkSavableReader SavableVersion (reader t);

    instance (Reader reader) => Reader (SavableReader reader) where
    {
        type ReaderSubject (SavableReader reader) = Savable (ReaderSubject reader);
        readFrom subj (MkSavableReader sv reader) = readFrom (subj sv) reader;
    };

    instance (FullReader reader) => FullReader (SavableReader reader) where
    {
        fromReader = do
        {
            so <- mapCleanReadable (MkSavableReader SavableOriginal) fromReader;
            sc <- mapCleanReadable (MkSavableReader SavableCurrent) fromReader;
            return (mkSavable so sc);
        };
    };

    data SavableEdit edit =
        SEEdit edit |   -- changes Current
        SESave | -- sets Original to Current
        SEUnsave (EditSubject edit); -- sets Original to given (invert SESave)

    instance Floating (SavableEdit edit) where
    {
        type FloatingEdit (SavableEdit edit) = SavableEdit edit;
    };

    instance (Eq (EditSubject edit), FullReader (EditReader edit), Edit edit) => Edit (SavableEdit edit) where
    {
        type EditReader (SavableEdit edit) = SavableReader (EditReader edit);
        applyEdit (SEEdit edit) (MkSavableReader SavableCurrent reader) =
            mapCleanReadable (MkSavableReader SavableCurrent) (applyEdit edit reader);
        applyEdit SESave (MkSavableReader SavableOriginal reader) =
            mapCleanReadable (MkSavableReader SavableCurrent) (readable reader);
        applyEdit (SEUnsave a) (MkSavableReader SavableOriginal reader) = return (readFrom a reader);
        applyEdit _ sreader = readable sreader;

        invertEdit (SEEdit edit) = fmap (fmap SEEdit)
            (mapCleanReadable (MkSavableReader SavableCurrent) (invertEdit edit));
        invertEdit SESave = do
        {
            so <- mapCleanReadable (MkSavableReader SavableOriginal) fromReader;
            return (Just (SEUnsave so));
        };
        invertEdit (SEUnsave _) = do
        {
            sav <- fromReader;
            return (Just (let
            {
                so = sav SavableOriginal;
                sc = sav SavableCurrent;
            } in if so == sc then SESave else SEUnsave so));
        };
    };

    savableLens :: (Applicative m) => Lens' m a b -> Lens' m (Savable a) (Savable b);
    savableLens = cfmap;

    --objSubscribe :: forall edit. ((edit -> IO ()) -> IO (Object edit)) -> Object edit;

    --savable :: (FullEdit edit) => Object edit -> Object (SavableEdit edit);
    --savable sub = objSubscribe foo

    --    foo :: (SavableEdit edit -> IO ()) -> IO (Object (SavableEdit edit))

{-
    data FloatingEditLens' m state edita editb = MkFloatingEditLens
    {
        floatingEditLensSimple :: FloatingLens' m state (ReaderSubject edita) (ReaderSubject editb),
        floatingEditLensUpdate :: edita -> state -> ConstFunction (ReaderSubject edita) (state,Maybe editb),
        floatingEditLensPutEdit :: state -> editb -> ConstFunction (ReaderSubject edita) (m (state,edita))    -- m failure means impossible
    };

    savableLens :: FloatingEditLens' Identity (Maybe (ReaderSubject edit)) edit (SavableEdit edit);
    savableLens = MkFloatingEditLens
    {

    };
-}
}
