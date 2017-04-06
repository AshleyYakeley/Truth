module Truth.Core.Types.Tuple where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;


    class TestEquality sel => TupleSelector (sel :: * -> *) where
    {
        type TupleSubject sel :: *;
        tupleIsFullReaderEdit :: forall edit. sel edit -> ConstraintWitness (FullReader (EditReader edit),Edit edit);
        tupleReadFrom :: forall edit. sel edit -> TupleSubject sel -> EditSubject edit;
        tupleConstruct :: forall m. Monad m => (forall edit. sel edit -> m (EditSubject edit)) -> m (TupleSubject sel);
    };

    data TupleEditReader sel t where
    {
        MkTupleEditReader :: sel edit -> EditReader edit t -> TupleEditReader sel t;
    };

    tupleReadFunction :: sel edit -> ReadFunction (TupleEditReader sel) (EditReader edit);
    tupleReadFunction sel r = readable $ MkTupleEditReader sel r;

    instance (TupleSelector sel) => Reader (TupleEditReader sel) where
    {
        type ReaderSubject (TupleEditReader sel) = TupleSubject sel;
        readFrom a (MkTupleEditReader seledit reader) = case tupleIsFullReaderEdit seledit of
        {
            MkConstraintWitness -> readFrom (tupleReadFrom seledit a) reader;
        };
    };

    instance (TupleSelector sel) => FullReader (TupleEditReader sel) where
    {
        fromReader = tupleConstruct (\(seledit :: sel edit) -> case tupleIsFullReaderEdit seledit of
        {
            MkConstraintWitness -> mapCleanReadable (MkTupleEditReader seledit) fromReader;
        });
    };


    data TupleEdit sel where
    {
        MkTupleEdit :: sel edit -> edit -> TupleEdit sel;
    };

    instance Floating (TupleEdit sel) (TupleEdit sel);

    instance (TupleSelector sel) => Edit (TupleEdit sel) where
    {
        type EditReader (TupleEdit sel) = TupleEditReader sel;

        applyEdit (MkTupleEdit aggedite edit) aggreader@(MkTupleEditReader aggeditr reader) =
            case (tupleIsFullReaderEdit aggedite,testEquality aggedite aggeditr) of
            {
                (MkConstraintWitness,Just Refl) -> mapCleanReadable (MkTupleEditReader aggedite) (applyEdit edit reader);
                _ -> readable aggreader;
            };

        invertEdit (MkTupleEdit seledit edit) = case tupleIsFullReaderEdit seledit of
        {
            MkConstraintWitness -> fmap (fmap (MkTupleEdit seledit))
                (mapCleanReadable (MkTupleEditReader seledit) (invertEdit edit));
        };
    };

    class TupleSelector sel => FiniteTupleSelector sel where
    {
        tupleAllSelectors :: [AnyWitness sel];
        tupleIsFullEdit :: forall edit. sel edit -> ConstraintWitness (FullEdit edit);
    };

    instance FiniteTupleSelector sel => FullEdit (TupleEdit sel) where
    {
        replaceEdit = do
        {
            editss <- traverse (\(MkAnyWitness sel) -> case tupleIsFullEdit sel of
            {
                MkConstraintWitness -> do
                {
                    edits <- mapReadable (tupleReadFunction sel) $ replaceEdit;
                    return $ fmap (MkTupleEdit sel) edits;
                };
            }) tupleAllSelectors;
            return $ mconcat editss;
        };
    };

    tupleLens :: TestEquality sel => sel edit -> CleanEditLens' Identity (TupleEdit sel) edit;
    tupleLens seledit = MkCleanEditLens
    {
        cleanEditLensFunction = MkCleanEditFunction
        {
            cleanEditGet = MkTupleEditReader seledit,
            cleanEditUpdate = \(MkTupleEdit seledit' edit) -> case testEquality seledit seledit' of
            {
                Just Refl -> [edit];
                _ -> [];
            }
        },
        cleanEditLensPutEdit = Identity . (MkTupleEdit seledit)
    };
}
