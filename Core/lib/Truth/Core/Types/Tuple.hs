module Truth.Core.Types.Tuple where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;


    class TestEquality sel => TupleSelector (sel :: * -> *) where
    {
        type TupleSubject sel :: *;
        tupleIsEdit :: forall edit. sel edit -> ConstraintWitness (Edit edit);
        tupleReadFrom :: forall edit. sel edit -> TupleSubject sel -> EditSubject edit;
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
        readFrom a (MkTupleEditReader seledit reader) = case tupleIsEdit seledit of
        {
            MkConstraintWitness -> readFrom (tupleReadFrom seledit a) reader;
        };
    };

    class TupleSelector sel => FiniteTupleSelector (sel :: * -> *) where
    {
        tupleIsFullReader :: forall edit. sel edit -> ConstraintWitness (FullReader (EditReader edit));
        tupleConstruct :: forall m. Applicative m => (forall edit. sel edit -> m (EditSubject edit)) -> m (TupleSubject sel);
    };

    tupleAllSelectors :: FiniteTupleSelector sel => [AnyWitness sel];
    tupleAllSelectors = getConstant $ tupleConstruct $ \sel -> Constant [MkAnyWitness sel];

    instance (FiniteTupleSelector sel) => FullReader (TupleEditReader sel) where
    {
        fromReader = tupleConstruct (\(seledit :: sel edit) -> case tupleIsFullReader seledit of
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
            case (tupleIsEdit aggedite,testEquality aggedite aggeditr) of
            {
                (MkConstraintWitness,Just Refl) -> mapCleanReadable (MkTupleEditReader aggedite) (applyEdit edit reader);
                _ -> readable aggreader;
            };

        invertEdit (MkTupleEdit seledit edit) = case tupleIsEdit seledit of
        {
            MkConstraintWitness -> fmap (fmap (MkTupleEdit seledit))
                (mapCleanReadable (MkTupleEditReader seledit) (invertEdit edit));
        };
    };

    class FiniteTupleSelector sel => FullTupleSelector sel where
    {
        tupleIsFullEdit :: forall edit. sel edit -> ConstraintWitness (FullEdit edit);
    };

    instance FullTupleSelector sel => FullEdit (TupleEdit sel) where
    {
        replaceEdit = do
        {
            editss <- traverse (\(MkAnyWitness sel) -> case tupleIsFullEdit sel of
            {
                MkConstraintWitness -> reWriterReadable (MkTupleEdit sel) $ mapReadable (tupleReadFunction sel) replaceEdit;
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
