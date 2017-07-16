module Truth.Core.Types.Tuple where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;


    class TupleWitness (c :: * -> Constraint) (sel :: * -> *) where
    {
        tupleWitness :: forall proxy edit. proxy c -> sel edit -> ConstraintWitness (c edit);
    };

    $(return []);
    instance HasInfo TupleWitness where
    {
        info = mkSimpleInfo $(ionamedwitness[t|TupleWitness|]) [$(declInfo [d|
        |])];
    };

    class (TestEquality sel,TupleWitness Edit sel) => TupleSelector (sel :: * -> *) where
    {
        type TupleSubject sel :: *;
        tupleReadFrom :: forall edit. sel edit -> TupleSubject sel -> EditSubject edit;
    };
    $(typeFamilyProxy "TupleSubject");

    $(return []);
    instance HasInfo TupleSelector where
    {
        info = mkSimpleInfo $(ionamedwitness[t|TupleSelector|]) [$(declInfo [d|
        |])];
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
        readFrom a (MkTupleEditReader seledit reader) = case tupleWitness (Proxy::Proxy Edit) seledit of
        {
            MkConstraintWitness -> readFrom (tupleReadFrom seledit a) reader;
        };
    };

    class TupleReaderWitness (c :: (* -> *) -> Constraint) (sel :: * -> *) where
    {
        tupleReaderWitness :: forall proxy edit. proxy c -> sel edit -> ConstraintWitness (c (EditReader edit));
    };

    $(return []);
    instance HasInfo TupleReaderWitness where
    {
        info = mkSimpleInfo $(ionamedwitness[t|TupleReaderWitness|]) [$(declInfo [d|
        |])];
    };

    class TupleSelector sel => FiniteTupleSelector (sel :: * -> *) where
    {
        tupleConstruct :: forall m. Applicative m => (forall edit. sel edit -> m (EditSubject edit)) -> m (TupleSubject sel);
    };
    instance HasInfo FiniteTupleSelector where
    {
        info = mkSimpleInfo $(ionamedwitness[t|FiniteTupleSelector|]) [$(declInfo [d|
        |])];
    };

    tupleAllSelectors :: FiniteTupleSelector sel => [AnyWitness sel];
    tupleAllSelectors = getConstant $ tupleConstruct $ \sel -> Constant [MkAnyWitness sel];

    instance (FiniteTupleSelector sel,TupleReaderWitness IOFullReader sel) => IOFullReader (TupleEditReader sel) where
    {
        ioFromReader = tupleConstruct (\(seledit :: sel edit) -> case tupleReaderWitness (Proxy::Proxy IOFullReader) seledit of
        {
            MkConstraintWitness -> mapCleanReadable (MkTupleEditReader seledit) ioFromReader;
        });
    };

    instance (FiniteTupleSelector sel,TupleReaderWitness IOFullReader sel,TupleReaderWitness FullReader sel) => FullReader (TupleEditReader sel) where
    {
        fromReader = tupleConstruct (\(seledit :: sel edit) -> case tupleReaderWitness (Proxy::Proxy FullReader) seledit of
        {
            MkConstraintWitness -> mapCleanReadable (MkTupleEditReader seledit) fromReader;
        });
    };

    $(return []);
    instance HasInfo TupleEditReader where
    {
        info = mkSimpleInfo $(ionamedwitness[t|TupleEditReader|]) [$(declInfo [d|
            instance (TupleSelector sel) => Reader (TupleEditReader sel) where
            {
                type ReaderSubject (TupleEditReader sel) = TupleSubject sel;
            };
            instance (FiniteTupleSelector sel,TupleReaderWitness IOFullReader sel) => IOFullReader (TupleEditReader sel);
            instance (FiniteTupleSelector sel,TupleReaderWitness IOFullReader sel,TupleReaderWitness FullReader sel) => FullReader (TupleEditReader sel);
        |])];
    };


    data TupleEdit sel where
    {
        MkTupleEdit :: sel edit -> edit -> TupleEdit sel;
    };

    instance TupleSelector sel => Floating (TupleEdit sel) (TupleEdit sel) where
    {
        floatingUpdate (MkTupleEdit s1 e1) edit@(MkTupleEdit s2 e2) = case testEquality s1 s2 of
        {
            Just Refl -> case tupleWitness (Proxy::Proxy Edit) s2 of
            {
                MkConstraintWitness -> MkTupleEdit s2 $ floatingUpdate e1 e2;
            };
            Nothing -> edit;
        };
    };

    instance TupleSelector sel => Edit (TupleEdit sel) where
    {
        type EditReader (TupleEdit sel) = TupleEditReader sel;

        applyEdit (MkTupleEdit aggedite edit) aggreader@(MkTupleEditReader aggeditr reader) =
            case (tupleWitness (Proxy::Proxy Edit) aggedite,testEquality aggedite aggeditr) of
            {
                (MkConstraintWitness,Just Refl) -> mapCleanReadable (MkTupleEditReader aggedite) (applyEdit edit reader);
                _ -> readable aggreader;
            };

        invertEdit (MkTupleEdit seledit edit) = case tupleWitness (Proxy::Proxy Edit) seledit of
        {
            MkConstraintWitness -> fmap (fmap (MkTupleEdit seledit))
                (mapCleanReadable (MkTupleEditReader seledit) (invertEdit edit));
        };
    };

    instance (FiniteTupleSelector sel,TupleReaderWitness IOFullReader sel,TupleWitness IOFullEdit sel) => IOFullEdit (TupleEdit sel) where
    {
        ioReplaceEdit = do
        {
            editss <- traverse (\(MkAnyWitness sel) -> case tupleWitness (Proxy::Proxy IOFullEdit) sel of
            {
                MkConstraintWitness -> reWriterReadable (MkTupleEdit sel) $ mapReadable (tupleReadFunction sel) ioReplaceEdit;
            }) tupleAllSelectors;
            return $ mconcat editss;
        };
    };

    instance (FiniteTupleSelector sel,TupleReaderWitness IOFullReader sel,TupleReaderWitness FullReader sel,TupleWitness IOFullEdit sel,TupleWitness FullEdit sel) => FullEdit (TupleEdit sel) where
    {
        replaceEdit = do
        {
            editss <- traverse (\(MkAnyWitness sel) -> case tupleWitness (Proxy::Proxy FullEdit) sel of
            {
                MkConstraintWitness -> reWriterReadable (MkTupleEdit sel) $ mapReadable (tupleReadFunction sel) replaceEdit;
            }) tupleAllSelectors;
            return $ mconcat editss;
        };
    };

    $(return []);
    instance HasInfo TupleEdit where
    {
        info = mkSimpleInfo $(ionamedwitness[t|TupleEdit|]) [$(declInfo [d|
            instance TupleSelector sel => Edit (TupleEdit sel) where
            {
                type EditReader (TupleEdit sel) = TupleEditReader sel;
            };
            --instance FullTupleSelector sel => FullEdit (TupleEdit sel);
        |])];
    };

    class TupleSelector sel => TupleHasInfo (sel :: * -> *) where
    {
        tupleHasInfo :: forall edit. sel edit -> Info edit;
    };

    $(return []);
    instance HasInfo TupleHasInfo where
    {
        info = mkSimpleInfo $(ionamedwitness[t|TupleHasInfo|]) [$(declInfo [d|
        |])];
    };

    tupleCleanEditLens :: TestEquality sel => sel edit -> CleanEditLens' Identity (TupleEdit sel) edit;
    tupleCleanEditLens seledit = MkCleanEditLens
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
        cleanEditLensPutEdit = Identity . pure . (MkTupleEdit seledit)
    };
}
