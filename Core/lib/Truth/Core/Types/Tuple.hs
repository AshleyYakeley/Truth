module Truth.Core.Types.Tuple where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;


    class TupleWitness (c :: * -> Constraint) (sel :: * -> *) where
    {
        tupleWitness :: forall proxy edit. proxy c -> sel edit -> ConstraintWitness (c edit);
    };

    newtype Tuple sel = MkTuple (forall edit. sel edit -> EditSubject edit);

    class (TestEquality sel,TupleReaderWitness SubjectReader sel) => SubjectTupleSelector (sel :: * -> *) where
    {
        type TupleSubject sel :: *;
        type TupleSubject sel = Tuple sel;

        tupleReadFromSubject :: forall edit. sel edit -> TupleSubject sel -> EditSubject edit;
        default tupleReadFromSubject :: forall edit. (TupleSubject sel ~ Tuple sel) => sel edit -> TupleSubject sel -> EditSubject edit;
        tupleReadFromSubject sel (MkTuple tuple) = tuple sel;
    };

    data TupleEditReader sel t where
    {
        MkTupleEditReader :: sel edit -> EditReader edit t -> TupleEditReader sel t;
    };

    tupleReadFunction :: sel edit -> ReadFunction (TupleEditReader sel) (EditReader edit);
    tupleReadFunction sel r = readable $ MkTupleEditReader sel r;

    instance (SubjectTupleSelector sel) => SubjectReader (TupleEditReader sel) where
    {
        type ReaderSubject (TupleEditReader sel) = TupleSubject sel;
        readFromSubject a (MkTupleEditReader seledit reader) = case tupleReaderWitness (Proxy::Proxy SubjectReader) seledit of
        {
            MkConstraintWitness -> readFromSubject (tupleReadFromSubject seledit a) reader;
        };
    };

    class TupleReaderWitness (c :: (* -> *) -> Constraint) (sel :: * -> *) where
    {
        tupleReaderWitness :: forall proxy edit. proxy c -> sel edit -> ConstraintWitness (c (EditReader edit));
    };

    class TupleSubjectWitness (c :: * -> Constraint) (sel :: * -> *) where
    {
        tupleSubjectWitness :: forall proxy edit. proxy c -> sel edit -> ConstraintWitness (c (EditSubject edit));
    };

    class TestEquality sel => FiniteTupleSelector (sel :: * -> *) where
    {
        tupleConstruct :: forall m. Applicative m => (forall edit. sel edit -> m (EditSubject edit)) -> m (TupleSubject sel);
    };

    tupleAllSelectors :: FiniteTupleSelector sel => [AnyWitness sel];
    tupleAllSelectors = getConst $ tupleConstruct $ \sel -> Const [MkAnyWitness sel];

    instance (SubjectTupleSelector sel,FiniteTupleSelector sel,TupleReaderWitness FullSubjectReader sel) => FullSubjectReader (TupleEditReader sel) where
    {
        subjectFromReader = tupleConstruct (\(seledit :: sel edit) -> case tupleReaderWitness (Proxy::Proxy FullSubjectReader) seledit of
        {
            MkConstraintWitness -> mapReadable (readable . MkTupleEditReader seledit) subjectFromReader;
        });
    };


    data TupleEdit sel where
    {
        MkTupleEdit :: sel edit -> edit -> TupleEdit sel;
    };

    instance (TestEquality sel,TupleWitness Edit sel) => Floating (TupleEdit sel) (TupleEdit sel) where
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

    instance (TestEquality sel,TupleWitness Edit sel) => Edit (TupleEdit sel) where
    {
        type EditReader (TupleEdit sel) = TupleEditReader sel;

        applyEdit (MkTupleEdit aggedite edit) aggreader@(MkTupleEditReader aggeditr reader) =
            case (tupleWitness (Proxy::Proxy Edit) aggedite,testEquality aggedite aggeditr) of
            {
                (MkConstraintWitness,Just Refl) -> mapReadable (readable . MkTupleEditReader aggedite) (applyEdit edit reader);
                _ -> readable aggreader;
            };
    };

    instance (TestEquality sel,TupleWitness Edit sel,TupleWitness InvertableEdit sel) => InvertableEdit (TupleEdit sel) where
    {
        invertEdit (MkTupleEdit seledit edit) = case tupleWitness (Proxy::Proxy InvertableEdit) seledit of
        {
            MkConstraintWitness -> fmap (fmap (MkTupleEdit seledit))
                (mapReadable (readable . MkTupleEditReader seledit) (invertEdit edit));
        };
    };

    instance (SubjectTupleSelector sel,FiniteTupleSelector sel,TupleReaderWitness FullSubjectReader sel,TupleWitness Edit sel,TupleWitness FullEdit sel) => FullEdit (TupleEdit sel) where
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

    splitTupleEditList :: TestEquality w => [TupleEdit w] -> AllF w [];
    splitTupleEditList [] = MkAllF $ \_ -> [];
    splitTupleEditList ((MkTupleEdit wt t):rr) = MkAllF $ \wt' -> case testEquality wt wt' of
    {
        Just Refl -> t:(getAllF (splitTupleEditList rr) wt');
        Nothing -> getAllF (splitTupleEditList rr) wt';
    };

    tupleEditFunction ::  forall sel edit. TestEquality sel => sel edit -> PureEditFunction (TupleEdit sel) edit;
    tupleEditFunction seledit = let
    {
        editAccess :: IOStateAccess ();
        editAccess = unitStateAccess;
        editGet :: () -> ReadFunction (TupleEditReader sel) (EditReader edit);
        editGet () = tupleReadFunction seledit;
        editUpdate (MkTupleEdit seledit' edit) () = case testEquality seledit seledit' of
        {
            Just Refl -> return ((),[edit]);
            _ -> return ((),[]);
        };
    } in MkEditFunction{..};

    tupleEditLens :: forall sel edit. (TestEquality sel) =>
        sel edit -> PureEditLens (TupleEdit sel) edit;
    tupleEditLens seledit = let
    {
        editLensFunction = tupleEditFunction seledit;
        editLensPutEdit :: () -> edit -> Readable (TupleEditReader sel) (Maybe ((),[TupleEdit sel]));
        editLensPutEdit () edit = return $ pure ((),[MkTupleEdit seledit edit]);
    } in MkEditLens{..};

    tupleGeneralLens :: forall sel edit. (TestEquality sel) =>
        sel edit -> GeneralLens (TupleEdit sel) edit;
    tupleGeneralLens seledit = MkCloseState $ tupleEditLens seledit;
}
