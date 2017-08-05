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
    instance HasTypeInfo TupleWitness where
    {
        typeWitness = $(generateWitness [t|TupleWitness|]);
        typeName _ = "TupleWitness";
    };

    newtype Tuple sel = MkTuple (forall edit. sel edit -> EditSubject edit);

    $(return []);
    instance HasTypeInfo Tuple where
    {
        typeWitness = $(generateWitness [t|Tuple|]);
        typeName _ = "Tuple";
    };

    class (TestEquality sel,TupleWitness Edit sel) => TupleSelector (sel :: * -> *) where
    {
        type TupleSubject sel :: *;
        type TupleSubject sel = Tuple sel;

        tupleReadFrom :: forall edit. sel edit -> TupleSubject sel -> EditSubject edit;
        default tupleReadFrom :: forall edit. (TupleSubject sel ~ Tuple sel) => sel edit -> TupleSubject sel -> EditSubject edit;
        tupleReadFrom sel (MkTuple tuple) = tuple sel;
    };
    $(generateFamilyProxy "TupleSubject");

    $(return []);
    instance HasTypeInfo TupleSelector where
    {
        typeWitness = $(generateWitness [t|TupleSelector|]);
        typeName _ = "TupleSelector";
    };

    data TupleEditReader sel t where
    {
        MkTupleEditReader :: sel edit -> EditReader edit t -> TupleEditReader sel t;
    };

    tupleReadFunction :: sel edit -> PureReadFunction (TupleEditReader sel) (EditReader edit);
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
    instance HasTypeInfo TupleReaderWitness where
    {
        typeWitness = $(generateWitness [t|TupleReaderWitness|]);
        typeName _ = "TupleReaderWitness";
    };

    class TupleSelector sel => FiniteTupleSelector (sel :: * -> *) where
    {
        tupleConstruct :: forall m. Applicative m => (forall edit. sel edit -> m (EditSubject edit)) -> m (TupleSubject sel);
    };
    instance HasTypeInfo FiniteTupleSelector where
    {
        typeWitness = $(generateWitness [t|FiniteTupleSelector|]);
        typeName _ = "FiniteTupleSelector";
    };

    tupleAllSelectors :: FiniteTupleSelector sel => [AnyWitness sel];
    tupleAllSelectors = getConstant $ tupleConstruct $ \sel -> Constant [MkAnyWitness sel];

    instance (FiniteTupleSelector sel,ReadableConstraint c,TupleReaderWitness (FullReader c) sel) => FullReader c (TupleEditReader sel) where
    {
        fromReader = tupleConstruct (\(seledit :: sel edit) -> case tupleReaderWitness (Proxy::Proxy (FullReader c)) seledit of
        {
            MkConstraintWitness -> mapReadable (readable . MkTupleEditReader seledit) fromReader;
        });
    };

    $(return []);
    instance HasTypeInfo TupleEditReader where
    {
        typeWitness = $(generateWitness [t|TupleEditReader|]);
        typeName _ = "TupleEditReader";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance (TupleSelector sel) => Reader (TupleEditReader sel) where
            {
                type ReaderSubject (TupleEditReader sel) = TupleSubject sel;
            };
            instance (FiniteTupleSelector sel,ReadableConstraint c,TupleReaderWitness (FullReader c) sel) => FullReader c (TupleEditReader sel);
        |]);
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
                (MkConstraintWitness,Just Refl) -> mapReadable (readable . MkTupleEditReader aggedite) (applyEdit edit reader);
                _ -> readable aggreader;
            };

        invertEdit (MkTupleEdit seledit edit) = case tupleWitness (Proxy::Proxy Edit) seledit of
        {
            MkConstraintWitness -> fmap (fmap (MkTupleEdit seledit))
                (mapReadable (readable . MkTupleEditReader seledit) (invertEdit edit));
        };
    };

    instance (FiniteTupleSelector sel,ReadableConstraint c,TupleReaderWitness (FullReader c) sel,TupleWitness (FullEdit c) sel) => FullEdit c (TupleEdit sel) where
    {
        replaceEdit = do
        {
            editss <- traverse (\(MkAnyWitness sel) -> case tupleWitness (Proxy::Proxy (FullEdit c)) sel of
            {
                MkConstraintWitness -> reWriterReadable (MkTupleEdit sel) $ mapReadable (tupleReadFunction sel) replaceEdit;
            }) tupleAllSelectors;
            return $ mconcat editss;
        };
    };

    $(return []);
    instance HasTypeInfo TupleEdit where
    {
        typeWitness = $(generateWitness [t|TupleEdit|]);
        typeName _ = "TupleEdit";
        typeKnowledge _ = mconcat [
            typeInfoKnowledge (typeInfo @TupleEditReader),
            $(generateTypeKnowledge [d|
            instance TupleSelector sel => Edit (TupleEdit sel) where
            {
                type EditReader (TupleEdit sel) = TupleEditReader sel;
            };
            instance (FiniteTupleSelector sel,ReadableConstraint c,TupleReaderWitness (FullReader c) sel,TupleWitness (FullEdit c) sel) => FullEdit c (TupleEdit sel);
        |])];
    };

    class TupleSelector sel => TupleHasInfo (sel :: * -> *) where
    {
        tupleHasInfo :: forall edit. sel edit -> TypeInfo edit;
    };

    $(return []);
    instance HasTypeInfo TupleHasInfo where
    {
        typeWitness = $(generateWitness [t|TupleHasInfo|]);
        typeName _ = "TupleHasInfo";
    };

    tupleEditLens :: forall c m sel edit. (TestEquality sel,Applicative m) =>
        sel edit -> EditLens' c m () (TupleEdit sel) edit;
    tupleEditLens seledit = let
    {
        editInitial = ();
        editGet :: () -> ReadFunction c (TupleEditReader sel) (EditReader edit);
        editGet () rt = readable $ MkTupleEditReader seledit rt;
        editUpdate (MkTupleEdit seledit' edit) () = case testEquality seledit seledit' of
        {
            Just Refl -> return ((),[edit]);
            _ -> return ((),[]);
        };
        editLensFunction = MkEditFunction{..};
        editLensPutEdit :: () -> edit -> Readable c (TupleEditReader sel) (m ((),[TupleEdit sel]));
        editLensPutEdit () edit = return $ pure ((),[MkTupleEdit seledit edit]);
    } in MkEditLens{..};
}
