module Truth.Core.Types.List where
{
    import Truth.Core.Import;
    import Truth.Core.Sequence;
    import Truth.Core.Read;
    import Truth.Core.Edit;


    data ListReader seq reader t where
    {
        ListReadLength :: ListReader seq reader (SequencePoint seq);
        ListReadItem :: SequencePoint seq -> reader t -> ListReader seq reader (Maybe t);
    };

    itemReadFunction :: SequencePoint seq -> ReadFunctionF Maybe (ListReader seq reader) reader;
    itemReadFunction i reader = readable $ ListReadItem i reader;

    knownItemReadFunction :: Integral (Index seq) => SequencePoint seq -> ReadFunction (ListReader seq reader) reader;
    knownItemReadFunction i reader = do
    {
        mt <- itemReadFunction i reader;
        case mt of
        {
            Just t -> return t;
            Nothing -> fail $ "missing item "++ show i ++" in list";
        };
    };

    instance (IsSequence seq,Reader reader,ReaderSubject reader ~ Element seq) => Reader (ListReader seq reader) where
    {
        type ReaderSubject (ListReader seq reader) = seq;

        readFrom sq ListReadLength = seqLength sq;
        readFrom sq (ListReadItem i reader) = fmap (\e -> readFrom e reader) $ seqIndex sq i;
    };

    instance (IsSequence seq,IOFullReader reader,ReaderSubject reader ~ Element seq) => IOFullReader (ListReader seq reader) where
    {
        ioFromReader = do
        {
            len <- readable ListReadLength;
            list <- traverse (\i -> mapReadable (knownItemReadFunction i) ioFromReader) [0..pred len];
            return $ fromList list;
        };
    };

    instance (IsSequence seq,FullReader reader,ReaderSubject reader ~ Element seq) => FullReader (ListReader seq reader) where
    {
        fromReader = do
        {
            len <- readable ListReadLength;
            list <- traverse (\i -> mapReadable (knownItemReadFunction i) fromReader) [0..pred len];
            return $ fromList list;
        };
    };

    $(return []);
    instance HasTypeInfo ListReader where
    {
        typeWitness = $(generateWitness [t|ListReader|]);
        typeName _ = "ListReader";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance (IsSequence seq,Reader reader,ReaderSubject reader ~ Element seq) => Reader (ListReader seq reader) where
            {
                type ReaderSubject (ListReader seq reader) = seq;
            };
            instance (IsSequence seq,FullReader reader,ReaderSubject reader ~ Element seq) => FullReader (ListReader seq reader);
        |]);
    };

    data ListEdit seq edit where
    {
        ListEditItem :: SequencePoint seq -> edit -> ListEdit seq edit;
        ListDeleteItem :: SequencePoint seq -> ListEdit seq edit;
        ListInsertItem :: SequencePoint seq -> EditSubject edit -> ListEdit seq edit;
        ListClear :: ListEdit seq edit;
    };

    instance (Enum (Index seq),Ord (Index seq)) => Floating (ListEdit seq edit) (SequencePoint seq) where
    {
        floatingUpdate (ListDeleteItem p) i | p < i = pred i;
        floatingUpdate (ListInsertItem p _) i | p <= i = succ i;
        floatingUpdate _ i = i;
    };

    instance (Enum (Index seq),Ord (Index seq)) => Floating (ListEdit seq edit) (ListEdit seq edit) where
    {
        floatingUpdate edit (ListEditItem i e) = ListEditItem (floatingUpdate edit i) e;
        floatingUpdate edit (ListDeleteItem i) = ListDeleteItem (floatingUpdate edit i);
        floatingUpdate edit (ListInsertItem i a) = ListInsertItem (floatingUpdate edit i) a;
        floatingUpdate _edit ListClear = ListClear;
    };

    instance (IsSequence seq,IOFullReader (EditReader edit),Edit edit,EditSubject edit ~ Element seq) => Edit (ListEdit seq edit) where
    {
        type EditReader (ListEdit seq edit) = ListReader seq (EditReader edit);

        applyEdit (ListEditItem p edit) (ListReadItem i reader) | p == i = mapReadableF (itemReadFunction i) $ applyEdit edit reader; -- already checks bounds
        applyEdit (ListEditItem _ _) reader = readable reader;
        applyEdit (ListDeleteItem p) ListReadLength = do
        {
            len <- readable ListReadLength;
            return $ if p >= 0 && p < len then len - 1 else len;
        };
        applyEdit (ListDeleteItem p) (ListReadItem i reader) | p >=0 && p < i = readable $ ListReadItem (i + 1) reader;
        applyEdit (ListDeleteItem _) (ListReadItem i reader) = readable $ ListReadItem i reader;
        applyEdit (ListInsertItem p _) ListReadLength = do
        {
            len <- readable ListReadLength;
            return $ if p >= 0 && p <= len then len + 1 else len;
        };
        applyEdit (ListInsertItem p a) (ListReadItem i reader) | p == i = do
        {
            len <- readable ListReadLength;
            return $ if p >= 0 && p <= len then Just $ readFrom a reader else Nothing;
        };
        applyEdit (ListInsertItem p _) (ListReadItem i reader) | p >= 0 && p < i = readable $ ListReadItem (i - 1) reader;
        applyEdit (ListInsertItem _ _) (ListReadItem i reader) = readable $ ListReadItem i reader;
        applyEdit ListClear reader = readFromM (return mempty) reader;

        invertEdit (ListEditItem p edit) = do
        {
            minvedits <- mapReadableF (itemReadFunction p) $ invertEdit edit;
            case minvedits of
            {
                Just invedits -> return $ fmap (ListEditItem p) invedits;
                Nothing -> return [];
            }
        };
        invertEdit (ListInsertItem p _) = do
        {
            len <- readable ListReadLength;
            return $ if p >= 0 && p <= len then [ListDeleteItem p] else [];
        };
        invertEdit (ListDeleteItem p) = do
        {
            ma <- mapReadableF (itemReadFunction p) ioFromReader;
            case ma of
            {
                Just a -> return [ListInsertItem p a];
                Nothing -> return [];
            };
        };
        invertEdit ListClear = ioWriterToReadable ioReplaceEdit;
    };

    instance (IsSequence seq,IOFullReader (EditReader edit),Edit edit,EditSubject edit ~ Element seq) => IOFullEdit (ListEdit seq edit) where
    {
        ioReplaceEdit = do
        {
            wrWrite ListClear;
            len <- readable ListReadLength;
            let
            {
                readWriteItem :: SequencePoint seq -> IOWriterReadable (ListEdit seq edit) (ListReader seq (EditReader edit)) ();
                readWriteItem i = do
                {
                    item <- mapReadable (knownItemReadFunction i) $ readableToM ioFromReader;
                    wrWrite $ ListInsertItem i item;
                };
            };
            traverse_ readWriteItem [0..pred len];
        };
    };

    instance (IsSequence seq,FullReader (EditReader edit),Edit edit,EditSubject edit ~ Element seq) => FullEdit (ListEdit seq edit) where
    {
        replaceEdit = do
        {
            wrWrite ListClear;
            len <- readable ListReadLength;
            let
            {
                readWriteItem :: SequencePoint seq -> WriterReadable (ListEdit seq edit) (ListReader seq (EditReader edit)) ();
                readWriteItem i = do
                {
                    item <- mapReadable (knownItemReadFunction i) $ readableToM fromReader;
                    wrWrite $ ListInsertItem i item;
                };
            };
            traverse_ readWriteItem [0..pred len];
        };
    };

    $(return []);
    instance HasTypeInfo ListEdit where
    {
        typeWitness = $(generateWitness [t|ListEdit|]);
        typeName _ = "ListEdit";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance (Enum (Index seq),Ord (Index seq)) => Floating (ListEdit seq edit) (SequencePoint seq);
            instance (Enum (Index seq),Ord (Index seq)) => Floating (ListEdit seq edit) (ListEdit seq edit);
            instance (IsSequence seq,FullReader (EditReader edit),Edit edit,EditSubject edit ~ Element seq) => Edit (ListEdit seq edit) where
            {
                type EditReader (ListEdit seq edit) = ListReader seq (EditReader edit);
            };
            instance (IsSequence seq,FullReader (EditReader edit),Edit edit,EditSubject edit ~ Element seq) => FullEdit (ListEdit seq edit);
        |]);
    };
}
