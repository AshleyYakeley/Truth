module Truth.Core.Types.Key where
{
    import Truth.Core.Import;
    import Truth.Core.Sequence;
    import Truth.Core.Read;
    import Truth.Core.Edit;


    data KeyReader cont reader t where
    {
        KeyReadKeys :: KeyReader cont reader [ContainerKey cont];
        KeyReadItem :: ContainerKey cont -> reader t -> KeyReader cont reader (Maybe t);
    };

    keyItemReadFunction :: ContainerKey cont -> ReadFunctionF Maybe (KeyReader cont reader) reader;
    keyItemReadFunction key reader = readable $ KeyReadItem key reader;

    knownKeyItemReadFunction :: ContainerKey cont -> ReadFunction (KeyReader cont reader) reader;
    knownKeyItemReadFunction key reader = do
    {
        mt <- keyItemReadFunction key reader;
        case mt of
        {
            Just t -> return t;
            Nothing -> fail $ "missing item in list";
        };
    };

    instance (KeyContainer cont,Reader reader,ReaderSubject reader ~ Element cont) => Reader (KeyReader cont reader) where
    {
        type ReaderSubject (KeyReader cont reader) = cont;

        readFrom cont KeyReadKeys = keys cont;
        readFrom cont (KeyReadItem key reader) = fmap (\e -> readFrom e reader) $ lookupElement key cont;
    };

    instance (KeyContainer cont,FullReader reader,ReaderSubject reader ~ Element cont) => FullReader (KeyReader cont reader) where
    {
        fromReader = do
        {
            allkeys <- readable KeyReadKeys;
            list <- traverse (\key -> mapReadable (knownKeyItemReadFunction key) fromReader) allkeys;
            return $ fromElementList list;
        };
    };

    $(return []);
    instance HasInfo KeyReader where
    {
        info = mkSimpleInfo $(iowitness[t|KeyReader|]) [$(declInfo [d|
            instance (KeyContainer cont,Reader reader,ReaderSubject reader ~ Element cont) => Reader (KeyReader cont reader) where
            {
                type ReaderSubject (KeyReader cont reader) = cont;
            };
            instance (KeyContainer cont,FullReader reader,ReaderSubject reader ~ Element cont) => FullReader (KeyReader cont reader);
        |])];
    };

    data KeyEdit cont edit where
    {
        KeyEditItem :: ContainerKey cont -> edit -> KeyEdit cont edit;
        KeyDeleteItem :: ContainerKey cont -> KeyEdit cont edit;
        KeyInsertReplaceItem :: EditSubject edit -> KeyEdit cont edit;
        KeyClear :: KeyEdit cont edit;
    };

    instance Floating (KeyEdit cont edit) (KeyEdit cont edit);

    instance (KeyContainer cont,FullReader (EditReader edit),Edit edit,EditSubject edit ~ Element cont) => Edit (KeyEdit cont edit) where
    {
        type EditReader (KeyEdit cont edit) = KeyReader cont (EditReader edit);

        applyEdit (KeyEditItem key' edit) (KeyReadItem key reader) | key' == key = mapReadableF (keyItemReadFunction key) $ applyEdit edit reader;
        applyEdit (KeyEditItem _ _) reader = readable reader;
        applyEdit (KeyDeleteItem key) KeyReadKeys = do
        {
            allkeys <- readable KeyReadKeys;
            return $ delete key allkeys;
        };
        applyEdit (KeyDeleteItem key') (KeyReadItem key _reader) | key' == key = return Nothing;
        applyEdit (KeyDeleteItem _) (KeyReadItem key reader) = readable $ KeyReadItem key reader;
        applyEdit (KeyInsertReplaceItem item) KeyReadKeys = do
        {
            allkeys <- readable KeyReadKeys;
            let
            {
                newkey = elementKey (Proxy :: Proxy cont) item;
            };
            if elem newkey allkeys then return allkeys else return $ newkey:allkeys;
        };
        applyEdit (KeyInsertReplaceItem item) (KeyReadItem key reader) | elementKey (Proxy @cont) item == key = return $ Just $ readFrom item reader;
        applyEdit (KeyInsertReplaceItem _) (KeyReadItem key reader) = readable $ KeyReadItem key reader;
        applyEdit KeyClear reader = readFromM (return mempty) reader;

        invertEdit (KeyEditItem p edit) = do
        {
            minvedits <- mapReadableF (keyItemReadFunction p) $ invertEdit edit;
            case minvedits of
            {
                Just invedits -> return $ fmap (KeyEditItem p) invedits;
                Nothing -> return [];
            }
        };
        invertEdit (KeyInsertReplaceItem item) = do
        {
            let
            {
                newkey = elementKey (Proxy :: Proxy cont) item;
            };
            molditem <- mapReadableF (keyItemReadFunction newkey) fromReader;
            case molditem of
            {
                Just olditem -> return [KeyInsertReplaceItem olditem];
                Nothing -> return [KeyDeleteItem newkey];
            }
        };
        invertEdit (KeyDeleteItem key) = do
        {
            ma <- mapReadableF (keyItemReadFunction key) fromReader;
            case ma of
            {
                Just a -> return [KeyInsertReplaceItem a];
                Nothing -> return [];
            };
        };
        invertEdit KeyClear = writerToReadable replaceEdit;
    };

    instance (KeyContainer cont,FullReader (EditReader edit),Edit edit,EditSubject edit ~ Element cont) => FullEdit (KeyEdit cont edit) where
    {
        replaceEdit = do
        {
            wrWrite KeyClear;
            allkeys <- readable KeyReadKeys;
            let
            {
                readWriteItem :: ContainerKey cont -> WriterReadable (KeyEdit cont edit) (KeyReader cont (EditReader edit)) ();
                readWriteItem key = do
                {
                    item <- mapReadable (knownKeyItemReadFunction key) $ readableToM fromReader;
                    wrWrite $ KeyInsertReplaceItem item;
                };
            };
            traverse_ readWriteItem allkeys;
        };
    };

    $(return []);
    instance HasInfo KeyEdit where
    {
        info = mkSimpleInfo $(iowitness[t|KeyEdit|]) [$(declInfo [d|
            instance Floating (KeyEdit cont edit) (KeyEdit cont edit);
            instance (KeyContainer cont,FullReader (EditReader edit),Edit edit,EditSubject edit ~ Element cont) => Edit (KeyEdit cont edit) where
            {
                type EditReader (KeyEdit cont edit) = KeyReader cont (EditReader edit);
            };
            instance (KeyContainer cont,FullReader (EditReader edit),Edit edit,EditSubject edit ~ Element cont) => FullEdit (KeyEdit cont edit);
        |])];
    };
}
