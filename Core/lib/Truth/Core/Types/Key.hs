module Truth.Core.Types.Key where
{
    import Truth.Core.Import;
    import Data.UUID;
    import Truth.Core.Sequence;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.OneReader;
    import Truth.Core.Types.OneEdit;
    import Truth.Core.Types.Sum;
    import Truth.Core.Types.Whole;
    import Truth.Core.Types.OneWholeEdit;
    import Truth.Core.Types.Pair;


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

    instance (KeyContainer cont,IOFullReader reader,ReaderSubject reader ~ Element cont) => IOFullReader (KeyReader cont reader) where
    {
        ioFromReader = do
        {
            allkeys <- readable KeyReadKeys;
            list <- traverse (\key -> mapReadable (knownKeyItemReadFunction key) ioFromReader) allkeys;
            return $ fromElementList list;
        };
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
        info = mkSimpleInfo $(ionamedwitness[t|KeyReader|]) [$(declInfo [d|
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
        KeyInsertReplaceItem :: Element cont -> KeyEdit cont edit;
        KeyClear :: KeyEdit cont edit;
    };

    class (Reader reader,ReaderSubject reader ~ Element cont) => HasKeyReader cont reader where
    {
        readKey :: proxy cont -> Readable reader (ContainerKey cont);
    };

    instance HasInfo HasKeyReader where
    {
        info = mkSimpleInfo $(ionamedwitness[t|HasKeyReader|]) [];
    };

    instance (EditSubject keyedit ~ key,EditSubject valedit ~ val,Edit keyedit,FullReader (EditReader keyedit),Edit valedit) =>
        HasKeyReader [(key,val)] (PairEditReader keyedit valedit) where
    {
        readKey _ = mapReadable firstReadFunction fromReader;
    };

    instance Floating (KeyEdit cont edit) (KeyEdit cont edit);

    replace :: Eq a => a -> a -> [a] -> [a];
    replace _ _ [] = [];
    replace old new (a:aa) | old == a = new:aa;
    replace old new (a:aa) = a : (replace old new aa);

    instance (KeyContainer cont,IOFullReader (EditReader edit),Edit edit,HasKeyReader cont (EditReader edit)) => Edit (KeyEdit cont edit) where
    {
        type EditReader (KeyEdit cont edit) = KeyReader cont (EditReader edit);

        applyEdit (KeyEditItem oldkey edit) kreader@(KeyReadItem key reader) = do
        {
            mnewkey <- mapReadableF (keyItemReadFunction oldkey) $ mapReadable (applyEdit edit) $ readKey (Proxy :: Proxy cont); -- the edit may change the element's key
            case mnewkey of
            {
                Just newkey | key == newkey -> mapReadableF (keyItemReadFunction key) $ applyEdit edit reader;
                _ -> if key == oldkey then return Nothing else readable kreader;
            }
        };
        applyEdit (KeyEditItem oldkey edit) KeyReadKeys = do
        {
            oldkeys <- readable KeyReadKeys;
            mnewkey <- mapReadableF (keyItemReadFunction oldkey) $ mapReadable (applyEdit edit) $ readKey (Proxy :: Proxy cont); -- the edit may change the element's key
            return $ case mnewkey of
            {
                Just newkey -> replace oldkey newkey oldkeys;
                _ -> oldkeys;
            }
        };
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
            molditem <- mapReadableF (keyItemReadFunction newkey) ioFromReader;
            case molditem of
            {
                Just olditem -> return [KeyInsertReplaceItem olditem];
                Nothing -> return [KeyDeleteItem newkey];
            }
        };
        invertEdit (KeyDeleteItem key) = do
        {
            ma <- mapReadableF (keyItemReadFunction key) ioFromReader;
            case ma of
            {
                Just a -> return [KeyInsertReplaceItem a];
                Nothing -> return [];
            };
        };
        invertEdit KeyClear = ioWriterToReadable ioReplaceEdit;
    };

    instance (KeyContainer cont,IOFullReader (EditReader edit),Edit edit,HasKeyReader cont (EditReader edit)) => IOFullEdit (KeyEdit cont edit) where
    {
        ioReplaceEdit = do
        {
            wrWrite KeyClear;
            allkeys <- readable KeyReadKeys;
            let
            {
                readWriteItem :: ContainerKey cont -> IOWriterReadable (KeyEdit cont edit) (KeyReader cont (EditReader edit)) ();
                readWriteItem key = do
                {
                    item <- mapReadable (knownKeyItemReadFunction key) $ readableToM ioFromReader;
                    wrWrite $ KeyInsertReplaceItem item;
                };
            };
            traverse_ readWriteItem allkeys;
        };
    };

    instance (KeyContainer cont,FullReader (EditReader edit),Edit edit,HasKeyReader cont (EditReader edit)) => FullEdit (KeyEdit cont edit) where
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
        info = mkSimpleInfo $(ionamedwitness[t|KeyEdit|]) [$(declInfo [d|
            instance Eq key => KeyContainer [(key, value)];
            type instance Element [a] = a;
            type instance ContainerKey [(key, value)] = key;
            instance Show UUID;
            instance Floating (KeyEdit cont edit) (KeyEdit cont edit);
            instance (KeyContainer cont,FullReader (EditReader edit),Edit edit,HasKeyReader cont (EditReader edit)) => Edit (KeyEdit cont edit) where
            {
                type EditReader (KeyEdit cont edit) = KeyReader cont (EditReader edit);
            };
            instance (KeyContainer cont,FullReader (EditReader edit),Edit edit,HasKeyReader cont (EditReader edit)) => FullEdit (KeyEdit cont edit);
            instance (HasNewValue value) => IONewItemKeyContainer [(UUID, value)];
            instance (EditSubject keyedit ~ key,EditSubject valedit ~ val,Edit keyedit,FullReader (EditReader keyedit),Edit valedit) =>
                HasKeyReader [(key,val)] (PairEditReader keyedit valedit);
        |])];
    };

    keyLens :: forall cont edit. (KeyContainer cont,Eq (ContainerKey cont),HasKeyReader cont (EditReader edit),Edit edit) =>
        ContainerKey cont -> FloatingEditLens' Identity (ContainerKey cont) (KeyEdit cont edit) (OneWholeEdit Maybe edit);
    keyLens floatingEditInitial = let
    {
        floatingEditGet :: ContainerKey cont -> ReadFunction (KeyReader cont (EditReader edit)) (OneReader Maybe (EditReader edit));
        floatingEditGet key ReadHasOne = do
        {
            kk <- readable $ KeyReadKeys;
            return $ if elem key kk then Just () else Nothing;
        };
        floatingEditGet key (ReadOne rt) = readable $ KeyReadItem key rt;

        floatingEditUpdate :: KeyEdit cont edit -> ContainerKey cont -> Readable (KeyReader cont (EditReader edit)) (ContainerKey cont,[OneWholeEdit Maybe edit]);
        floatingEditUpdate KeyClear key = return (key,[SumEditLeft (MkWholeEdit Nothing)]);
        floatingEditUpdate (KeyDeleteItem k) key | k == key = return (key,[SumEditLeft (MkWholeEdit Nothing)]);
        floatingEditUpdate (KeyEditItem k edit) oldkey | k == oldkey = do
        {
            mnewkey <- mapReadableF (keyItemReadFunction oldkey) $ mapReadable (applyEdit edit) $ readKey (Proxy::Proxy cont);
            return $ case mnewkey of
            {
                Just newkey -> (newkey,[SumEditRight (MkOneEdit edit)]);
                Nothing -> (oldkey,[]);
            }
        };
        floatingEditUpdate (KeyInsertReplaceItem item) key | elementKey (Proxy::Proxy cont) item == key = return (key,[SumEditLeft (MkWholeEdit (Just item))]);
        floatingEditUpdate _ key = return (key,[]);

        floatingEditLensFunction = MkFloatingEditFunction{..};

        floatingEditLensPutEdit key (SumEditLeft (MkWholeEdit (Just subj))) = return $ pure (key,[KeyInsertReplaceItem subj]);
        floatingEditLensPutEdit key (SumEditLeft (MkWholeEdit Nothing)) = return $ pure (key,[KeyDeleteItem key]);
        floatingEditLensPutEdit oldkey (SumEditRight (MkOneEdit edit)) = do
        {
            mnewkey <- mapReadableF (keyItemReadFunction oldkey) $ mapReadable (applyEdit edit) $ readKey (Proxy::Proxy cont);
            return $ pure $ case mnewkey of
            {
                Just newkey -> (newkey,[KeyEditItem oldkey edit]);
                Nothing -> (oldkey,[]);
            }
        };
    } in MkFloatingEditLens{..};
}
