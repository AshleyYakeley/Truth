module Truth.Core.Types.Key where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.OneReader;
    import Truth.Core.Types.OneEdit;
    import Truth.Core.Types.Sum;
    import Truth.Core.Types.Whole;
    import Truth.Core.Types.OneWholeEdit;
    import Truth.Core.Types.Tuple;
    import Truth.Core.Types.Pair;
    import Truth.Core.Types.Context;


    data KeyReader cont reader t where
    {
        KeyReadKeys :: KeyReader cont reader (FiniteSet (ContainerKey cont));
        KeyReadItem :: ContainerKey cont -> reader t -> KeyReader cont reader (Maybe t);
    };

    keyItemReadFunction :: forall c cont reader. ContainerKey cont -> ReadFunctionF c Maybe (KeyReader cont reader) reader;
    keyItemReadFunction key reader = readable $ KeyReadItem key reader;

    knownKeyItemReadFunction :: forall c cont reader. ContainerKey cont -> ReadFunction c (KeyReader cont reader) reader;
    knownKeyItemReadFunction key reader = do
    {
        mt <- keyItemReadFunction key reader;
        case mt of
        {
            Just t -> return t;
            Nothing -> error $ "missing item in list";
        };
    };

    instance (KeyContainer cont,Reader reader,ReaderSubject reader ~ Element cont) => Reader (KeyReader cont reader) where
    {
        type ReaderSubject (KeyReader cont reader) = cont;

        readFrom cont KeyReadKeys = MkFiniteSet $ keys cont;
        readFrom cont (KeyReadItem key reader) = fmap (\e -> readFrom e reader) $ lookupElement key cont;
    };

    instance (KeyContainer cont,ReadableConstraint c,FullReader c reader,ReaderSubject reader ~ Element cont) => FullReader c (KeyReader cont reader) where
    {
        fromReader = do
        {
            MkFiniteSet allkeys <- readable KeyReadKeys;
            list <- traverse (\key -> mapReadable (knownKeyItemReadFunction key) fromReader) allkeys;
            return $ fromElementList list;
        };
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
        readKey :: proxy cont -> PureReadable reader (ContainerKey cont);
    };

    instance (EditSubject keyedit ~ key,EditSubject valedit ~ val,Edit keyedit,PureFullReader (EditReader keyedit),Edit valedit) =>
        HasKeyReader [(key,val)] (PairEditReader keyedit valedit) where
    {
        readKey _ = mapReadable firstReadFunction pureFromReader;
    };

    instance HasKeyReader (FiniteSet t) (WholeReader t) where
    {
        readKey _ = readable ReadWhole;
    };

    instance Floating (KeyEdit cont edit) (KeyEdit cont edit);

    replace :: Eq a => a -> a -> FiniteSet a -> FiniteSet a;
    replace _ _ (MkFiniteSet []) = mempty;
    replace old new (MkFiniteSet (a:aa)) | old == a = MkFiniteSet $ new:aa;
    replace old new (MkFiniteSet (a:aa)) = MkFiniteSet $ a : (unFiniteSet $ replace old new $ MkFiniteSet aa);

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
            return $ deleteSet key allkeys;
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
            if elem newkey allkeys then return allkeys else return $ insertSet newkey allkeys;
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
        invertEdit KeyClear = writerToReadable ioReplaceEdit;
    };

    instance (KeyContainer cont,ReadableConstraint c,IOFullReader (EditReader edit),FullReader c (EditReader edit),Edit edit,HasKeyReader cont (EditReader edit)) => FullEdit c (KeyEdit cont edit) where
    {
        replaceEdit = do
        {
            wrWrite KeyClear;
            allkeys <- readable KeyReadKeys;
            let
            {
                readWriteItem :: ContainerKey cont -> WriterReadable c (KeyEdit cont edit) (KeyReader cont (EditReader edit)) ();
                readWriteItem key = do
                {
                    item <- case selfWriterReadable @c @(KeyEdit cont edit) @(EditReader edit) of
                    {
                        MkConstraintWitness -> mapReadable (knownKeyItemReadFunction key) $ readableToM @c fromReader;
                    };
                    wrWrite $ KeyInsertReplaceItem item;
                };
            };
            traverse_ readWriteItem allkeys;
        };
    };

    keyElementLens :: forall cont edit. (KeyContainer cont,HasKeyReader cont (EditReader edit),Edit edit) =>
        ContainerKey cont -> GeneralLens (KeyEdit cont edit) (OneWholeEdit Maybe edit);
    keyElementLens editInitial = let
    {
        editGet :: ContainerKey cont -> PureReadFunction (KeyReader cont (EditReader edit)) (OneReader Maybe (EditReader edit));
        editGet key ReadHasOne = do
        {
            kk <- readable $ KeyReadKeys;
            return $ if elem key kk then Just () else Nothing;
        };
        editGet key (ReadOne rt) = readable $ KeyReadItem key rt;

        editUpdate :: KeyEdit cont edit -> ContainerKey cont -> PureReadable (KeyReader cont (EditReader edit)) (ContainerKey cont,[OneWholeEdit Maybe edit]);
        editUpdate KeyClear key = return (key,[SumEditLeft (MkWholeEdit Nothing)]);
        editUpdate (KeyDeleteItem k) key | k == key = return (key,[SumEditLeft (MkWholeEdit Nothing)]);
        editUpdate (KeyEditItem k edit) oldkey | k == oldkey = do
        {
            mnewkey <- mapReadableF (keyItemReadFunction oldkey) $ mapReadable (applyEdit edit) $ readKey (Proxy::Proxy cont);
            return $ case mnewkey of
            {
                Just newkey -> (newkey,[SumEditRight (MkOneEdit edit)]);
                Nothing -> (oldkey,[]);
            }
        };
        editUpdate (KeyInsertReplaceItem item) key | elementKey (Proxy::Proxy cont) item == key = return (key,[SumEditLeft (MkWholeEdit (Just item))]);
        editUpdate _ key = return (key,[]);

        editLensFunction = MkEditFunction{..};

        editLensPutEdit key (SumEditLeft (MkWholeEdit (Just subj))) = return $ pure (key,[KeyInsertReplaceItem subj]);
        editLensPutEdit key (SumEditLeft (MkWholeEdit Nothing)) = return $ pure (key,[KeyDeleteItem key]);
        editLensPutEdit oldkey (SumEditRight (MkOneEdit edit)) = do
        {
            mnewkey <- mapReadableF (keyItemReadFunction oldkey) $ mapReadable (applyEdit edit) $ readKey (Proxy::Proxy cont);
            return $ pure $ case mnewkey of
            {
                Just newkey -> (newkey,[KeyEditItem oldkey edit]);
                Nothing -> (oldkey,[]);
            }
        };

        lens :: PureEditLens' Identity (ContainerKey cont) (KeyEdit cont edit) (OneWholeEdit Maybe edit);
        lens = MkEditLens{..};
    } in toGeneralLens lens;

    keyValueLens :: forall cont keyedit valueedit.
        (
            KeyContainer cont,
            HasKeyReader cont (PairEditReader keyedit valueedit),
            Edit keyedit,
            IOFullReader (EditReader keyedit),
            IOFullEdit valueedit
        ) => ContainerKey cont -> GeneralLens (KeyEdit cont (PairEdit keyedit valueedit)) (OneWholeEdit Maybe valueedit);
    keyValueLens key = (oneWholeLiftGeneralLens $ tupleEditLens EditSecond) <.> keyElementLens key;

    liftKeyElementLens :: forall c f state conta contb edita editb.
        (
            ReadableConstraint c,
            Applicative f,
            ContainerKey conta ~ ContainerKey contb,
            EditSubject edita ~ Element conta,
            EditSubject editb ~ Element contb,
            Reader (EditReader edita),
            FullReader c (EditReader editb)
        ) =>
        (forall m. (Monad m,c m) => EditSubject editb -> m (f (EditSubject edita))) ->
        EditLens' c f state edita editb -> EditLens' c f state (KeyEdit conta edita) (KeyEdit contb editb);
    liftKeyElementLens bma (MkEditLens (MkEditFunction editInitial g u) pe) = let
    {
        editGet :: state -> ReadFunction c (KeyReader conta (EditReader edita)) (KeyReader contb (EditReader editb));
        editGet _ KeyReadKeys = readable KeyReadKeys;
        editGet curstate (KeyReadItem key rt) = mapReadableF (keyItemReadFunction key) $ g curstate rt;

        editUpdate :: KeyEdit conta edita -> state -> Readable c (KeyReader conta (EditReader edita)) (state,[KeyEdit contb editb]);
        editUpdate KeyClear oldstate = return (oldstate,[KeyClear]);
        editUpdate (KeyInsertReplaceItem itema) oldstate = do
        {
            itemb <- liftReadable $ fromReadFunctionM (g oldstate) (return itema);
            return (oldstate,[KeyInsertReplaceItem itemb]);
        };

        editUpdate (KeyDeleteItem key) oldstate = return (oldstate,[KeyDeleteItem key]);
        editUpdate (KeyEditItem key ea) oldstate = do
        {
            mresult <- mapReadableF (keyItemReadFunction @Monad @conta key) $ u ea oldstate;
            case mresult of
            {
                Just (newstate,ebs) -> return (newstate,fmap (KeyEditItem key) ebs);
                Nothing -> return (oldstate,[]);
            };
        };

        editLensPutEdit :: state -> KeyEdit contb editb -> Readable c (KeyReader conta (EditReader edita)) (f (state,[KeyEdit conta edita]));
        editLensPutEdit oldstate KeyClear = return $ pure (oldstate,[KeyClear]);
        editLensPutEdit oldstate (KeyInsertReplaceItem itemb) = do
        {
            fitema <- liftReadable $ bma itemb;
            return $ fmap (\itema -> (oldstate,[KeyInsertReplaceItem $ itema])) fitema;
        };
        editLensPutEdit oldstate (KeyDeleteItem key) = return $ pure (oldstate,[KeyDeleteItem key]);
        editLensPutEdit oldstate (KeyEditItem key eb) = do
        {
            mfresult <- mapReadableF (keyItemReadFunction @Monad @conta key) $ pe oldstate eb;
            case mfresult of
            {
                Just fsea -> return $ fmap (fmap (fmap $ KeyEditItem key)) fsea;
                Nothing -> return $ pure (oldstate,[]);
            };
        };

        editLensFunction = MkEditFunction{..};
    } in MkEditLens{..};

    contextKeyLens' :: forall c editx cont edit.
        EditLens' c Maybe () (ContextEdit editx (KeyEdit cont edit)) (KeyEdit cont (ContextEdit editx edit));
    contextKeyLens' = let
    {
        editInitial = ();

        editGet :: () -> KeyReader cont (ContextEditReader editx edit) t -> Readable c (ContextEditReader editx (KeyEdit cont edit)) t;
        editGet () KeyReadKeys = readable $ MkTupleEditReader EditContent KeyReadKeys;
        editGet () (KeyReadItem _ (MkTupleEditReader EditContext reader)) = fmap Just $ readable $ MkTupleEditReader EditContext reader;
        editGet () (KeyReadItem key (MkTupleEditReader EditContent reader)) = readable $ MkTupleEditReader EditContent $ KeyReadItem key reader;

        editUpdate :: ContextEdit editx (KeyEdit cont edit) -> () -> Readable c (ContextEditReader editx (KeyEdit cont edit)) ((), [KeyEdit cont (ContextEdit editx edit)]);
        editUpdate (MkTupleEdit EditContext edit) () = do
        {
            MkFiniteSet kk <- readable $ MkTupleEditReader EditContent KeyReadKeys;
            return $ pure $ fmap (\key -> KeyEditItem key $ MkTupleEdit EditContext edit) kk;
        };
        editUpdate (MkTupleEdit EditContent (KeyEditItem key edit)) () = return $ pure [KeyEditItem key (MkTupleEdit EditContent edit)];
        editUpdate (MkTupleEdit EditContent (KeyDeleteItem key)) () = return $ pure [KeyDeleteItem key];
        editUpdate (MkTupleEdit EditContent (KeyInsertReplaceItem el)) () = return $ pure [KeyInsertReplaceItem el];
        editUpdate (MkTupleEdit EditContent KeyClear) () = return $ pure [KeyClear];

        editLensFunction = MkEditFunction{..};

        editLensPutEdit :: () -> KeyEdit cont (ContextEdit editx edit) -> Readable c (ContextEditReader editx (KeyEdit cont edit)) (Maybe ((), [ContextEdit editx (KeyEdit cont edit)]));
        editLensPutEdit () (KeyEditItem _ (MkTupleEdit EditContext edit)) = return $ pure $ pure [MkTupleEdit EditContext edit];
        editLensPutEdit () (KeyEditItem key (MkTupleEdit EditContent edit)) = return $ pure $ pure [MkTupleEdit EditContent $ KeyEditItem key edit];
        editLensPutEdit () (KeyDeleteItem key) = return $ pure $ pure [MkTupleEdit EditContent $ KeyDeleteItem key];
        editLensPutEdit () (KeyInsertReplaceItem el) = return $ pure $ pure [MkTupleEdit EditContent $ KeyInsertReplaceItem el];
        editLensPutEdit () KeyClear = return $ pure $ pure [MkTupleEdit EditContent KeyClear];
    } in MkEditLens {..};

    contextKeyLens :: GeneralLens (ContextEdit editx (KeyEdit cont edit)) (KeyEdit cont (ContextEdit editx edit));
    contextKeyLens = MkCloseState contextKeyLens';
}
