module Truth.Core.Types.Key where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Sequence

--import Truth.Core.Types.Context
import Truth.Core.Types.List
import Truth.Core.Types.OneEdit
import Truth.Core.Types.OneReader
import Truth.Core.Types.OneWholeEdit
import Truth.Core.Types.Pair
import Truth.Core.Types.Sum
import Truth.Core.Types.Tuple
import Truth.Core.Types.Whole

data KeyReader cont reader t where
    KeyReadKeys :: KeyReader cont reader (FiniteSet (ContainerKey cont))
    KeyReadItem :: ContainerKey cont -> reader t -> KeyReader cont reader (Maybe t)

keyItemReadFunction :: forall cont reader. ContainerKey cont -> ReadFunctionF Maybe (KeyReader cont reader) reader
keyItemReadFunction key mr rt = Compose $ mr $ KeyReadItem key rt

knownKeyItemReadFunction :: forall cont reader. ContainerKey cont -> ReadFunction (KeyReader cont reader) reader
knownKeyItemReadFunction key mr rt = do
    mt <- getCompose $ keyItemReadFunction key mr rt
    case mt of
        Just t -> return t
        Nothing -> error $ "missing item in list"

instance (KeyContainer cont, SubjectReader reader, ReaderSubject reader ~ Element cont) =>
         SubjectReader (KeyReader cont reader) where
    type ReaderSubject (KeyReader cont reader) = cont
    subjectToRead cont KeyReadKeys = MkFiniteSet $ keys cont
    subjectToRead cont (KeyReadItem key reader) = fmap (\e -> subjectToRead e reader) $ lookupElement key cont

instance (KeyContainer cont, FullSubjectReader reader, ReaderSubject reader ~ Element cont) =>
         FullSubjectReader (KeyReader cont reader) where
    mutableReadToSubject mr = do
        MkFiniteSet allkeys <- mr KeyReadKeys
        list <- for allkeys $ \key -> mutableReadToSubject $ knownKeyItemReadFunction key mr
        return $ fromElementList list

data KeyEdit cont edit where
    KeyEditItem :: ContainerKey cont -> edit -> KeyEdit cont edit
    KeyDeleteItem :: ContainerKey cont -> KeyEdit cont edit
    KeyInsertReplaceItem :: Element cont -> KeyEdit cont edit
    KeyClear :: KeyEdit cont edit

class (SubjectReader reader, ReaderSubject reader ~ Element cont) =>
      HasKeyReader cont reader where
    readKey ::
           forall m. MonadIO m
        => MutableRead m reader
        -> m (ContainerKey cont)

instance ( EditSubject keyedit ~ key
         , EditSubject valedit ~ val
         , SubjectReader (EditReader keyedit)
         , FullSubjectReader (EditReader keyedit)
         , SubjectReader (EditReader valedit)
         ) =>
         HasKeyReader [(key, val)] (PairEditReader keyedit valedit) where
    readKey mr = mutableReadToSubject $ firstReadFunction mr

instance HasKeyReader (FiniteSet t) (WholeReader t) where
    readKey mr = mr ReadWhole

instance Floating (KeyEdit cont edit) (KeyEdit cont edit)

replace :: Eq a => a -> a -> FiniteSet a -> FiniteSet a
replace _ _ (MkFiniteSet []) = mempty
replace old new (MkFiniteSet (a:aa))
    | old == a = MkFiniteSet $ new : aa
replace old new (MkFiniteSet (a:aa)) = MkFiniteSet $ a : (unFiniteSet $ replace old new $ MkFiniteSet aa)

instance (KeyContainer cont, FullSubjectReader (EditReader edit), Edit edit, HasKeyReader cont (EditReader edit)) =>
         Edit (KeyEdit cont edit) where
    type EditReader (KeyEdit cont edit) = KeyReader cont (EditReader edit)
    applyEdit (KeyEditItem oldkey edit) mr kreader@(KeyReadItem key rt) = do
        mnewkey <- getCompose $ readKey @cont $ applyEdit edit $ keyItemReadFunction oldkey mr -- the edit may change the element's key
        case mnewkey of
            Just newkey
                | key == newkey -> getCompose $ (applyEdit edit $ keyItemReadFunction key mr) rt
            _ ->
                if key == oldkey
                    then return Nothing
                    else mr kreader
    applyEdit (KeyEditItem oldkey edit) mr KeyReadKeys = do
        oldkeys <- mr KeyReadKeys
        mnewkey <-
            getCompose $ readKey @cont $ applyEdit edit $ keyItemReadFunction oldkey mr -- the edit may change the element's key
        return $
            case mnewkey of
                Just newkey -> replace oldkey newkey oldkeys
                _ -> oldkeys
    applyEdit (KeyDeleteItem key) mr KeyReadKeys = do
        allkeys <- mr KeyReadKeys
        return $ deleteSet key allkeys
    applyEdit (KeyDeleteItem key') _mr (KeyReadItem key _reader)
        | key' == key = return Nothing
    applyEdit (KeyDeleteItem _) mr (KeyReadItem key reader) = mr $ KeyReadItem key reader
    applyEdit (KeyInsertReplaceItem item) mr KeyReadKeys = do
        allkeys <- mr KeyReadKeys
        let newkey = elementKey (Proxy :: Proxy cont) item
        if elem newkey allkeys
            then return allkeys
            else return $ insertSet newkey allkeys
    applyEdit (KeyInsertReplaceItem item) _mr (KeyReadItem key reader)
        | elementKey (Proxy @cont) item == key = return $ Just $ subjectToRead item reader
    applyEdit (KeyInsertReplaceItem _) mr (KeyReadItem key reader) = mr $ KeyReadItem key reader
    applyEdit KeyClear _mr reader = mSubjectToMutableRead (return mempty) reader

instance ( KeyContainer cont
         , FullSubjectReader (EditReader edit)
         , InvertibleEdit edit
         , HasKeyReader cont (EditReader edit)
         ) =>
         InvertibleEdit (KeyEdit cont edit) where
    invertEdit (KeyEditItem key edit) mr = do
        minvedits <- getCompose $ invertEdit edit $ keyItemReadFunction key mr
        case minvedits of
            Just invedits -> return $ fmap (KeyEditItem key) invedits
            Nothing -> return []
    invertEdit (KeyInsertReplaceItem item) mr = do
        let newkey = elementKey (Proxy :: Proxy cont) item
        molditem <- getCompose $ mutableReadToSubject $ keyItemReadFunction newkey mr
        case molditem of
            Just olditem -> return [KeyInsertReplaceItem olditem]
            Nothing -> return [KeyDeleteItem newkey]
    invertEdit (KeyDeleteItem key) mr = do
        ma <- getCompose $ mutableReadToSubject $ keyItemReadFunction key mr
        case ma of
            Just a -> return [KeyInsertReplaceItem a]
            Nothing -> return []
    invertEdit KeyClear mr = getReplaceEdits mr

instance (KeyContainer cont, FullSubjectReader (EditReader edit), Edit edit, HasKeyReader cont (EditReader edit)) =>
         FullEdit (KeyEdit cont edit) where
    replaceEdit mr write = do
        write KeyClear
        allkeys <- mr KeyReadKeys
        for_ allkeys $ \key -> do
            item <- mutableReadToSubject $ knownKeyItemReadFunction key mr
            write $ KeyInsertReplaceItem item

getKeyElementEditLens ::
       forall cont edit. (KeyContainer cont, HasKeyReader cont (EditReader edit), Edit edit)
    => ContainerKey cont
    -> IO (EditLens (KeyEdit cont edit) (MaybeEdit edit))
getKeyElementEditLens initial =
    newMVar initial >>= \var -> let
        unlift :: Unlift (StateT (ContainerKey cont))
        unlift = mvarUnlift var
        efGet ::
               ReadFunctionT (StateT (ContainerKey cont)) (KeyReader cont (EditReader edit)) (OneReader Maybe (EditReader edit))
        efGet mr ReadHasOne = do
            kk <- lift $ mr KeyReadKeys
            key <- get
            return $
                if elem key kk
                    then Just ()
                    else Nothing
        efGet mr (ReadOne rt) = do
            key <- get
            lift $ mr $ KeyReadItem key rt
        efUpdate ::
               forall m. MonadIO m
            => KeyEdit cont edit
            -> MutableRead m (EditReader (KeyEdit cont edit))
            -> StateT (ContainerKey cont) m [MaybeEdit edit]
        efUpdate KeyClear _ = return [SumEditLeft (MkWholeEdit Nothing)]
        efUpdate (KeyDeleteItem k) _ = do
            key <- get
            return $
                if k == key
                    then [SumEditLeft (MkWholeEdit Nothing)]
                    else []
        efUpdate (KeyEditItem k edit) mr = do
            oldkey <- get
            if k == oldkey
                then do
                    mnewkey <- lift $ getCompose $ readKey @cont $ applyEdit edit $ keyItemReadFunction oldkey mr
                    case mnewkey of
                        Just newkey -> do
                            put newkey
                            return [SumEditRight (MkOneEdit edit)]
                        Nothing -> return []
                else return []
        efUpdate (KeyInsertReplaceItem item) _ = do
            key <- get
            return $
                if elementKey (Proxy :: Proxy cont) item == key
                    then [SumEditLeft (MkWholeEdit (Just item))]
                    else []
        elFunction :: AnEditFunction (StateT (ContainerKey cont)) (KeyEdit cont edit) (MaybeEdit edit)
        elFunction = MkAnEditFunction {..}
        elPutEdit ::
               forall m. MonadIO m
            => MaybeEdit edit
            -> MutableRead m (EditReader (KeyEdit cont edit))
            -> StateT (ContainerKey cont) m (Maybe [KeyEdit cont edit])
        elPutEdit (SumEditLeft (MkWholeEdit (Just subj))) _ = return $ Just [KeyInsertReplaceItem subj]
        elPutEdit (SumEditLeft (MkWholeEdit Nothing)) _ = do
            key <- get
            return $ Just [KeyDeleteItem key]
        elPutEdit (SumEditRight (MkOneEdit edit)) mr = do
            oldkey <- get
            mnewkey <- lift $ getCompose $ readKey @cont $ applyEdit edit $ keyItemReadFunction oldkey mr
            case mnewkey of
                Just newkey -> do
                    put newkey
                    return $ Just [KeyEditItem oldkey edit]
                Nothing -> return $ Just []
        in return $ MkCloseUnlift unlift MkAnEditLens {..}

getKeyValueEditLens ::
       forall cont keyedit valueedit.
       ( KeyContainer cont
       , HasKeyReader cont (PairEditReader keyedit valueedit)
       , Edit keyedit
       , FullSubjectReader (EditReader keyedit)
       , FullEdit valueedit
       )
    => ContainerKey cont
    -> IO (EditLens (KeyEdit cont (PairEdit keyedit valueedit)) (MaybeEdit valueedit))
getKeyValueEditLens key = do
    lens <- getKeyElementEditLens key
    return $ (oneWholeLiftEditLens $ tupleEditLens EditSecond) <.> lens

liftKeyElementAnEditFunction ::
       forall t conta contb edita editb.
       ( MonadTransUnlift t
       , ContainerKey conta ~ ContainerKey contb
       , EditSubject edita ~ Element conta
       , EditSubject editb ~ Element contb
       , SubjectReader (EditReader edita)
       , FullSubjectReader (EditReader editb)
       )
    => AnEditFunction t edita editb
    -> AnEditFunction t (KeyEdit conta edita) (KeyEdit contb editb)
liftKeyElementAnEditFunction (MkAnEditFunction g u) = let
    efGet :: ReadFunctionT t (KeyReader conta (EditReader edita)) (KeyReader contb (EditReader editb))
    efGet mr KeyReadKeys = lift $ mr KeyReadKeys
    efGet mr (KeyReadItem key rt) = transComposeOne $ g (keyItemReadFunction key mr) rt
    efUpdate ::
           forall m. MonadIO m
        => KeyEdit conta edita
        -> MutableRead m (EditReader (KeyEdit conta edita))
        -> t m [KeyEdit contb editb]
    efUpdate KeyClear _ = lift $ return [KeyClear]
    efUpdate (KeyInsertReplaceItem itema) _ =
        withTransConstraintTM @MonadIO $ do
            itemb <- mutableReadToSubject $ g $ subjectToMutableRead itema
            return [KeyInsertReplaceItem itemb]
    efUpdate (KeyDeleteItem key) _ = lift $ return [KeyDeleteItem key]
    efUpdate (KeyEditItem key ea) mr =
        withTransConstraintTM @MonadIO $ do
            mresult <- transComposeOne $ u ea (keyItemReadFunction @conta key mr)
            case mresult of
                Just ebs -> return $ fmap (KeyEditItem key) ebs
                Nothing -> return []
    in MkAnEditFunction {..}

liftKeyElementEditFunction ::
       forall conta contb edita editb.
       ( ContainerKey conta ~ ContainerKey contb
       , EditSubject edita ~ Element conta
       , EditSubject editb ~ Element contb
       , SubjectReader (EditReader edita)
       , FullSubjectReader (EditReader editb)
       )
    => EditFunction edita editb
    -> EditFunction (KeyEdit conta edita) (KeyEdit contb editb)
liftKeyElementEditFunction (MkCloseUnlift unlift ef) = MkCloseUnlift unlift $ liftKeyElementAnEditFunction ef

liftKeyElementEditLens ::
       forall conta contb edita editb.
       ( ContainerKey conta ~ ContainerKey contb
       , EditSubject edita ~ Element conta
       , EditSubject editb ~ Element contb
       , SubjectReader (EditReader edita)
       , FullSubjectReader (EditReader editb)
       )
    => (forall m. MonadIO m =>
                      EditSubject editb -> m (Maybe (EditSubject edita)))
    -> EditLens edita editb
    -> EditLens (KeyEdit conta edita) (KeyEdit contb editb)
liftKeyElementEditLens bma (MkCloseUnlift (unlift :: Unlift t) (MkAnEditLens ef pe)) = let
    elFunction = liftKeyElementAnEditFunction ef
    elPutEdit ::
           forall m. MonadIO m
        => KeyEdit contb editb
        -> MutableRead m (EditReader (KeyEdit conta edita))
        -> t m (Maybe [KeyEdit conta edita])
    elPutEdit KeyClear _ = withTransConstraintTM @MonadIO $ return $ Just [KeyClear]
    elPutEdit (KeyInsertReplaceItem itemb) _ =
        withTransConstraintTM @MonadIO $ do
            fitema <- bma itemb
            return $ fmap (\itema -> [KeyInsertReplaceItem itema]) fitema
    elPutEdit (KeyDeleteItem key) _ = withTransConstraintTM @MonadIO $ return $ Just [KeyDeleteItem key]
    elPutEdit (KeyEditItem key eb) mr =
        withTransConstraintTM @MonadIO $ do
            mfresult <- transComposeOne $ pe eb (keyItemReadFunction @conta key mr)
            case mfresult of
                Just fsea -> return $ fmap (fmap $ KeyEditItem key) fsea
                Nothing -> return $ Just []
    in MkCloseUnlift unlift $ MkAnEditLens {..}

{-
contextKeyEditLens ::
       forall editx cont edit.
       PureEditLens (ContextEdit editx (KeyEdit cont edit)) (KeyEdit cont (ContextEdit editx edit))
contextKeyEditLens = let
    editAccess :: IOStateAccess ()
    editAccess = unitStateAccess
    editGet ::
           ()
        -> KeyReader cont (ContextEditReader editx edit) t
        -> Readable (ContextEditReader editx (KeyEdit cont edit)) t
    editGet () KeyReadKeys = readable $ MkTupleEditReader EditContent KeyReadKeys
    editGet () (KeyReadItem _ (MkTupleEditReader EditContext reader)) =
        fmap Just $ readable $ MkTupleEditReader EditContext reader
    editGet () (KeyReadItem key (MkTupleEditReader EditContent reader)) =
        readable $ MkTupleEditReader EditContent $ KeyReadItem key reader
    editUpdate ::
           ContextEdit editx (KeyEdit cont edit)
        -> ()
        -> Readable (ContextEditReader editx (KeyEdit cont edit)) ((), [KeyEdit cont (ContextEdit editx edit)])
    editUpdate (MkTupleEdit EditContext edit) () = do
        MkFiniteSet kk <- readable $ MkTupleEditReader EditContent KeyReadKeys
        return $ pure $ fmap (\key -> KeyEditItem key $ MkTupleEdit EditContext edit) kk
    editUpdate (MkTupleEdit EditContent (KeyEditItem key edit)) () =
        return $ pure [KeyEditItem key (MkTupleEdit EditContent edit)]
    editUpdate (MkTupleEdit EditContent (KeyDeleteItem key)) () = return $ pure [KeyDeleteItem key]
    editUpdate (MkTupleEdit EditContent (KeyInsertReplaceItem el)) () = return $ pure [KeyInsertReplaceItem el]
    editUpdate (MkTupleEdit EditContent KeyClear) () = return $ pure [KeyClear]
    editLensFunction = MkEditFunction {..}
    editLensPutEdit ::
           ()
        -> KeyEdit cont (ContextEdit editx edit)
        -> Readable (ContextEditReader editx (KeyEdit cont edit)) (Maybe ((), [ContextEdit editx (KeyEdit cont edit)]))
    editLensPutEdit () (KeyEditItem _ (MkTupleEdit EditContext edit)) =
        return $ pure $ pure [MkTupleEdit EditContext edit]
    editLensPutEdit () (KeyEditItem key (MkTupleEdit EditContent edit)) =
        return $ pure $ pure [MkTupleEdit EditContent $ KeyEditItem key edit]
    editLensPutEdit () (KeyDeleteItem key) = return $ pure $ pure [MkTupleEdit EditContent $ KeyDeleteItem key]
    editLensPutEdit () (KeyInsertReplaceItem el) =
        return $ pure $ pure [MkTupleEdit EditContent $ KeyInsertReplaceItem el]
    editLensPutEdit () KeyClear = return $ pure $ pure [MkTupleEdit EditContent KeyClear]
    in MkEditLens {..}

contextKeyGeneralLens :: EditLens (ContextEdit editx (KeyEdit cont edit)) (KeyEdit cont (ContextEdit editx edit))
contextKeyGeneralLens = MkCloseState contextKeyEditLens
-}
findBy :: (a -> a -> Ordering) -> [a] -> a -> (Int, Bool)
findBy cmp items x = let
    ords = fmap (cmp x) items
    in (length $ filter ((==) GT) ords, any ((==) EQ) ords)

orderedKeyList ::
       forall cont seq edit. (Index seq ~ Int, Element cont ~ EditSubject edit, KeyContainer cont, FullEdit edit)
    => (EditSubject edit -> EditSubject edit -> Ordering)
    -> EditFunction (KeyEdit cont edit) (ListEdit seq edit)
orderedKeyList cmp = let
    getUnsortedPairs ::
           forall m. MonadIO m
        => MutableRead m (KeyReader cont (EditReader edit))
        -> m [(ContainerKey cont, EditSubject edit)]
    getUnsortedPairs mr = do
        keyset <- mr KeyReadKeys
        for (toList keyset) $ \key -> do
            t <- mutableReadToSubject $ knownKeyItemReadFunction key mr
            return (key, t)
    getSortedKeys ::
           forall m. MonadIO m
        => MutableRead m (KeyReader cont (EditReader edit))
        -> m [ContainerKey cont]
    getSortedKeys mr = do
        pairs <- getUnsortedPairs mr
        return $ fmap fst $ sortBy (\p q -> cmp (snd p) (snd q)) pairs
    findKey ::
           forall m. MonadIO m
        => ContainerKey cont
        -> MutableRead m (KeyReader cont (EditReader edit))
        -> m (SequencePoint seq, Bool)
    findKey key mr = do
        pairs <- getUnsortedPairs mr
        skey <- mutableReadToSubject $ knownKeyItemReadFunction key mr
        return $ (\(i, found) -> (MkSequencePoint i, found)) $ findBy cmp (fmap snd pairs) skey
    efGet :: ReadFunctionT IdentityT (EditReader (KeyEdit cont edit)) (EditReader (ListEdit seq edit))
    efGet mr ListReadLength = do
        keyset <- lift $ mr KeyReadKeys
        return $ MkSequencePoint $ length keyset
    efGet mr (ListReadItem (MkSequencePoint i) reader) = do
        keylist <- lift $ getSortedKeys mr
        case index keylist i of
            Just key -> lift $ mr $ KeyReadItem key reader
            Nothing -> return Nothing
    efUpdate ::
           forall m. MonadIO m
        => KeyEdit cont edit
        -> MutableRead m (EditReader (KeyEdit cont edit))
        -> IdentityT m [ListEdit seq edit]
    efUpdate (KeyEditItem key edit) mr = do
        (i, found) <- lift $ findKey key mr
        return $
            if found
                then [ListEditItem i edit]
                else []
    efUpdate (KeyDeleteItem key) mr = do
        (i, found) <- lift $ findKey key mr
        return $
            if found
                then [ListDeleteItem i]
                else []
    efUpdate (KeyInsertReplaceItem item) mr = do
        (i, found) <- lift $ findKey (elementKey (Proxy :: Proxy cont) item) mr
        if found
            then return [ListInsertItem i item]
            else do
                edits <- getReplaceEditsFromSubject item
                return $ fmap (ListEditItem i) edits
    efUpdate KeyClear _ = return [ListClear]
    in MkCloseUnlift identityUnlift MkAnEditFunction {..}
