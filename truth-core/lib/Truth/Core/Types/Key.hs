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

instance (Show (ContainerKey cont), AllWitnessConstraint Show reader) => Show (KeyReader cont reader t) where
    show KeyReadKeys = "keys"
    show (KeyReadItem key rt) = "item " ++ show key ++ " " ++ showAllWitness rt

instance (Show (ContainerKey cont), AllWitnessConstraint Show reader) =>
             AllWitnessConstraint Show (KeyReader cont reader) where
    allWitnessConstraint = Dict

instance (Show (ContainerKey cont), WitnessConstraint Show reader) => WitnessConstraint Show (KeyReader cont reader) where
    witnessConstraint KeyReadKeys = Dict
    witnessConstraint (KeyReadItem _ rt) =
        case witnessConstraint @_ @Show rt of
            Dict -> Dict

keyItemReadFunction :: forall cont reader. ContainerKey cont -> ReadFunctionF Maybe (KeyReader cont reader) reader
keyItemReadFunction key mr rt = MkComposeM $ mr $ KeyReadItem key rt

knownKeyItemReadFunction :: forall cont reader. ContainerKey cont -> ReadFunction (KeyReader cont reader) reader
knownKeyItemReadFunction key mr rt = do
    mt <- getComposeM $ keyItemReadFunction key mr rt
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

instance (Show (ContainerKey cont), Show edit, Show (Element cont)) => Show (KeyEdit cont edit) where
    show (KeyEditItem key edit) = "edit " ++ show key ++ " " ++ show edit
    show (KeyDeleteItem key) = "delete " ++ show key
    show (KeyInsertReplaceItem element) = "insert " ++ show element
    show KeyClear = "clear"

class (SubjectReader reader, ReaderSubject reader ~ Element cont) => HasKeyReader cont reader where
    readKey ::
           forall m. MonadIO m
        => MutableRead m reader
        -> m (ContainerKey cont)

instance ( EditSubject keyedit ~ key
         , EditSubject valedit ~ val
         , SubjectReader (EditReader keyedit)
         , FullSubjectReader (EditReader keyedit)
         , SubjectReader (EditReader valedit)
         ) => HasKeyReader [(key, val)] (PairEditReader keyedit valedit) where
    readKey mr = mutableReadToSubject $ firstReadFunction mr

instance HasKeyReader (FiniteSet t) (WholeReader t) where
    readKey mr = mr ReadWhole

instance Floating (KeyEdit cont edit) (KeyEdit cont edit)

replace :: Eq a => a -> a -> FiniteSet a -> FiniteSet a
replace _ _ (MkFiniteSet []) = mempty
replace old new (MkFiniteSet (a:aa))
    | old == a = MkFiniteSet $ new : aa
replace old new (MkFiniteSet (a:aa)) = MkFiniteSet $ a : (unFiniteSet $ replace old new $ MkFiniteSet aa)

type instance EditReader (KeyEdit cont edit) =
     KeyReader cont (EditReader edit)

instance ( KeyContainer cont
         , FullSubjectReader (EditReader edit)
         , ApplicableEdit edit
         , HasKeyReader cont (EditReader edit)
         ) => ApplicableEdit (KeyEdit cont edit) where
    applyEdit (KeyEditItem oldkey edit) mr kreader@(KeyReadItem key rt) = do
        mnewkey <- getComposeM $ readKey @cont $ applyEdit edit $ keyItemReadFunction oldkey mr -- the edit may change the element's key
        case mnewkey of
            Just newkey
                | key == newkey -> getComposeM $ (applyEdit edit $ keyItemReadFunction key mr) rt
            _ ->
                if key == oldkey
                    then return Nothing
                    else mr kreader
    applyEdit (KeyEditItem oldkey edit) mr KeyReadKeys = do
        oldkeys <- mr KeyReadKeys
        mnewkey <-
            getComposeM $ readKey @cont $ applyEdit edit $ keyItemReadFunction oldkey mr -- the edit may change the element's key
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
        let newkey = elementKey @cont item
        if elem newkey allkeys
            then return allkeys
            else return $ insertSet newkey allkeys
    applyEdit (KeyInsertReplaceItem item) _mr (KeyReadItem key reader)
        | elementKey @cont item == key = return $ Just $ subjectToRead item reader
    applyEdit (KeyInsertReplaceItem _) mr (KeyReadItem key reader) = mr $ KeyReadItem key reader
    applyEdit KeyClear _mr reader = mSubjectToMutableRead (return mempty) reader

instance ( KeyContainer cont
         , FullSubjectReader (EditReader edit)
         , ApplicableEdit edit
         , SubjectMapEdit edit
         , InvertibleEdit edit
         , HasKeyReader cont (EditReader edit)
         ) => InvertibleEdit (KeyEdit cont edit) where
    invertEdit (KeyEditItem key edit) mr = do
        minvedits <- getComposeM $ invertEdit edit $ keyItemReadFunction key mr
        case minvedits of
            Just invedits -> return $ fmap (KeyEditItem key) invedits
            Nothing -> return []
    invertEdit (KeyInsertReplaceItem item) mr = do
        let newkey = elementKey @cont item
        molditem <- getComposeM $ mutableReadToSubject $ keyItemReadFunction newkey mr
        case molditem of
            Just olditem -> return [KeyInsertReplaceItem olditem]
            Nothing -> return [KeyDeleteItem newkey]
    invertEdit (KeyDeleteItem key) mr = do
        ma <- getComposeM $ mutableReadToSubject $ keyItemReadFunction key mr
        case ma of
            Just a -> return [KeyInsertReplaceItem a]
            Nothing -> return []
    invertEdit KeyClear mr = getReplaceEdits mr

instance (KeyContainer cont, EditSubject edit ~ Element cont, SubjectMapEdit edit) => SubjectMapEdit (KeyEdit cont edit) where
    mapSubjectEdits =
        mapEditToMapEdits $ \keyedit subj ->
            case keyedit of
                KeyEditItem key edit ->
                    case lookupElement key subj of
                        Nothing -> return subj
                        Just oldelem -> do
                            newelem <- mapSubjectEdits [edit] oldelem
                            return $ insertElement newelem subj
                KeyDeleteItem key -> return $ deleteElement key subj
                KeyInsertReplaceItem item -> return $ insertElement item subj
                KeyClear -> return mempty

instance ( KeyContainer cont
         , FullSubjectReader (EditReader edit)
         , ApplicableEdit edit
         , SubjectMapEdit edit
         , HasKeyReader cont (EditReader edit)
         ) => FullEdit (KeyEdit cont edit) where
    replaceEdit mr write = do
        write KeyClear
        allkeys <- mr KeyReadKeys
        for_ allkeys $ \key -> do
            item <- mutableReadToSubject $ knownKeyItemReadFunction key mr
            write $ KeyInsertReplaceItem item

unliftKeyElementEditLens ::
       forall cont edit.
       ( KeyContainer cont
       , HasKeyReader cont (EditReader edit)
       , ApplicableEdit edit
       , FullSubjectReader (EditReader edit)
       )
    => Unlift (StateT (ContainerKey cont))
    -> EditLens (KeyEdit cont edit) (MaybeEdit edit)
unliftKeyElementEditLens unlift = let
    ufGet ::
           ReadFunctionT (StateT (ContainerKey cont)) (KeyReader cont (EditReader edit)) (OneReader Maybe (EditReader edit))
    ufGet mr ReadHasOne = do
        kk <- lift $ mr KeyReadKeys
        key <- get
        return $
            if elem key kk
                then Just ()
                else Nothing
    ufGet mr (ReadOne rt) = do
        key <- get
        lift $ mr $ KeyReadItem key rt
    ufUpdate ::
           forall m. MonadIO m
        => KeyEdit cont edit
        -> MutableRead m (EditReader (KeyEdit cont edit))
        -> StateT (ContainerKey cont) m [MaybeEdit edit]
    ufUpdate KeyClear _ = return [SumEditLeft (MkWholeEdit Nothing)]
    ufUpdate (KeyDeleteItem k) _ = do
        key <- get
        return $
            if k == key
                then [SumEditLeft (MkWholeEdit Nothing)]
                else []
    ufUpdate (KeyEditItem k edit) mr = do
        oldkey <- get
        if k == oldkey
            then do
                mnewkey <- lift $ getComposeM $ readKey @cont $ applyEdit edit $ keyItemReadFunction oldkey mr
                case mnewkey of
                    Just newkey -> do
                        put newkey
                        return [SumEditRight (MkOneEdit edit)]
                    Nothing -> return []
            else return []
    ufUpdate (KeyInsertReplaceItem item) _ = do
        key <- get
        return $
            if elementKey @cont item == key
                then [SumEditLeft (MkWholeEdit (Just item))]
                else []
    elFunction :: AnUpdateFunction (StateT (ContainerKey cont)) (KeyEdit cont edit) (MaybeEdit edit)
    elFunction = MkAnUpdateFunction {..}
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
        mnewkey <- lift $ getComposeM $ readKey @cont $ applyEdit edit $ keyItemReadFunction oldkey mr
        case mnewkey of
            Just newkey -> do
                put newkey
                return $ Just [KeyEditItem oldkey edit]
            Nothing -> return $ Just []
    elPutEdits ::
           forall m. MonadIO m
        => [MaybeEdit edit]
        -> MutableRead m (EditReader (KeyEdit cont edit))
        -> StateT (ContainerKey cont) m (Maybe [KeyEdit cont edit])
    elPutEdits = elPutEditsFromPutEdit elPutEdit
    in MkCloseUnlift unlift MkAnEditLens {..}

getKeyElementEditLens ::
       forall cont edit.
       ( KeyContainer cont
       , HasKeyReader cont (EditReader edit)
       , ApplicableEdit edit
       , FullSubjectReader (EditReader edit)
       )
    => ContainerKey cont
    -> IO (EditLens (KeyEdit cont edit) (MaybeEdit edit))
getKeyElementEditLens initial = do
    var <- newMVar initial
    return $ unliftKeyElementEditLens $ mvarUnlift var

stableKeyElementEditLens ::
       forall cont edit.
       ( KeyContainer cont
       , HasKeyReader cont (EditReader edit)
       , ApplicableEdit edit
       , FullSubjectReader (EditReader edit)
       )
    => ContainerKey cont
    -> EditLens (KeyEdit cont edit) (MaybeEdit edit)
stableKeyElementEditLens key = unliftKeyElementEditLens $ stateDiscardingUnlift key

getKeyValueEditLens ::
       forall cont keyedit valueedit.
       ( KeyContainer cont
       , HasKeyReader cont (PairEditReader keyedit valueedit)
       , ApplicableEdit keyedit
       , FullSubjectReader (EditReader keyedit)
       , FullEdit valueedit
       )
    => ContainerKey cont
    -> IO (EditLens (KeyEdit cont (PairEdit keyedit valueedit)) (MaybeEdit valueedit))
getKeyValueEditLens key = do
    lens <- getKeyElementEditLens key
    return $ (oneWholeLiftEditLens $ tupleEditLens SelectSecond) . lens

liftKeyElementAnUpdateFunction ::
       forall t conta contb edita editb.
       ( MonadTransUnlift t
       , ContainerKey conta ~ ContainerKey contb
       , EditSubject edita ~ Element conta
       , EditSubject editb ~ Element contb
       , SubjectReader (EditReader edita)
       , FullSubjectReader (EditReader editb)
       )
    => AnUpdateFunction t edita editb
    -> AnUpdateFunction t (KeyEdit conta edita) (KeyEdit contb editb)
liftKeyElementAnUpdateFunction (MkAnUpdateFunction g u) = let
    ufGet :: ReadFunctionT t (KeyReader conta (EditReader edita)) (KeyReader contb (EditReader editb))
    ufGet mr KeyReadKeys = lift $ mr KeyReadKeys
    ufGet mr (KeyReadItem key rt) = transComposeOne $ g (keyItemReadFunction key mr) rt
    ufUpdate ::
           forall m. MonadIO m
        => KeyEdit conta edita
        -> MutableRead m (EditReader (KeyEdit conta edita))
        -> t m [KeyEdit contb editb]
    ufUpdate KeyClear _ = lift $ return [KeyClear]
    ufUpdate (KeyInsertReplaceItem itema) _ =
        withTransConstraintTM @MonadIO $ do
            itemb <- mutableReadToSubject $ g $ subjectToMutableRead itema
            return [KeyInsertReplaceItem itemb]
    ufUpdate (KeyDeleteItem key) _ = lift $ return [KeyDeleteItem key]
    ufUpdate (KeyEditItem key ea) mr =
        withTransConstraintTM @MonadIO $ do
            mresult <- transComposeOne $ u ea (keyItemReadFunction @conta key mr)
            case mresult of
                Just ebs -> return $ fmap (KeyEditItem key) ebs
                Nothing -> return []
    in MkAnUpdateFunction {..}

liftKeyElementUpdateFunction ::
       forall conta contb edita editb.
       ( ContainerKey conta ~ ContainerKey contb
       , EditSubject edita ~ Element conta
       , EditSubject editb ~ Element contb
       , SubjectReader (EditReader edita)
       , FullSubjectReader (EditReader editb)
       )
    => UpdateFunction edita editb
    -> UpdateFunction (KeyEdit conta edita) (KeyEdit contb editb)
liftKeyElementUpdateFunction (MkCloseUnlift unlift ef) = MkCloseUnlift unlift $ liftKeyElementAnUpdateFunction ef

liftKeyElementEditLens ::
       forall conta contb edita editb.
       ( ContainerKey conta ~ ContainerKey contb
       , KeyContainer conta
       , HasKeyReader conta (EditReader edita)
       , EditSubject edita ~ Element conta
       , EditSubject editb ~ Element contb
       , ApplicableEdit edita
       , FullSubjectReader (EditReader edita)
       , FullSubjectReader (EditReader editb)
       )
    => (forall m. MonadIO m => EditSubject editb -> m (Maybe (EditSubject edita)))
    -> EditLens edita editb
    -> EditLens (KeyEdit conta edita) (KeyEdit contb editb)
liftKeyElementEditLens bma (MkCloseUnlift (unlift :: Unlift t) (MkAnEditLens ef pe)) = let
    elFunction = liftKeyElementAnUpdateFunction ef
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
            mfresult <- transComposeOne $ pe [eb] (keyItemReadFunction @conta key mr)
            case mfresult of
                Just fsea -> return $ fmap (fmap $ KeyEditItem key) fsea
                Nothing -> return $ Just []
    elPutEdits ::
           forall m. MonadIO m
        => [KeyEdit contb editb]
        -> MutableRead m (EditReader (KeyEdit conta edita))
        -> t m (Maybe [KeyEdit conta edita])
    elPutEdits = elPutEditsFromPutEdit elPutEdit
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
    editGet () KeyReadKeys = readable $ MkTupleEditReader SelectContent KeyReadKeys
    editGet () (KeyReadItem _ (MkTupleEditReader SelectContext reader)) =
        fmap Just $ readable $ MkTupleEditReader SelectContext reader
    editGet () (KeyReadItem key (MkTupleEditReader SelectContent reader)) =
        readable $ MkTupleEditReader SelectContent $ KeyReadItem key reader
    editUpdate ::
           ContextEdit editx (KeyEdit cont edit)
        -> ()
        -> Readable (ContextEditReader editx (KeyEdit cont edit)) ((), [KeyEdit cont (ContextEdit editx edit)])
    editUpdate (MkTupleEdit SelectContext edit) () = do
        MkFiniteSet kk <- readable $ MkTupleEditReader SelectContent KeyReadKeys
        return $ pure $ fmap (\key -> KeyEditItem key $ MkTupleEdit SelectContext edit) kk
    editUpdate (MkTupleEdit SelectContent (KeyEditItem key edit)) () =
        return $ pure [KeyEditItem key (MkTupleEdit SelectContent edit)]
    editUpdate (MkTupleEdit SelectContent (KeyDeleteItem key)) () = return $ pure [KeyDeleteItem key]
    editUpdate (MkTupleEdit SelectContent (KeyInsertReplaceItem el)) () = return $ pure [KeyInsertReplaceItem el]
    editUpdate (MkTupleEdit SelectContent KeyClear) () = return $ pure [KeyClear]
    editLensFunction = MkUpdateFunction {..}
    editLensPutEdit ::
           ()
        -> KeyEdit cont (ContextEdit editx edit)
        -> Readable (ContextEditReader editx (KeyEdit cont edit)) (Maybe ((), [ContextEdit editx (KeyEdit cont edit)]))
    editLensPutEdit () (KeyEditItem _ (MkTupleEdit SelectContext edit)) =
        return $ pure $ pure [MkTupleEdit SelectContext edit]
    editLensPutEdit () (KeyEditItem key (MkTupleEdit SelectContent edit)) =
        return $ pure $ pure [MkTupleEdit SelectContent $ KeyEditItem key edit]
    editLensPutEdit () (KeyDeleteItem key) = return $ pure $ pure [MkTupleEdit SelectContent $ KeyDeleteItem key]
    editLensPutEdit () (KeyInsertReplaceItem el) =
        return $ pure $ pure [MkTupleEdit SelectContent $ KeyInsertReplaceItem el]
    editLensPutEdit () KeyClear = return $ pure $ pure [MkTupleEdit SelectContent KeyClear]
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
    -> UpdateFunction (KeyEdit cont edit) (ListEdit seq edit)
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
    ufGet :: ReadFunctionT IdentityT (EditReader (KeyEdit cont edit)) (EditReader (ListEdit seq edit))
    ufGet mr ListReadLength = do
        keyset <- lift $ mr KeyReadKeys
        return $ MkSequencePoint $ length keyset
    ufGet mr (ListReadItem (MkSequencePoint i) reader) = do
        keylist <- lift $ getSortedKeys mr
        case index keylist i of
            Just key -> lift $ mr $ KeyReadItem key reader
            Nothing -> return Nothing
    ufUpdate ::
           forall m. MonadIO m
        => KeyEdit cont edit
        -> MutableRead m (EditReader (KeyEdit cont edit))
        -> IdentityT m [ListEdit seq edit]
    ufUpdate (KeyEditItem key edit) mr = do
        (i, found) <- lift $ findKey key mr
        return $
            if found
                then [ListEditItem i edit]
                else []
    ufUpdate (KeyDeleteItem key) mr = do
        (i, found) <- lift $ findKey key mr
        return $
            if found
                then [ListDeleteItem i]
                else []
    ufUpdate (KeyInsertReplaceItem item) mr = do
        (i, found) <- lift $ findKey (elementKey @cont item) mr
        if found
            then return [ListInsertItem i item]
            else do
                edits <- getReplaceEditsFromSubject item
                return $ fmap (ListEditItem i) edits
    ufUpdate KeyClear _ = return [ListClear]
    in MkCloseUnlift identityUnlift MkAnUpdateFunction {..}
