module Truth.Core.Types.Key where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Sequence
import Truth.Core.Types.List
import Truth.Core.Types.OneEdit
import Truth.Core.Types.OneReader
import Truth.Core.Types.OneWhole
import Truth.Core.Types.Pair
import Truth.Core.Types.Sum
import Truth.Core.Types.Whole

--- reader
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

class (SubjectReader reader, ReaderSubject reader ~ Element cont) => HasKeyReader cont reader where
    readKey ::
           forall m. MonadIO m
        => MutableRead m reader
        -> m (ContainerKey cont)

instance HasKeyReader (FiniteSet t) (WholeReader t) where
    readKey mr = mr ReadWhole

instance ( UpdateSubject keyupdate ~ key
         , UpdateSubject valupdate ~ val
         , SubjectReader (UpdateReader keyupdate)
         , FullSubjectReader (UpdateReader keyupdate)
         , SubjectReader (UpdateReader valupdate)
         ) => HasKeyReader [(key, val)] (PairUpdateReader keyupdate valupdate) where
    readKey mr = mutableReadToSubject $ firstReadFunction mr

--- edit
data KeyEdit cont edit where
    KeyEditItem :: ContainerKey cont -> edit -> KeyEdit cont edit
    KeyEditDelete :: ContainerKey cont -> KeyEdit cont edit
    KeyEditInsertReplace :: Element cont -> KeyEdit cont edit
    KeyEditClear :: KeyEdit cont edit

instance (Show (ContainerKey cont), Show edit, Show (Element cont)) => Show (KeyEdit cont edit) where
    show (KeyEditItem key edit) = "edit " ++ show key ++ " " ++ show edit
    show (KeyEditDelete key) = "delete " ++ show key
    show (KeyEditInsertReplace element) = "insert " ++ show element
    show KeyEditClear = "clear"

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
    applyEdit (KeyEditDelete key) mr KeyReadKeys = do
        allkeys <- mr KeyReadKeys
        return $ deleteSet key allkeys
    applyEdit (KeyEditDelete key') _mr (KeyReadItem key _reader)
        | key' == key = return Nothing
    applyEdit (KeyEditDelete _) mr (KeyReadItem key reader) = mr $ KeyReadItem key reader
    applyEdit (KeyEditInsertReplace item) mr KeyReadKeys = do
        allkeys <- mr KeyReadKeys
        let newkey = elementKey @cont item
        if elem newkey allkeys
            then return allkeys
            else return $ insertSet newkey allkeys
    applyEdit (KeyEditInsertReplace item) _mr (KeyReadItem key reader)
        | elementKey @cont item == key = return $ Just $ subjectToRead item reader
    applyEdit (KeyEditInsertReplace _) mr (KeyReadItem key reader) = mr $ KeyReadItem key reader
    applyEdit KeyEditClear _mr reader = mSubjectToMutableRead (return mempty) reader

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
    invertEdit (KeyEditInsertReplace item) mr = do
        let newkey = elementKey @cont item
        molditem <- getComposeM $ mutableReadToSubject $ keyItemReadFunction newkey mr
        case molditem of
            Just olditem -> return [KeyEditInsertReplace olditem]
            Nothing -> return [KeyEditDelete newkey]
    invertEdit (KeyEditDelete key) mr = do
        ma <- getComposeM $ mutableReadToSubject $ keyItemReadFunction key mr
        case ma of
            Just a -> return [KeyEditInsertReplace a]
            Nothing -> return []
    invertEdit KeyEditClear mr = getReplaceEdits mr

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
                KeyEditDelete key -> return $ deleteElement key subj
                KeyEditInsertReplace item -> return $ insertElement item subj
                KeyEditClear -> return mempty

instance ( KeyContainer cont
         , FullSubjectReader (EditReader edit)
         , ApplicableEdit edit
         , SubjectMapEdit edit
         , HasKeyReader cont (EditReader edit)
         ) => FullEdit (KeyEdit cont edit) where
    replaceEdit mr write = do
        write KeyEditClear
        allkeys <- mr KeyReadKeys
        for_ allkeys $ \key -> do
            item <- mutableReadToSubject $ knownKeyItemReadFunction key mr
            write $ KeyEditInsertReplace item

--- update
data KeyUpdate cont update where
    KeyUpdateItem :: ContainerKey cont -> update -> KeyUpdate cont update
    KeyUpdateDelete :: ContainerKey cont -> KeyUpdate cont update
    KeyUpdateInsertReplace :: Element cont -> KeyUpdate cont update
    KeyUpdateClear :: KeyUpdate cont update

type instance UpdateEdit (KeyUpdate cont update) =
     KeyEdit cont (UpdateEdit update)

instance IsUpdate update => IsUpdate (KeyUpdate cont update) where
    editUpdate (KeyEditItem key edit) = KeyUpdateItem key $ editUpdate edit
    editUpdate (KeyEditDelete key) = KeyUpdateDelete key
    editUpdate (KeyEditInsertReplace a) = KeyUpdateInsertReplace a
    editUpdate KeyEditClear = KeyUpdateClear

instance IsEditUpdate update => IsEditUpdate (KeyUpdate cont update) where
    updateEdit (KeyUpdateItem key update) = KeyEditItem key $ updateEdit update
    updateEdit (KeyUpdateDelete key) = KeyEditDelete key
    updateEdit (KeyUpdateInsertReplace a) = KeyEditInsertReplace a
    updateEdit KeyUpdateClear = KeyEditClear

type NewKeyGetter cont update
     = forall m.
           MonadIO m =>
                   update -> ContainerKey cont -> MutableRead m (KeyReader cont (UpdateReader update)) -> m (Maybe (ContainerKey cont))

updateKey ::
       forall cont update.
       (IsEditUpdate update, ApplicableEdit (UpdateEdit update), HasKeyReader cont (UpdateReader update))
    => NewKeyGetter cont update
updateKey update oldkey mr = getComposeM $ readKey @cont $ applyEdit (updateEdit update) $ keyItemReadFunction oldkey mr

useOldKey :: NewKeyGetter cont update
useOldKey _ oldkey _ = return $ Just oldkey

unliftKeyElementEditLens ::
       forall cont update.
       ( KeyContainer cont
       , HasKeyReader cont (UpdateReader update)
       , ApplicableEdit (UpdateEdit update)
       , FullSubjectReader (UpdateReader update)
       )
    => NewKeyGetter cont update
    -> ContainerKey cont
    -> StateEditLens (KeyUpdate cont update) (MaybeUpdate update)
unliftKeyElementEditLens newKeyGetter initKey = let
    sInit ::
           forall m. MonadIO m
        => MutableRead m (KeyReader cont (UpdateReader update))
        -> m (ContainerKey cont)
    sInit _ = return initKey
    sGet ::
           ReadFunctionT (StateT (ContainerKey cont)) (KeyReader cont (UpdateReader update)) (OneReader Maybe (UpdateReader update))
    sGet mr ReadHasOne = do
        kk <- lift $ mr KeyReadKeys
        key <- get
        return $
            if elem key kk
                then Just ()
                else Nothing
    sGet mr (ReadOne rt) = do
        key <- get
        lift $ mr $ KeyReadItem key rt
    sUpdate ::
           forall m. MonadIO m
        => KeyUpdate cont update
        -> MutableRead m (KeyReader cont (UpdateReader update))
        -> StateT (ContainerKey cont) m [MaybeUpdate update]
    sUpdate KeyUpdateClear _ = return [SumUpdateLeft (MkWholeReaderUpdate Nothing)]
    sUpdate (KeyUpdateDelete k) _ = do
        key <- get
        return $
            if k == key
                then [SumUpdateLeft (MkWholeReaderUpdate Nothing)]
                else []
    sUpdate (KeyUpdateItem k update) mr = do
        oldkey <- get
        if k == oldkey
            then do
                mnewkey <- lift $ newKeyGetter update oldkey mr
                case mnewkey of
                    Just newkey -> do
                        put newkey
                        return [SumUpdateRight (MkOneUpdate update)]
                    Nothing -> return []
            else return []
    sUpdate (KeyUpdateInsertReplace item) _ = do
        key <- get
        return $
            if elementKey @cont item == key
                then [SumUpdateLeft (MkWholeReaderUpdate (Just item))]
                else []
    sPutEdit ::
           forall m. MonadIO m
        => MaybeEdit (UpdateEdit update)
        -> MutableRead m (KeyReader cont (UpdateReader update))
        -> StateT (ContainerKey cont) m (Maybe [KeyEdit cont (UpdateEdit update)])
    sPutEdit (SumEditLeft (MkWholeReaderEdit (Just subj))) _ = return $ Just [KeyEditInsertReplace subj]
    sPutEdit (SumEditLeft (MkWholeReaderEdit Nothing)) _ = do
        key <- get
        return $ Just [KeyEditDelete key]
    sPutEdit (SumEditRight (MkOneEdit edit)) mr = do
        oldkey <- get
        mnewkey <- lift $ getComposeM $ readKey @cont $ applyEdit edit $ keyItemReadFunction oldkey mr
        case mnewkey of
            Just newkey -> do
                put newkey
                return $ Just [KeyEditItem oldkey edit]
            Nothing -> return $ Just []
    sPutEdits ::
           forall m. MonadIO m
        => [MaybeEdit (UpdateEdit update)]
        -> MutableRead m (KeyReader cont (UpdateReader update))
        -> StateT (ContainerKey cont) m (Maybe [KeyEdit cont (UpdateEdit update)])
    sPutEdits = elPutEditsFromPutEdit sPutEdit
    in MkStateEditLens {..}

getKeyElementEditLens ::
       forall cont update.
       ( KeyContainer cont
       , IsEditUpdate update
       , HasKeyReader cont (UpdateReader update)
       , ApplicableEdit (UpdateEdit update)
       , FullSubjectReader (UpdateReader update)
       )
    => ContainerKey cont
    -> StateEditLens (KeyUpdate cont update) (MaybeUpdate update)
getKeyElementEditLens key = unliftKeyElementEditLens updateKey key

stableKeyElementEditLens ::
       forall cont update.
       ( KeyContainer cont
       , HasKeyReader cont (UpdateReader update)
       , ApplicableEdit (UpdateEdit update)
       , FullSubjectReader (UpdateReader update)
       )
    => ContainerKey cont
    -> EditLens (KeyUpdate cont update) (MaybeUpdate update)
stableKeyElementEditLens key = discardingStateLens $ unliftKeyElementEditLens useOldKey key

{-
getKeyValueEditLens ::
       forall cont keyupdate valueupdate.
       ( KeyContainer cont
       , IsEditUpdate keyupdate
       , IsEditUpdate valueupdate
       , HasKeyReader cont (PairUpdateReader keyupdate valueupdate)
       , ApplicableEdit (UpdateEdit keyupdate)
       , FullSubjectReader (UpdateReader keyupdate)
       , FullEdit (UpdateEdit valueupdate)
       )
    => ContainerKey cont
    -> StateEditLens (KeyUpdate cont (PairUpdate keyupdate valueupdate)) (MaybeUpdate valueupdate)
getKeyValueEditLens key = (toStateEditLens $ oneWholeLiftEditLens $ tupleEditLens SelectSecond) . getKeyElementEditLens key
-}
liftKeyElementAnUpdateFunction ::
       forall conta contb updateA updateB.
       ( ContainerKey conta ~ ContainerKey contb
       , UpdateSubject updateA ~ Element conta
       , UpdateSubject updateB ~ Element contb
       , SubjectReader (UpdateReader updateA)
       , FullSubjectReader (UpdateReader updateB)
       )
    => UpdateFunction updateA updateB
    -> UpdateFunction (KeyUpdate conta updateA) (KeyUpdate contb updateB)
liftKeyElementAnUpdateFunction (MkUpdateFunction g u) = let
    ufGet :: ReadFunction (KeyReader conta (UpdateReader updateA)) (KeyReader contb (UpdateReader updateB))
    ufGet mr KeyReadKeys = mr KeyReadKeys
    ufGet (mr :: MutableRead m _) (KeyReadItem key rt) = getComposeM $ g (keyItemReadFunction key mr) rt
    ufUpdate ::
           forall m. MonadIO m
        => KeyUpdate conta updateA
        -> MutableRead m (KeyReader conta (UpdateReader updateA))
        -> m [KeyUpdate contb updateB]
    ufUpdate KeyUpdateClear _ = return [KeyUpdateClear]
    ufUpdate (KeyUpdateInsertReplace itema) _ = do
        itemb <- mutableReadToSubject $ g $ subjectToMutableRead @m itema
        return [KeyUpdateInsertReplace itemb]
    ufUpdate (KeyUpdateDelete key) _ = return [KeyUpdateDelete key]
    ufUpdate (KeyUpdateItem key ea) mr = do
        mresult <- getComposeM $ u ea (keyItemReadFunction @conta key mr)
        case mresult of
            Just ebs -> return $ fmap (KeyUpdateItem key) ebs
            Nothing -> return []
    in MkUpdateFunction {..}

liftKeyElementUpdateFunction ::
       forall conta contb updateA updateB.
       ( ContainerKey conta ~ ContainerKey contb
       , UpdateSubject updateA ~ Element conta
       , UpdateSubject updateB ~ Element contb
       , SubjectReader (UpdateReader updateA)
       , FullSubjectReader (UpdateReader updateB)
       )
    => UpdateFunction updateA updateB
    -> UpdateFunction (KeyUpdate conta updateA) (KeyUpdate contb updateB)
liftKeyElementUpdateFunction = liftKeyElementAnUpdateFunction

liftKeyElementEditLens ::
       forall conta contb updateA updateB.
       ( ContainerKey conta ~ ContainerKey contb
       , KeyContainer conta
       , HasKeyReader conta (UpdateReader updateA)
       , UpdateSubject updateA ~ Element conta
       , UpdateSubject updateB ~ Element contb
       , ApplicableEdit (UpdateEdit updateA)
       , FullSubjectReader (UpdateReader updateA)
       , FullSubjectReader (UpdateReader updateB)
       )
    => (forall m. MonadIO m => UpdateSubject updateB -> m (Maybe (UpdateSubject updateA)))
    -> EditLens updateA updateB
    -> EditLens (KeyUpdate conta updateA) (KeyUpdate contb updateB)
liftKeyElementEditLens bma (MkEditLens ef pe) = let
    elFunction = liftKeyElementAnUpdateFunction ef
    elPutEdit ::
           forall m. MonadIO m
        => KeyEdit contb (UpdateEdit updateB)
        -> MutableRead m (KeyReader conta (UpdateReader updateA))
        -> m (Maybe [KeyEdit conta (UpdateEdit updateA)])
    elPutEdit KeyEditClear _ = return $ Just [KeyEditClear]
    elPutEdit (KeyEditInsertReplace itemb) _ = do
        fitema <- bma itemb
        return $ fmap (\itema -> [KeyEditInsertReplace itema]) fitema
    elPutEdit (KeyEditDelete key) _ = return $ Just [KeyEditDelete key]
    elPutEdit (KeyEditItem key eb) mr = do
        mfresult <- getComposeM $ pe [eb] (keyItemReadFunction @conta key mr)
        case mfresult of
            Just fsea -> return $ fmap (fmap $ KeyEditItem key) fsea
            Nothing -> return $ Just []
    elPutEdits ::
           forall m. MonadIO m
        => [KeyEdit contb (UpdateEdit updateB)]
        -> MutableRead m (KeyReader conta (UpdateReader updateA))
        -> m (Maybe [KeyEdit conta (UpdateEdit updateA)])
    elPutEdits = elPutEditsFromPutEdit elPutEdit
    in MkEditLens {..}

findBy :: (a -> a -> Ordering) -> [a] -> a -> (Int, Bool)
findBy cmp items x = let
    ords = fmap (cmp x) items
    in (length $ filter ((==) GT) ords, any ((==) EQ) ords)

orderedKeyList ::
       forall cont seq update.
       ( IsUpdate update
       , Index seq ~ Int
       , Element cont ~ UpdateSubject update
       , KeyContainer cont
       , FullEdit (UpdateEdit update)
       )
    => (UpdateSubject update -> UpdateSubject update -> Ordering)
    -> UpdateFunction (KeyUpdate cont update) (ListUpdate seq update)
orderedKeyList cmp = let
    getUnsortedPairs ::
           forall m. MonadIO m
        => MutableRead m (KeyReader cont (UpdateReader update))
        -> m [(ContainerKey cont, UpdateSubject update)]
    getUnsortedPairs mr = do
        keyset <- mr KeyReadKeys
        for (toList keyset) $ \key -> do
            t <- mutableReadToSubject $ knownKeyItemReadFunction key mr
            return (key, t)
    getSortedKeys ::
           forall m. MonadIO m
        => MutableRead m (KeyReader cont (UpdateReader update))
        -> m [ContainerKey cont]
    getSortedKeys mr = do
        pairs <- getUnsortedPairs mr
        return $ fmap fst $ sortBy (\p q -> cmp (snd p) (snd q)) pairs
    findKey ::
           forall m. MonadIO m
        => ContainerKey cont
        -> MutableRead m (KeyReader cont (UpdateReader update))
        -> m (SequencePoint seq, Bool)
    findKey key mr = do
        pairs <- getUnsortedPairs mr
        skey <- mutableReadToSubject $ knownKeyItemReadFunction key mr
        return $ (\(i, found) -> (MkSequencePoint i, found)) $ findBy cmp (fmap snd pairs) skey
    ufGet :: ReadFunction (KeyReader cont (UpdateReader update)) (ListReader seq (UpdateReader update))
    ufGet mr ListReadLength = do
        keyset <- mr KeyReadKeys
        return $ MkSequencePoint $ length keyset
    ufGet mr (ListReadItem (MkSequencePoint i) reader) = do
        keylist <- getSortedKeys mr
        case index keylist i of
            Just key -> mr $ KeyReadItem key reader
            Nothing -> return Nothing
    ufUpdate ::
           forall m. MonadIO m
        => KeyUpdate cont update
        -> MutableRead m (KeyReader cont (UpdateReader update))
        -> m [ListUpdate seq update]
    ufUpdate (KeyUpdateItem key update) mr = do
        (i, found) <- findKey key mr
        return $
            if found
                then [ListUpdateItem i update]
                else []
    ufUpdate (KeyUpdateDelete key) mr = do
        (i, found) <- findKey key mr
        return $
            if found
                then [ListUpdateDelete i]
                else []
    ufUpdate (KeyUpdateInsertReplace item) mr = do
        (i, found) <- findKey (elementKey @cont item) mr
        if found
            then return [ListUpdateInsert i item]
            else do
                edits <- getReplaceEditsFromSubject item
                return $ fmap (\edit -> ListUpdateItem i $ editUpdate edit) edits
    ufUpdate KeyUpdateClear _ = return [ListUpdateClear]
    in MkUpdateFunction {..}

class HasKeyReader cont (UpdateReader update) => HasKeyUpdate cont update where
    updatesKey :: update -> Maybe (ContainerKey cont -> ContainerKey cont)

data UpdateOrder update =
    forall o. MkUpdateOrder (o -> o -> Ordering)
                            (UpdateFunction update (WholeUpdate o))

orderedSetLens ::
       forall update cont seq.
       ( Index seq ~ Int
       , Element cont ~ UpdateSubject update
       , KeyContainer cont
       , HasKeyUpdate cont update
       , FullSubjectReader (UpdateReader update)
       , ApplicableEdit (UpdateEdit update)
       , IsUpdate update
       )
    => UpdateOrder update
    -> StateEditLens (KeyUpdate cont update) (OrderedListUpdate seq update)
orderedSetLens (MkUpdateOrder (cmp :: o -> o -> Ordering) (MkUpdateFunction ordGet ordUpdate)) = let
    kcmp :: (o, ContainerKey cont) -> (o, ContainerKey cont) -> Ordering
    kcmp (o1, k1) (o2, k2) =
        case cmp o1 o2 of
            EQ ->
                if k1 == k2
                    then EQ
                    else LT
            c -> c
    getMaybeO ::
           forall m. MonadIO m
        => MutableRead m (KeyReader cont (UpdateReader update))
        -> ContainerKey cont
        -> m (Maybe o)
    getMaybeO mr k = getComposeM $ ordGet (\rt -> MkComposeM $ mr $ KeyReadItem k rt) ReadWhole
    getO ::
           forall m. MonadIO m
        => MutableRead m (KeyReader cont (UpdateReader update))
        -> ContainerKey cont
        -> m o
    getO mr k = do
        mo <- getMaybeO mr k
        case mo of
            Just o -> return o
            Nothing -> liftIO $ fail "orderedSetLens: missing key"
    sInit ::
           forall m. MonadIO m
        => MutableRead m (KeyReader cont (UpdateReader update))
        -> m (OrderedList (o, ContainerKey cont))
    sInit mr = do
        MkFiniteSet kk <- mr KeyReadKeys
        pairs <-
            for kk $ \k -> do
                o <- getO mr k
                return (o, k)
        return $ olFromList kcmp pairs
    sGet ::
           ReadFunctionT (StateT (OrderedList (o, ContainerKey cont))) (KeyReader cont (UpdateReader update)) (ListReader seq (UpdateReader update))
    sGet _ ListReadLength = do
        ol <- get
        return $ MkSequencePoint $ olLength ol
    sGet mr (ListReadItem (MkSequencePoint i) rt) = do
        ol <- get
        case olGetByPos ol i of
            Just (_, key) -> lift $ mr $ KeyReadItem key rt
            Nothing -> return Nothing
    lookUpByKey :: OrderedList (o, ContainerKey cont) -> ContainerKey cont -> Maybe (o, Int)
    lookUpByKey ol key = do
        ((o, _), pos) <- olLookupByPredicate ol $ \(_, k) -> k == key
        return (o, pos)
    sUpdate ::
           forall m. MonadIO m
        => KeyUpdate cont update
        -> MutableRead m (KeyReader cont (UpdateReader update))
        -> StateT (OrderedList (o, ContainerKey cont)) m [OrderedListUpdate seq update]
    sUpdate (KeyUpdateItem oldkey update) newmr = do
        ol <- get
        case lookUpByKey ol oldkey of
            Nothing -> return [] -- key not found, no change
            Just (oldO, oldPos) -> do
                let
                    mnewkey = do
                        mapkey <- updatesKey @cont update
                        let newkey = mapkey oldkey
                        if newkey == oldkey
                            then Nothing
                            else return newkey
                case mnewkey of
                        -- key hasn't changed
                    Nothing -> do
                        ws <- lift $ ordUpdate update $ knownKeyItemReadFunction oldkey newmr
                        case lastWholeUpdate ws of
                                    -- order hasn't changed
                            Nothing ->
                                return [OrderedListUpdateItem (MkSequencePoint oldPos) (MkSequencePoint oldPos) update] -- key & order unchanged
                            Just newO -> do
                                let (newPos, newOL) = olInsert (newO, oldkey) $ olDeleteByPos oldPos ol
                                put newOL
                                return [OrderedListUpdateItem (MkSequencePoint oldPos) (MkSequencePoint newPos) update]
                        -- key changed
                    Just newkey -> do
                        ws <- lift $ ordUpdate update $ knownKeyItemReadFunction newkey newmr
                        let
                            newO =
                                case lastWholeUpdate ws of
                                    Just o -> o
                                    Nothing -> oldO
                        let (newPos, newOL) = olInsert (newO, newkey) $ olDeleteByPos oldPos ol
                        put newOL
                        return [OrderedListUpdateItem (MkSequencePoint oldPos) (MkSequencePoint newPos) update]
    sUpdate (KeyUpdateDelete key) _mr = do
        ol <- get
        case lookUpByKey ol key of
            Just (_, pos) -> do
                put $ olDeleteByPos pos ol
                return [OrderedListUpdateDelete $ MkSequencePoint pos]
            Nothing -> return []
    sUpdate (KeyUpdateInsertReplace newitem) _mr = do
        ol <- get
        let
            imr :: MutableRead (StateT (OrderedList (o, ContainerKey cont)) m) (UpdateReader update)
            imr = subjectToMutableRead newitem
        key <- readKey @cont imr
        o <- ordGet imr ReadWhole
        let (found, MkSequencePoint -> pos) = olLookupByItem ol (o, key)
        if found
            then return [OrderedListUpdateDelete pos, OrderedListUpdateInsert pos newitem]
            else case lookUpByKey ol key of
                     Just (_, oldpos) -> do
                         put $ snd $ olInsert (o, key) $ olDeleteByPos oldpos ol
                         return [OrderedListUpdateDelete $ MkSequencePoint oldpos, OrderedListUpdateInsert pos newitem]
                     Nothing -> do
                         put $ snd $ olInsert (o, key) ol
                         return [OrderedListUpdateInsert pos newitem]
    sUpdate KeyUpdateClear _ = do
        put $ olEmpty kcmp
        return [OrderedListUpdateClear]
    sPutEdit ::
           forall m. MonadIO m
        => OrderedListEdit seq (UpdateEdit update)
        -> MutableRead m (KeyReader cont (UpdateReader update))
        -> StateT (OrderedList (o, ContainerKey cont)) m (Maybe [KeyEdit cont (UpdateEdit update)])
    sPutEdit OrderedListEditClear _ = do
        put $ olEmpty kcmp
        return $ Just [KeyEditClear]
    sPutEdit (OrderedListEditDelete (MkSequencePoint pos)) _ = do
        ol <- get
        case olGetByPos ol pos of
            Nothing -> return $ Just []
            Just (_, key) -> do
                put $ olDeleteByPos pos ol
                return $ Just [KeyEditDelete key]
    sPutEdit (OrderedListEditItem (MkSequencePoint oldPos) edit) oldmr = do
        ol <- get
        case olGetByPos ol oldPos of
            Nothing -> return $ Just []
            Just (oldO, oldkey) -> do
                let
                    update = editUpdate edit
                    mnewkey = do
                        mapkey <- updatesKey @cont update
                        let newkey = mapkey oldkey
                        if newkey == oldkey
                            then Nothing
                            else return newkey
                case mnewkey of
                        -- key hasn't changed
                    Nothing -> do
                        ws <- lift $ ordUpdate update $ knownKeyItemReadFunction oldkey oldmr
                        case lastWholeUpdate ws of
                                    -- order hasn't changed
                            Nothing -> return $ Just [KeyEditItem oldkey edit] -- key & order unchanged
                            Just newO -> do
                                let (_, newOL) = olInsert (newO, oldkey) $ olDeleteByPos oldPos ol
                                put newOL
                                return $ Just [KeyEditItem oldkey edit]
                        -- key changed
                    Just newkey -> do
                        ws <- lift $ ordUpdate update $ knownKeyItemReadFunction newkey oldmr
                        let
                            newO =
                                case lastWholeUpdate ws of
                                    Just o -> o
                                    Nothing -> oldO
                        let (_, newOL) = olInsert (newO, newkey) $ olDeleteByPos oldPos ol
                        put newOL
                        return $ Just [KeyEditItem oldkey edit]
    sPutEdits ::
           forall m. MonadIO m
        => [OrderedListEdit seq (UpdateEdit update)]
        -> MutableRead m (KeyReader cont (UpdateReader update))
        -> StateT (OrderedList (o, ContainerKey cont)) m (Maybe [KeyEdit cont (UpdateEdit update)])
    sPutEdits = elPutEditsFromPutEdit sPutEdit
    in MkStateEditLens {..}
