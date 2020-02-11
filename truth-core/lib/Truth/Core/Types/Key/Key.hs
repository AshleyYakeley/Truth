module Truth.Core.Types.Key.Key
    ( KeyReader(..)
    , KeyEdit(..)
    , KeyUpdate(..)
    , keyElementEditLens
    , fixedKeyElementEditLens
    , liftKeyElementEditLens
    , liftKeyElementFloatingEditLens
    , UpdateOrder(..)
    , mapUpdateOrder
    , mapReadOnlyUpdateOrder
    , orderedSetLens
    , contextOrderedSetLens
    , contextKeyEditLens
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Lens
import Truth.Core.Read
import Truth.Core.Sequence
import Truth.Core.Types.Key.HasKey
import Truth.Core.Types.List
import Truth.Core.Types.One.FullResult
import Truth.Core.Types.One.Read
import Truth.Core.Types.One.Result
import Truth.Core.Types.ReadOnly
import Truth.Core.Types.Tuple.Context
import Truth.Core.Types.Tuple.Tuple
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

instance (KeyContainer cont, SubjectReader reader, ReaderSubject reader ~ Item cont) =>
             SubjectReader (KeyReader cont reader) where
    type ReaderSubject (KeyReader cont reader) = cont
    subjectToRead cont KeyReadKeys = MkFiniteSet $ keys cont
    subjectToRead cont (KeyReadItem key reader) = fmap (\e -> subjectToRead e reader) $ lookupItem key cont

instance (KeyContainer cont, FullSubjectReader reader, ReaderSubject reader ~ Item cont) =>
             FullSubjectReader (KeyReader cont reader) where
    mutableReadToSubject mr = do
        MkFiniteSet allkeys <- mr KeyReadKeys
        list <- for allkeys $ \key -> mutableReadToSubject $ knownKeyItemReadFunction key mr
        return $ fromItemList list

--- edit
data KeyEdit cont edit where
    KeyEditItem :: ContainerKey cont -> edit -> KeyEdit cont edit
    KeyEditDelete :: ContainerKey cont -> KeyEdit cont edit
    KeyEditInsertReplace :: Item cont -> KeyEdit cont edit
    KeyEditClear :: KeyEdit cont edit

instance (Show (ContainerKey cont), Show edit, Show (Item cont)) => Show (KeyEdit cont edit) where
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

instance ( FullSubjectReader (EditReader edit)
         , ApplicableEdit edit
         , HasKeyReader cont (EditReader edit)
         , Item cont ~ EditSubject edit
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
        let newkey = itemKey @cont item
        if elem newkey allkeys
            then return allkeys
            else return $ insertSet newkey allkeys
    applyEdit (KeyEditInsertReplace item) _mr (KeyReadItem key reader)
        | itemKey @cont item == key = return $ Just $ subjectToRead item reader
    applyEdit (KeyEditInsertReplace _) mr (KeyReadItem key reader) = mr $ KeyReadItem key reader
    applyEdit KeyEditClear _mr reader = mSubjectToMutableRead (return mempty) reader

instance ( FullSubjectReader (EditReader edit)
         , ApplicableEdit edit
         , SubjectMapEdit edit
         , InvertibleEdit edit
         , HasKeyReader cont (EditReader edit)
         , Item cont ~ EditSubject edit
         ) => InvertibleEdit (KeyEdit cont edit) where
    invertEdit (KeyEditItem key edit) mr = do
        minvedits <- getComposeM $ invertEdit edit $ keyItemReadFunction key mr
        case minvedits of
            Just invedits -> return $ fmap (KeyEditItem key) invedits
            Nothing -> return []
    invertEdit (KeyEditInsertReplace item) mr = do
        let newkey = itemKey @cont item
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

instance (KeyContainer cont, EditSubject edit ~ Item cont, SubjectMapEdit edit) => SubjectMapEdit (KeyEdit cont edit) where
    mapSubjectEdits =
        mapEditToMapEdits $ \keyedit subj ->
            case keyedit of
                KeyEditItem key edit ->
                    case lookupItem key subj of
                        Nothing -> return subj
                        Just oldelem -> do
                            newelem <- mapSubjectEdits [edit] oldelem
                            return $ insertItem newelem subj
                KeyEditDelete key -> return $ deleteKey key subj
                KeyEditInsertReplace item -> return $ insertItem item subj
                KeyEditClear -> return mempty

instance ( FullSubjectReader (EditReader edit)
         , ApplicableEdit edit
         , SubjectMapEdit edit
         , HasKeyReader cont (EditReader edit)
         , Item cont ~ EditSubject edit
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
    KeyUpdateInsertReplace :: Item cont -> KeyUpdate cont update
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

instance ( IsEditUpdate update
         , FullSubjectReader (UpdateReader update)
         , ApplicableEdit (UpdateEdit update)
         , HasKeyReader cont (UpdateReader update)
         , Item cont ~ UpdateSubject update
         ) => ApplicableUpdate (KeyUpdate cont update)

keyElementEditLens ::
       forall cont update.
       ( HasKeyUpdate cont update
       , ApplicableEdit (UpdateEdit update)
       , FullSubjectReader (UpdateReader update)
       , Item cont ~ UpdateSubject update
       )
    => ContainerKey cont
    -> FloatingEditLens (KeyUpdate cont update) (MaybeUpdate update)
keyElementEditLens initKey = let
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
    sUpdate KeyUpdateClear _ = return [MkFullResultOneUpdate $ NewResultOneUpdate Nothing]
    sUpdate (KeyUpdateDelete k) _ = do
        key <- get
        return $
            if k == key
                then [MkFullResultOneUpdate $ NewResultOneUpdate Nothing]
                else []
    sUpdate (KeyUpdateItem k update) _ = do
        oldkey <- get
        if k == oldkey
            then do
                case updatesKey @cont update of
                    Just mapkey -> do
                        newkey <- liftIO $ mapkey oldkey
                        put newkey
                    Nothing -> return ()
                return [MkFullResultOneUpdate $ SuccessResultOneUpdate update]
            else return []
    sUpdate (KeyUpdateInsertReplace item) _ = do
        key <- get
        return $
            if itemKey @cont item == key
                then [MkFullResultOneUpdate $ NewResultOneUpdate $ Just ()]
                else []
    sPutEdit ::
           forall m. MonadIO m
        => MaybeEdit (UpdateEdit update)
        -> MutableRead m (KeyReader cont (UpdateReader update))
        -> StateT (ContainerKey cont) m (Maybe [KeyEdit cont (UpdateEdit update)])
    sPutEdit (NewFullResultOneEdit (Just subj)) _ = return $ Just [KeyEditInsertReplace subj]
    sPutEdit (NewFullResultOneEdit Nothing) _ = do
        key <- get
        return $ Just [KeyEditDelete key]
    sPutEdit (SuccessFullResultOneEdit edit) mr = do
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
    in makeStateLens MkStateEditLens {..}

fixedKeyElementEditLens ::
       forall cont update.
       ( HasKeyUpdate cont update
       , ApplicableEdit (UpdateEdit update)
       , FullSubjectReader (UpdateReader update)
       , Item cont ~ UpdateSubject update
       )
    => ContainerKey cont
    -> EditLens (KeyUpdate cont update) (MaybeUpdate update)
fixedKeyElementEditLens key = floatingToDiscardingEditLens $ keyElementEditLens key

type InternalKeyMap key r = [(key, r)]

liftKeyElementEditLens ::
       forall conta contb updateA updateB.
       ( ContainerKey conta ~ ContainerKey contb
       , Eq (ContainerKey conta)
       , HasKeyReader conta (UpdateReader updateA)
       , UpdateSubject updateA ~ Item conta
       , UpdateSubject updateB ~ Item contb
       , ApplicableEdit (UpdateEdit updateA)
       , FullSubjectReader (UpdateReader updateA)
       , FullSubjectReader (UpdateReader updateB)
       )
    => (forall m. MonadIO m => UpdateSubject updateB -> m (Maybe (UpdateSubject updateA)))
    -> EditLens updateA updateB
    -> EditLens (KeyUpdate conta updateA) (KeyUpdate contb updateB)
liftKeyElementEditLens bma (MkEditLens g u pe) = let
    elGet :: ReadFunction (KeyReader conta (UpdateReader updateA)) (KeyReader contb (UpdateReader updateB))
    elGet mr KeyReadKeys = mr KeyReadKeys
    elGet (mr :: MutableRead m _) (KeyReadItem key rt) = getComposeM $ g (keyItemReadFunction key mr) rt
    elUpdate ::
           forall m. MonadIO m
        => KeyUpdate conta updateA
        -> MutableRead m (KeyReader conta (UpdateReader updateA))
        -> m [KeyUpdate contb updateB]
    elUpdate KeyUpdateClear _ = return [KeyUpdateClear]
    elUpdate (KeyUpdateInsertReplace itema) _ = do
        itemb <- mutableReadToSubject $ g $ subjectToMutableRead @m itema
        return [KeyUpdateInsertReplace itemb]
    elUpdate (KeyUpdateDelete key) _ = return [KeyUpdateDelete key]
    elUpdate (KeyUpdateItem key ea) mr = do
        mresult <- getComposeM $ u ea (keyItemReadFunction @conta key mr)
        case mresult of
            Just ebs -> return $ fmap (KeyUpdateItem key) ebs
            Nothing -> return []
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
        return $
            case mfresult of
                Just fsea -> fmap (fmap $ KeyEditItem key) fsea
                Nothing -> Just []
    elPutEdits ::
           forall m. MonadIO m
        => [KeyEdit contb (UpdateEdit updateB)]
        -> MutableRead m (KeyReader conta (UpdateReader updateA))
        -> m (Maybe [KeyEdit conta (UpdateEdit updateA)])
    elPutEdits = elPutEditsFromPutEdit elPutEdit
    in MkEditLens {..}

liftKeyElementFloatingEditLens ::
       forall conta contb updateA updateB.
       ( ContainerKey conta ~ ContainerKey contb
       , Eq (ContainerKey conta)
       , HasKeyReader conta (UpdateReader updateA)
       , UpdateSubject updateA ~ Item conta
       , UpdateSubject updateB ~ Item contb
       , ApplicableEdit (UpdateEdit updateA)
       , FullSubjectReader (UpdateReader updateA)
       , FullSubjectReader (UpdateReader updateB)
       )
    => (forall m. MonadIO m => UpdateSubject updateB -> m (Maybe (UpdateSubject updateA)))
    -> FloatingEditLens updateA updateB
    -> FloatingEditLens (KeyUpdate conta updateA) (KeyUpdate contb updateB)
liftKeyElementFloatingEditLens bma (MkFloatingEditLens NoFloatInit rlens) =
    editLensToFloating $ liftKeyElementEditLens bma $ rlens ()
liftKeyElementFloatingEditLens bma (MkFloatingEditLens (ReadFloatInit init :: FloatInit _ r) rlens) = let
    sInit :: StateLensInit (KeyReader conta (UpdateReader updateA)) (InternalKeyMap (ContainerKey conta) r)
    sInit _ = return mempty
    getR ::
           forall m. MonadIO m
        => MutableRead m (KeyReader conta (UpdateReader updateA))
        -> ContainerKey conta
        -> StateT (InternalKeyMap (ContainerKey conta) r) m (Maybe r)
    getR mr key = do
        rmap <- get
        case lookup key rmap of
            Just r -> return $ Just r
            Nothing -> do
                mnewr <- lift $ getComposeM $ init $ \rt -> MkComposeM $ mr $ KeyReadItem key rt
                case mnewr of
                    Just newr -> put $ insertMap key newr rmap
                    Nothing -> return ()
                return mnewr
    sGet ::
           ReadFunctionT (StateT (InternalKeyMap (ContainerKey conta) r)) (KeyReader conta (UpdateReader updateA)) (KeyReader contb (UpdateReader updateB))
    sGet mr KeyReadKeys = lift $ mr KeyReadKeys
    sGet (mr :: MutableRead m _) (KeyReadItem key rt) = do
        mrc <- getR mr key
        lift $
            getComposeM $ do
                r <- MkComposeM $ return mrc
                elGet (rlens r) (keyItemReadFunction key mr) rt
    sUpdate ::
           forall m. MonadIO m
        => KeyUpdate conta updateA
        -> MutableRead m (KeyReader conta (UpdateReader updateA))
        -> StateT (InternalKeyMap (ContainerKey conta) r) m [KeyUpdate contb updateB]
    sUpdate KeyUpdateClear _ = return [KeyUpdateClear]
    sUpdate (KeyUpdateInsertReplace itema) _ = do
        let
            imr :: MutableRead (StateT (InternalKeyMap (ContainerKey conta) r) m) (UpdateReader updateA)
            imr = subjectToMutableRead itema
        r <- init imr
        itemb <- lift $ mutableReadToSubject $ elGet (rlens r) $ subjectToMutableRead @m itema
        rmap <- get
        key <- readKey @conta imr
        put $ insertMap key r rmap
        return [KeyUpdateInsertReplace itemb]
    sUpdate (KeyUpdateDelete key) _ = return [KeyUpdateDelete key]
    sUpdate (KeyUpdateItem key ea) mr = do
        mrc <- getR mr key
        mresult <-
            lift $
            getComposeM $ do
                r <- MkComposeM $ return mrc
                elUpdate (rlens r) ea (keyItemReadFunction @conta key mr)
        case mresult of
            Just ebs -> return $ fmap (KeyUpdateItem key) ebs
            Nothing -> return []
    sPutEdit ::
           forall m. MonadIO m
        => KeyEdit contb (UpdateEdit updateB)
        -> MutableRead m (KeyReader conta (UpdateReader updateA))
        -> StateT (InternalKeyMap (ContainerKey conta) r) m (Maybe [KeyEdit conta (UpdateEdit updateA)])
    sPutEdit KeyEditClear _ = return $ Just [KeyEditClear]
    sPutEdit (KeyEditInsertReplace itemb) _ = do
        fitema <- lift $ bma itemb
        return $ fmap (\itema -> [KeyEditInsertReplace itema]) fitema
    sPutEdit (KeyEditDelete key) _ = return $ Just [KeyEditDelete key]
    sPutEdit (KeyEditItem key eb) mr = do
        mrc <- getR mr key
        mfresult <-
            lift $
            getComposeM $ do
                r <- MkComposeM $ return mrc
                elPutEdits (rlens r) [eb] (keyItemReadFunction @conta key mr)
        return $
            case mfresult of
                Just fsea -> fmap (fmap $ KeyEditItem key) fsea
                Nothing -> Just []
    sPutEdits ::
           forall m. MonadIO m
        => [KeyEdit contb (UpdateEdit updateB)]
        -> MutableRead m (KeyReader conta (UpdateReader updateA))
        -> StateT (InternalKeyMap (ContainerKey conta) r) m (Maybe [KeyEdit conta (UpdateEdit updateA)])
    sPutEdits = elPutEditsFromPutEdit sPutEdit
    in makeStateLens MkStateEditLens {..}

data UpdateOrder update =
    forall o. MkUpdateOrder (o -> o -> Ordering)
                            (FloatingEditLens update (ReadOnlyUpdate (WholeUpdate o)))

mapUpdateOrder :: FloatingEditLens updateB updateA -> UpdateOrder updateA -> UpdateOrder updateB
mapUpdateOrder lens (MkUpdateOrder cmp flens) = MkUpdateOrder cmp $ flens . lens

mapReadOnlyUpdateOrder ::
       FloatingEditLens updateB (ReadOnlyUpdate updateA) -> UpdateOrder updateA -> UpdateOrder updateB
mapReadOnlyUpdateOrder lens (MkUpdateOrder cmp flens) = MkUpdateOrder cmp $ liftReadOnlyFloatingEditLens flens . lens

orderedSetLens ::
       forall update cont seq.
       ( Index seq ~ Int
       , HasKeyUpdate cont update
       , FullSubjectReader (UpdateReader update)
       , Item cont ~ UpdateSubject update
       , ApplicableEdit (UpdateEdit update)
       , IsUpdate update
       )
    => UpdateOrder update
    -> FloatingEditLens (KeyUpdate cont update) (OrderedListUpdate seq update)
orderedSetLens (MkUpdateOrder (cmp :: o -> o -> Ordering) (MkFloatingEditLens (ordInit :: FloatInit _ or) rOrdLens)) = let
    kcmp :: (o, ContainerKey cont, or) -> (o, ContainerKey cont, or) -> Ordering
    kcmp (o1, k1, _) (o2, k2, _) =
        case cmp o1 o2 of
            EQ ->
                if k1 == k2
                    then EQ
                    else LT
            c -> c
    getMaybeO ::
           forall m. MonadIO m
        => or
        -> MutableRead m (KeyReader cont (UpdateReader update))
        -> ContainerKey cont
        -> m (Maybe o)
    getMaybeO ordr mr k = getComposeM $ elGet (rOrdLens ordr) (\rt -> MkComposeM $ mr $ KeyReadItem k rt) ReadWhole
    getO ::
           forall m. MonadIO m
        => or
        -> MutableRead m (KeyReader cont (UpdateReader update))
        -> ContainerKey cont
        -> m o
    getO ordr mr k = do
        mo <- getMaybeO ordr mr k
        case mo of
            Just o -> return o
            Nothing -> liftIO $ fail "orderedSetLens: missing key"
    sInit ::
           forall m. MonadIO m
        => MutableRead m (KeyReader cont (UpdateReader update))
        -> m (OrderedList (o, ContainerKey cont, or))
    sInit mr = do
        MkFiniteSet kk <- mr KeyReadKeys
        pairs <-
            for kk $ \k -> do
                ordr <- runFloatInit ordInit $ knownKeyItemReadFunction k mr
                o <- getO ordr mr k
                return (o, k, ordr)
        return $ olFromList kcmp pairs
    sGet ::
           ReadFunctionT (StateT (OrderedList (o, ContainerKey cont, or))) (KeyReader cont (UpdateReader update)) (ListReader seq (UpdateReader update))
    sGet _ ListReadLength = do
        ol <- get
        return $ MkSequencePoint $ olLength ol
    sGet mr (ListReadItem (MkSequencePoint i) rt) = do
        ol <- get
        case olGetByPos ol i of
            Just (_, key, _) -> lift $ mr $ KeyReadItem key rt
            Nothing -> return Nothing
    lookUpByKey :: OrderedList (o, ContainerKey cont, or) -> ContainerKey cont -> Maybe (o, Int, or)
    lookUpByKey ol key = do
        ((o, _, ordr), pos) <- olLookupByPredicate ol $ \(_, k, _) -> k == key
        return (o, pos, ordr)
    sUpdate ::
           forall m. MonadIO m
        => KeyUpdate cont update
        -> MutableRead m (KeyReader cont (UpdateReader update))
        -> StateT (OrderedList (o, ContainerKey cont, or)) m [OrderedListUpdate seq update]
    sUpdate (KeyUpdateItem oldkey update) newmr = do
        ol <- get
        case lookUpByKey ol oldkey of
            Nothing -> return [] -- key not found, no change
            Just (oldO, oldPos, ordr) -> do
                mnewkey <-
                    case updatesKey @cont update of
                        Just mapkey -> do
                            newkey <- liftIO $ mapkey oldkey
                            return $
                                if newkey == oldkey
                                    then Nothing
                                    else Just newkey
                        Nothing -> return Nothing
                case mnewkey of
                        -- key hasn't changed
                    Nothing -> do
                        ws <- lift $ elUpdate (rOrdLens ordr) update $ knownKeyItemReadFunction oldkey newmr
                        case lastReadOnlyWholeUpdate ws of
                                    -- order hasn't changed
                            Nothing ->
                                return [OrderedListUpdateItem (MkSequencePoint oldPos) (MkSequencePoint oldPos) update] -- key & order unchanged
                            Just newO -> do
                                let (newPos, newOL) = olInsert (newO, oldkey, ordr) $ olDeleteByPos oldPos ol
                                put newOL
                                return [OrderedListUpdateItem (MkSequencePoint oldPos) (MkSequencePoint newPos) update]
                        -- key changed
                    Just newkey -> do
                        ws <- lift $ elUpdate (rOrdLens ordr) update $ knownKeyItemReadFunction newkey newmr
                        let
                            newO =
                                case lastReadOnlyWholeUpdate ws of
                                    Just o -> o
                                    Nothing -> oldO
                        let (newPos, newOL) = olInsert (newO, newkey, ordr) $ olDeleteByPos oldPos ol
                        put newOL
                        return [OrderedListUpdateItem (MkSequencePoint oldPos) (MkSequencePoint newPos) update]
    sUpdate (KeyUpdateDelete key) _mr = do
        ol <- get
        case lookUpByKey ol key of
            Just (_, pos, _) -> do
                put $ olDeleteByPos pos ol
                return [OrderedListUpdateDelete $ MkSequencePoint pos]
            Nothing -> return []
    sUpdate (KeyUpdateInsertReplace newitem) _mr = do
        ol <- get
        let
            imr :: MutableRead (StateT (OrderedList (o, ContainerKey cont, or)) m) (UpdateReader update)
            imr = subjectToMutableRead newitem
        key <- readKey @cont imr
        ordr <- runFloatInit ordInit imr
        o <- elGet (rOrdLens ordr) imr ReadWhole
        let (found, MkSequencePoint -> pos) = olLookupByItem ol (o, key, ordr)
        if found
            then return [OrderedListUpdateDelete pos, OrderedListUpdateInsert pos newitem]
            else case lookUpByKey ol key of
                     Just (_, oldpos, _) -> do
                         put $ snd $ olInsert (o, key, ordr) $ olDeleteByPos oldpos ol
                         return [OrderedListUpdateDelete $ MkSequencePoint oldpos, OrderedListUpdateInsert pos newitem]
                     Nothing -> do
                         put $ snd $ olInsert (o, key, ordr) ol
                         return [OrderedListUpdateInsert pos newitem]
    sUpdate KeyUpdateClear _ = do
        put $ olEmpty kcmp
        return [OrderedListUpdateClear]
    sPutEdit ::
           forall m. MonadIO m
        => OrderedListEdit seq (UpdateEdit update)
        -> MutableRead m (KeyReader cont (UpdateReader update))
        -> StateT (OrderedList (o, ContainerKey cont, or)) m (Maybe [KeyEdit cont (UpdateEdit update)])
    sPutEdit OrderedListEditClear _ = do
        put $ olEmpty kcmp
        return $ Just [KeyEditClear]
    sPutEdit (OrderedListEditDelete (MkSequencePoint pos)) _ = do
        ol <- get
        case olGetByPos ol pos of
            Nothing -> return $ Just []
            Just (_, key, _) -> do
                put $ olDeleteByPos pos ol
                return $ Just [KeyEditDelete key]
    sPutEdit (OrderedListEditItem (MkSequencePoint oldPos) edit) oldmr = do
        ol <- get
        case olGetByPos ol oldPos of
            Nothing -> return $ Just []
            Just (oldO, oldkey, ordr) -> do
                let update = editUpdate edit
                mnewkey <-
                    case updatesKey @cont update of
                        Just mapkey -> do
                            newkey <- liftIO $ mapkey oldkey
                            return $
                                if newkey == oldkey
                                    then Nothing
                                    else Just newkey
                        Nothing -> return Nothing
                case mnewkey of
                        -- key hasn't changed
                    Nothing -> do
                        ws <- lift $ elUpdate (rOrdLens ordr) update $ knownKeyItemReadFunction oldkey oldmr
                        case lastReadOnlyWholeUpdate ws of
                                    -- order hasn't changed
                            Nothing -> return $ Just [KeyEditItem oldkey edit] -- key & order unchanged
                            Just newO -> do
                                let (_, newOL) = olInsert (newO, oldkey, ordr) $ olDeleteByPos oldPos ol
                                put newOL
                                return $ Just [KeyEditItem oldkey edit]
                        -- key changed
                    Just newkey -> do
                        ws <- lift $ elUpdate (rOrdLens ordr) update $ knownKeyItemReadFunction newkey oldmr
                        let
                            newO =
                                case lastReadOnlyWholeUpdate ws of
                                    Just o -> o
                                    Nothing -> oldO
                        let (_, newOL) = olInsert (newO, newkey, ordr) $ olDeleteByPos oldPos ol
                        put newOL
                        return $ Just [KeyEditItem oldkey edit]
    sPutEdits ::
           forall m. MonadIO m
        => [OrderedListEdit seq (UpdateEdit update)]
        -> MutableRead m (KeyReader cont (UpdateReader update))
        -> StateT (OrderedList (o, ContainerKey cont, or)) m (Maybe [KeyEdit cont (UpdateEdit update)])
    sPutEdits = elPutEditsFromPutEdit sPutEdit
    in makeStateLens MkStateEditLens {..}

contextOrderedSetLens ::
       forall updateX updateN cont seq.
       ( Index seq ~ Int
       , HasKeyUpdate cont updateN
       , FullSubjectReader (UpdateReader updateN)
       , Item cont ~ UpdateSubject updateN
       --, ApplicableEdit (UpdateEdit updateX)
       , ApplicableEdit (UpdateEdit updateN)
       , IsUpdate updateN
       )
    => UpdateOrder (ContextUpdate updateX updateN)
    -> FloatingEditLens (ContextUpdate updateX (KeyUpdate cont updateN)) (ContextUpdate updateX (OrderedListUpdate seq updateN))
contextOrderedSetLens (MkUpdateOrder (cmp :: o -> o -> Ordering) (MkFloatingEditLens (ordInit :: FloatInit _ or) rOrdLens)) = let
    kcmp :: (o, ContainerKey cont, or) -> (o, ContainerKey cont, or) -> Ordering
    kcmp (o1, k1, _) (o2, k2, _) =
        case cmp o1 o2 of
            EQ ->
                if k1 == k2
                    then EQ
                    else LT
            c -> c
    getMaybeO ::
           forall m. MonadIO m
        => or
        -> MutableRead m (ContextUpdateReader updateX (KeyUpdate cont updateN))
        -> ContainerKey cont
        -> m (Maybe o)
    getMaybeO ordr mr k = let
        cmr :: MutableRead (ComposeM Maybe m) (TupleUpdateReader (WithContextSelector updateX updateN))
        cmr (MkTupleUpdateReader SelectContext rt) = MkComposeM $ fmap Just $ mr $ MkTupleUpdateReader SelectContext rt
        cmr (MkTupleUpdateReader SelectContent rt) =
            MkComposeM $ mr $ MkTupleUpdateReader SelectContent $ KeyReadItem k rt
        in getComposeM $ elGet (rOrdLens ordr) cmr ReadWhole
    getO ::
           forall m. MonadIO m
        => or
        -> MutableRead m (ContextUpdateReader updateX (KeyUpdate cont updateN))
        -> ContainerKey cont
        -> m o
    getO ordr mr k = do
        mo <- getMaybeO ordr mr k
        case mo of
            Just o -> return o
            Nothing -> liftIO $ fail "orderedSetLens: missing key"
    keyRF ::
           ContainerKey cont
        -> ReadFunction (ContextUpdateReader updateX (KeyUpdate cont updateN)) (ContextUpdateReader updateX updateN)
    keyRF _ mr (MkTupleUpdateReader SelectContext rt) = mr $ MkTupleUpdateReader SelectContext rt
    keyRF key mr (MkTupleUpdateReader SelectContent rt) =
        knownKeyItemReadFunction key (tupleReadFunction SelectContent mr) rt
    sInit ::
           forall m. MonadIO m
        => MutableRead m (ContextUpdateReader updateX (KeyUpdate cont updateN))
        -> m (OrderedList (o, ContainerKey cont, or))
    sInit mr = do
        MkFiniteSet kk <- mr $ MkTupleUpdateReader SelectContent KeyReadKeys
        pairs <-
            for kk $ \k -> do
                ordr <- runFloatInit ordInit $ keyRF k mr
                o <- getO ordr mr k
                return (o, k, ordr)
        return $ olFromList kcmp pairs
    sGet ::
           ReadFunctionT (StateT (OrderedList (o, ContainerKey cont, or))) (ContextUpdateReader updateX (KeyUpdate cont updateN)) (ContextUpdateReader updateX (OrderedListUpdate seq updateN))
    sGet mr (MkTupleUpdateReader SelectContext rt) = lift $ mr $ MkTupleUpdateReader SelectContext rt
    sGet _ (MkTupleUpdateReader SelectContent ListReadLength) = do
        ol <- get
        return $ MkSequencePoint $ olLength ol
    sGet mr (MkTupleUpdateReader SelectContent (ListReadItem (MkSequencePoint i) rt)) = do
        ol <- get
        case olGetByPos ol i of
            Just (_, key, _) -> lift $ mr $ MkTupleUpdateReader SelectContent $ KeyReadItem key rt
            Nothing -> return Nothing
    lookUpByKey :: OrderedList (o, ContainerKey cont, or) -> ContainerKey cont -> Maybe (o, Int, or)
    lookUpByKey ol key = do
        ((o, _, ordr), pos) <- olLookupByPredicate ol $ \(_, k, _) -> k == key
        return (o, pos, ordr)
    sUpdate ::
           forall m. MonadIO m
        => ContextUpdate updateX (KeyUpdate cont updateN)
        -> MutableRead m (ContextUpdateReader updateX (KeyUpdate cont updateN))
        -> StateT (OrderedList (o, ContainerKey cont, or)) m [ContextUpdate updateX (OrderedListUpdate seq updateN)]
    sUpdate (MkTupleUpdate SelectContext update) _ = do
        -- oldol <- get
        -- NYI: update order
        return [MkTupleUpdate SelectContext update]
    sUpdate (MkTupleUpdate SelectContent (KeyUpdateItem oldkey (update :: updateN))) newmr = do
        ol <- get
        case lookUpByKey ol oldkey of
            Nothing -> return [] -- key not found, no change
            Just (oldO, oldPos, ordr) -> do
                mnewkey <-
                    case updatesKey @cont update of
                        Just mapkey -> do
                            newkey <- liftIO $ mapkey oldkey
                            return $
                                if newkey == oldkey
                                    then Nothing
                                    else Just newkey
                        Nothing -> return Nothing
                case mnewkey of
                        -- key hasn't changed
                    Nothing -> do
                        ws <- lift $ elUpdate (rOrdLens ordr) (MkTupleUpdate SelectContent update) $ keyRF oldkey newmr
                        case lastReadOnlyWholeUpdate ws of
                                    -- order hasn't changed
                            Nothing ->
                                return
                                    [ MkTupleUpdate SelectContent $
                                      OrderedListUpdateItem (MkSequencePoint oldPos) (MkSequencePoint oldPos) update
                                    ] -- key & order unchanged
                            Just newO -> do
                                let (newPos, newOL) = olInsert (newO, oldkey, ordr) $ olDeleteByPos oldPos ol
                                put newOL
                                return
                                    [ MkTupleUpdate SelectContent $
                                      OrderedListUpdateItem (MkSequencePoint oldPos) (MkSequencePoint newPos) update
                                    ]
                        -- key changed
                    Just newkey -> do
                        ws <- lift $ elUpdate (rOrdLens ordr) (MkTupleUpdate SelectContent update) $ keyRF newkey newmr
                        let
                            newO =
                                case lastReadOnlyWholeUpdate ws of
                                    Just o -> o
                                    Nothing -> oldO
                        let (newPos, newOL) = olInsert (newO, newkey, ordr) $ olDeleteByPos oldPos ol
                        put newOL
                        return
                            [ MkTupleUpdate SelectContent $
                              OrderedListUpdateItem (MkSequencePoint oldPos) (MkSequencePoint newPos) update
                            ]
    sUpdate (MkTupleUpdate SelectContent (KeyUpdateDelete key)) _mr = do
        ol <- get
        case lookUpByKey ol key of
            Just (_, pos, _) -> do
                put $ olDeleteByPos pos ol
                return [MkTupleUpdate SelectContent $ OrderedListUpdateDelete $ MkSequencePoint pos]
            Nothing -> return []
    sUpdate (MkTupleUpdate SelectContent (KeyUpdateInsertReplace newitem)) mr = do
        ol <- get
        let
            imr :: forall .
                   MutableRead (StateT (OrderedList (o, ContainerKey cont, or)) m) (ContextUpdateReader updateX updateN)
            imr (MkTupleUpdateReader SelectContext rt) = lift $ mr $ MkTupleUpdateReader SelectContext rt
            imr (MkTupleUpdateReader SelectContent rt) = subjectToMutableRead newitem rt
        key <- readKey @cont imr
        ordr <- runFloatInit ordInit imr
        o <- elGet (rOrdLens ordr) imr ReadWhole
        let (found, MkSequencePoint -> pos) = olLookupByItem ol (o, key, ordr)
        if found
            then return
                     [ MkTupleUpdate SelectContent $ OrderedListUpdateDelete pos
                     , MkTupleUpdate SelectContent $ OrderedListUpdateInsert pos newitem
                     ]
            else case lookUpByKey ol key of
                     Just (_, oldpos, _) -> do
                         put $ snd $ olInsert (o, key, ordr) $ olDeleteByPos oldpos ol
                         return
                             [ MkTupleUpdate SelectContent $ OrderedListUpdateDelete $ MkSequencePoint oldpos
                             , MkTupleUpdate SelectContent $ OrderedListUpdateInsert pos newitem
                             ]
                     Nothing -> do
                         put $ snd $ olInsert (o, key, ordr) ol
                         return [MkTupleUpdate SelectContent $ OrderedListUpdateInsert pos newitem]
    sUpdate (MkTupleUpdate SelectContent KeyUpdateClear) _ = do
        put $ olEmpty kcmp
        return [MkTupleUpdate SelectContent $ OrderedListUpdateClear]
    sPutEdit ::
           forall m. MonadIO m
        => ContextUpdateEdit updateX (OrderedListUpdate seq updateN)
        -> MutableRead m (ContextUpdateReader updateX (KeyUpdate cont updateN))
        -> StateT (OrderedList (o, ContainerKey cont, or)) m (Maybe [ContextUpdateEdit updateX (KeyUpdate cont updateN)])
    sPutEdit (MkTupleUpdateEdit SelectContext edit) _ = return $ Just [MkTupleUpdateEdit SelectContext edit]
    sPutEdit (MkTupleUpdateEdit SelectContent OrderedListEditClear) _ = do
        put $ olEmpty kcmp
        return $ Just [MkTupleUpdateEdit SelectContent $ KeyEditClear]
    sPutEdit (MkTupleUpdateEdit SelectContent (OrderedListEditDelete (MkSequencePoint pos))) _ = do
        ol <- get
        case olGetByPos ol pos of
            Nothing -> return $ Just []
            Just (_, key, _) -> do
                put $ olDeleteByPos pos ol
                return $ Just [MkTupleUpdateEdit SelectContent $ KeyEditDelete key]
    sPutEdit (MkTupleUpdateEdit SelectContent (OrderedListEditItem (MkSequencePoint oldPos) edit)) oldmr = do
        ol <- get
        case olGetByPos ol oldPos of
            Nothing -> return $ Just []
            Just (oldO, oldkey, ordr) -> do
                let
                    update :: updateN
                    update = editUpdate edit
                mnewkey <-
                    case updatesKey @cont update of
                        Just mapkey -> do
                            newkey <- liftIO $ mapkey oldkey
                            return $
                                if newkey == oldkey
                                    then Nothing
                                    else Just newkey
                        Nothing -> return Nothing
                case mnewkey of
                        -- key hasn't changed
                    Nothing -> do
                        ws <- lift $ elUpdate (rOrdLens ordr) (MkTupleUpdate SelectContent update) $ keyRF oldkey oldmr
                        case lastReadOnlyWholeUpdate ws of
                                    -- order hasn't changed
                            Nothing -> return $ Just [MkTupleUpdateEdit SelectContent $ KeyEditItem oldkey edit] -- key & order unchanged
                            Just newO -> do
                                let (_, newOL) = olInsert (newO, oldkey, ordr) $ olDeleteByPos oldPos ol
                                put newOL
                                return $ Just [MkTupleUpdateEdit SelectContent $ KeyEditItem oldkey edit]
                        -- key changed
                    Just newkey -> do
                        ws <- lift $ elUpdate (rOrdLens ordr) (MkTupleUpdate SelectContent update) $ keyRF newkey oldmr
                        let
                            newO =
                                case lastReadOnlyWholeUpdate ws of
                                    Just o -> o
                                    Nothing -> oldO
                        let (_, newOL) = olInsert (newO, newkey, ordr) $ olDeleteByPos oldPos ol
                        put newOL
                        return $ Just [MkTupleUpdateEdit SelectContent $ KeyEditItem oldkey edit]
    contentOnlyApplyEdit ::
           ContextUpdateEdit updateX (KeyUpdate cont updateN)
        -> ReadFunction (ContextUpdateReader updateX (KeyUpdate cont updateN)) (ContextUpdateReader updateX (KeyUpdate cont updateN))
    contentOnlyApplyEdit (MkTupleUpdateEdit SelectContent edit) mr (MkTupleUpdateReader SelectContent rt) =
        applyEdit edit (mr . MkTupleUpdateReader SelectContent) rt
    contentOnlyApplyEdit _ mr rt = mr rt
    contentOnlyApplyEdits ::
           [ContextUpdateEdit updateX (KeyUpdate cont updateN)]
        -> ReadFunction (ContextUpdateReader updateX (KeyUpdate cont updateN)) (ContextUpdateReader updateX (KeyUpdate cont updateN))
    contentOnlyApplyEdits [] mr = mr
    contentOnlyApplyEdits (e:es) mr = contentOnlyApplyEdits es $ contentOnlyApplyEdit e mr
    sPutEdits ::
           forall m. MonadIO m
        => [ContextUpdateEdit updateX (OrderedListUpdate seq updateN)]
        -> MutableRead m (ContextUpdateReader updateX (KeyUpdate cont updateN))
        -> StateT (OrderedList (o, ContainerKey cont, or)) m (Maybe [ContextUpdateEdit updateX (KeyUpdate cont updateN)])
    sPutEdits [] _ = getComposeM $ return []
    sPutEdits (e:ee) mr =
        getComposeM $ do
            ea <- MkComposeM $ sPutEdit e mr
            eea <- MkComposeM $ sPutEdits ee $ contentOnlyApplyEdits ea mr
            return $ ea ++ eea
    in makeStateLens MkStateEditLens {..}

contextKeyEditLens ::
       forall cont1 cont2 ua ub.
       ( ContainerKey cont1 ~ ContainerKey cont2
       , Item cont1 ~ Item cont2
       , ApplicableEdit (UpdateEdit ua)
       , ApplicableEdit (UpdateEdit ub)
       , HasKeyReader cont1 (UpdateReader ub)
       , FullSubjectReader (UpdateReader ub)
       , Item cont1 ~ UpdateSubject ub
       )
    => EditLens (ContextUpdate ua (KeyUpdate cont1 ub)) (KeyUpdate cont2 (ContextUpdate ua ub))
contextKeyEditLens = let
    elGet :: ReadFunction (ContextUpdateReader ua (KeyUpdate cont1 ub)) (KeyReader cont2 (ContextUpdateReader ua ub))
    elGet mr KeyReadKeys = mr $ MkTupleUpdateReader SelectContent KeyReadKeys
    elGet mr (KeyReadItem _ (MkTupleUpdateReader SelectContext rt)) =
        fmap Just $ mr $ MkTupleUpdateReader SelectContext rt
    elGet mr (KeyReadItem key (MkTupleUpdateReader SelectContent rt)) =
        mr $ MkTupleUpdateReader SelectContent $ KeyReadItem key rt
    elUpdate ::
           forall m. MonadIO m
        => ContextUpdate ua (KeyUpdate cont1 ub)
        -> MutableRead m (ContextUpdateReader ua (KeyUpdate cont1 ub))
        -> m [KeyUpdate cont2 (ContextUpdate ua ub)]
    elUpdate (MkTupleUpdate SelectContext update) mr = do
        MkFiniteSet kk <- mr $ MkTupleUpdateReader SelectContent KeyReadKeys
        return $ fmap (\key -> KeyUpdateItem key $ MkTupleUpdate SelectContext update) kk
    elUpdate (MkTupleUpdate SelectContent (KeyUpdateItem key update)) _ =
        return [KeyUpdateItem key $ MkTupleUpdate SelectContent update]
    elUpdate (MkTupleUpdate SelectContent (KeyUpdateDelete key)) _ = return [KeyUpdateDelete key]
    elUpdate (MkTupleUpdate SelectContent (KeyUpdateInsertReplace e)) _ = return [KeyUpdateInsertReplace e]
    elUpdate (MkTupleUpdate SelectContent KeyUpdateClear) _ = return [KeyUpdateClear]
    elPutEdit ::
           forall m. MonadIO m
        => KeyEdit cont2 (ContextUpdateEdit ua ub)
        -> MutableRead m (ContextUpdateReader ua (KeyUpdate cont1 ub))
        -> m (Maybe [ContextUpdateEdit ua (KeyUpdate cont1 ub)])
    elPutEdit (KeyEditItem _ (MkTupleUpdateEdit SelectContext edit)) _ =
        return $ Just [MkTupleUpdateEdit SelectContext edit]
    elPutEdit (KeyEditItem key (MkTupleUpdateEdit SelectContent edit)) _ =
        return $ Just [MkTupleUpdateEdit SelectContent $ KeyEditItem key edit]
    elPutEdit (KeyEditDelete key) _ = return $ Just [MkTupleUpdateEdit SelectContent $ KeyEditDelete key]
    elPutEdit (KeyEditInsertReplace e) _ = return $ Just [MkTupleUpdateEdit SelectContent $ KeyEditInsertReplace e]
    elPutEdit KeyEditClear _ = return $ Just [MkTupleUpdateEdit SelectContent KeyEditClear]
    elPutEdits ::
           forall m. MonadIO m
        => [KeyEdit cont2 (ContextUpdateEdit ua ub)]
        -> MutableRead m (ContextUpdateReader ua (KeyUpdate cont1 ub))
        -> m (Maybe [ContextUpdateEdit ua (KeyUpdate cont1 ub)])
    elPutEdits = elPutEditsFromPutEdit elPutEdit
    in MkEditLens {..}
