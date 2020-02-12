module Truth.Core.Types.Key.Order
    ( UpdateOrder(..)
    , mapUpdateOrder
    , mapReadOnlyUpdateOrder
    , orderedSetLens
    , contextOrderedSetLens
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Lens
import Truth.Core.Read
import Truth.Core.Sequence
import Truth.Core.Types.Key.HasKey
import Truth.Core.Types.Key.Key
import Truth.Core.Types.List
import Truth.Core.Types.ReadOnly
import Truth.Core.Types.Tuple.Context
import Truth.Core.Types.Tuple.Tuple
import Truth.Core.Types.Whole

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
                                return
                                    [ OrderedListUpdateItem (MkSequencePoint oldPos) (MkSequencePoint oldPos) $
                                      Just update
                                    ] -- key & order unchanged
                            Just newO -> do
                                let (newPos, newOL) = olInsert (newO, oldkey, ordr) $ olDeleteByPos oldPos ol
                                put newOL
                                return
                                    [ OrderedListUpdateItem (MkSequencePoint oldPos) (MkSequencePoint newPos) $
                                      Just update
                                    ]
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
                        return [OrderedListUpdateItem (MkSequencePoint oldPos) (MkSequencePoint newPos) $ Just update]
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
    sUpdate (MkTupleUpdate SelectContext update) mr = do
        firstOL <- get
        moveUpdates <-
            for (toList firstOL) $ \(_, key, ordr) -> do
                oUpdates <- lift $ elUpdate (rOrdLens ordr) (MkTupleUpdate SelectContext update) $ keyRF key mr
                case lastReadOnlyWholeUpdate oUpdates of
                    Nothing -> return []
                    Just newO -> do
                        oldOL <- get
                        case lookUpByKey oldOL key -- slow
                              of
                            Nothing -> liftIO $ fail "key not found in order"
                            Just (_, oldPos, _) -> do
                                let
                                    midOL = olDeleteByPos oldPos oldOL
                                    (newPos, newOL) = olInsert (newO, key, ordr) midOL
                                put newOL
                                return $
                                    if oldPos == newPos
                                        then []
                                        else [ MkTupleUpdate SelectContent $
                                               OrderedListUpdateItem
                                                   (MkSequencePoint oldPos)
                                                   (MkSequencePoint newPos)
                                                   Nothing
                                             ]
        return $ mconcat moveUpdates <> [MkTupleUpdate SelectContext update]
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
                                      OrderedListUpdateItem (MkSequencePoint oldPos) (MkSequencePoint oldPos) $
                                      Just update
                                    ] -- key & order unchanged
                            Just newO -> do
                                let (newPos, newOL) = olInsert (newO, oldkey, ordr) $ olDeleteByPos oldPos ol
                                put newOL
                                return
                                    [ MkTupleUpdate SelectContent $
                                      OrderedListUpdateItem (MkSequencePoint oldPos) (MkSequencePoint newPos) $
                                      Just update
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
                              OrderedListUpdateItem (MkSequencePoint oldPos) (MkSequencePoint newPos) $ Just update
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
