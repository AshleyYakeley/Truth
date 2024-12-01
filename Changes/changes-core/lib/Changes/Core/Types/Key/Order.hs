module Changes.Core.Types.Key.Order
    ( orderedSetLens
    , contextOrderedSetLens
    ) where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Read
import Changes.Core.Types.Key.HasKey
import Changes.Core.Types.Key.Key
import Changes.Core.Types.List
import Changes.Core.Types.Tuple.Context
import Changes.Core.Types.Tuple.Tuple
import Changes.Core.Types.UpdateOrder
import Changes.Core.Types.Whole

orderedSetLens ::
       forall update cont.
       ( HasCallStack
       , HasKeyUpdate cont update
       , FullSubjectReader (UpdateReader update)
       , Item cont ~ UpdateSubject update
       , ApplicableEdit (UpdateEdit update)
       , IsUpdate update
       )
    => UpdateOrder update
    -> FloatingChangeLens (KeyUpdate cont update) (OrderedListUpdate update)
orderedSetLens (MkUpdateOrder (cmp :: o -> o -> Ordering) (MkFloatingChangeLens (ordInit :: FloatInit _ or) rOrdLens)) = let
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
        -> Readable m (KeyReader cont (UpdateReader update))
        -> ContainerKey cont
        -> m (Maybe o)
    getMaybeO ordr mr k =
        unComposeInner $ clRead (rOrdLens ordr) (\rt -> MkComposeInner $ mr $ KeyReadItem k rt) ReadWhole
    getO ::
           forall m. MonadIO m
        => or
        -> Readable m (KeyReader cont (UpdateReader update))
        -> ContainerKey cont
        -> m o
    getO ordr mr k = do
        mo <- getMaybeO ordr mr k
        case mo of
            Just o -> return o
            Nothing -> liftIO $ fail "orderedSetLens: missing key"
    sclInit ::
           forall m. MonadIO m
        => Readable m (KeyReader cont (UpdateReader update))
        -> m (OrderedList (o, ContainerKey cont, or))
    sclInit mr = do
        MkFiniteSet kk <- mr KeyReadKeys
        pairs <-
            for kk $ \k -> do
                ordr <- runFloatInit ordInit $ knownKeyItemReadFunction k mr
                o <- getO ordr mr k
                return (o, k, ordr)
        return $ olFromList kcmp pairs
    sclRead ::
           ReadFunctionT (StateT (OrderedList (o, ContainerKey cont, or))) (KeyReader cont (UpdateReader update)) (ListReader (UpdateReader update))
    sclRead _ ListReadLength = do
        ol <- get
        return $ fromIntegral $ olLength ol
    sclRead mr (ListReadItem i rt) = do
        ol <- get
        case olGetByPos ol (fromIntegral i) of
            Just (_, key, _) -> lift $ mr $ KeyReadItem key rt
            Nothing -> return Nothing
    lookUpByKey :: OrderedList (o, ContainerKey cont, or) -> ContainerKey cont -> Maybe (o, Int, or)
    lookUpByKey ol key = do
        ((o, _, ordr), pos) <- olLookupByPredicate ol $ \(_, k, _) -> k == key
        return (o, pos, ordr)
    sclUpdate ::
           forall m. (HasCallStack, MonadIO m)
        => KeyUpdate cont update
        -> Readable m (KeyReader cont (UpdateReader update))
        -> StateT (OrderedList (o, ContainerKey cont, or)) m [OrderedListUpdate update]
    sclUpdate (KeyUpdateItem oldkey update) newmr = do
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
                        ws <- lift $ clUpdate (rOrdLens ordr) update $ knownKeyItemReadFunction oldkey newmr
                        case lastReadOnlyWholeUpdate ws of
                                    -- order hasn't changed
                            Nothing ->
                                return [OrderedListUpdateItem (fromIntegral oldPos) (fromIntegral oldPos) $ pure update] -- key & order unchanged
                            Just newO -> do
                                let (newPos, newOL) = olInsert (newO, oldkey, ordr) $ olDeleteByPos oldPos ol
                                put newOL
                                return [OrderedListUpdateItem (fromIntegral oldPos) (fromIntegral newPos) $ pure update]
                        -- key changed
                    Just newkey -> do
                        ws <- lift $ clUpdate (rOrdLens ordr) update $ knownKeyItemReadFunction newkey newmr
                        let
                            newO =
                                case lastReadOnlyWholeUpdate ws of
                                    Just o -> o
                                    Nothing -> oldO
                        let (newPos, newOL) = olInsert (newO, newkey, ordr) $ olDeleteByPos oldPos ol
                        put newOL
                        return [OrderedListUpdateItem (fromIntegral oldPos) (fromIntegral newPos) $ pure update]
    sclUpdate (KeyUpdateDelete key) _mr = do
        ol <- get
        case lookUpByKey ol key of
            Just (_, pos, _) -> do
                put $ olDeleteByPos pos ol
                return [OrderedListUpdateDelete $ fromIntegral pos]
            Nothing -> return []
    sclUpdate (KeyUpdateInsertReplace newitem) _mr = do
        ol <- get
        let
            imr :: Readable (StateT (OrderedList (o, ContainerKey cont, or)) m) (UpdateReader update)
            imr = subjectToReadable newitem
        key <- readKey @cont imr
        ordr <- runFloatInit ordInit imr
        o <- clRead (rOrdLens ordr) imr ReadWhole
        let (found, fromIntegral -> pos) = olLookupByItem ol (o, key, ordr)
        if found
            then return [OrderedListUpdateDelete pos, OrderedListUpdateInsert pos newitem]
            else case lookUpByKey ol key of
                     Just (_, oldpos, _) -> do
                         put $ snd $ olInsert (o, key, ordr) $ olDeleteByPos oldpos ol
                         return [OrderedListUpdateDelete $ fromIntegral oldpos, OrderedListUpdateInsert pos newitem]
                     Nothing -> do
                         put $ snd $ olInsert (o, key, ordr) ol
                         return [OrderedListUpdateInsert pos newitem]
    sclUpdate KeyUpdateClear _ = do
        put $ olEmpty kcmp
        return [OrderedListUpdateClear]
    sPutEdit ::
           forall m. MonadIO m
        => OrderedListEdit (UpdateEdit update)
        -> Readable m (KeyReader cont (UpdateReader update))
        -> StateT (OrderedList (o, ContainerKey cont, or)) m (Maybe [KeyEdit cont (UpdateEdit update)])
    sPutEdit OrderedListEditClear _ = do
        put $ olEmpty kcmp
        return $ Just [KeyEditClear]
    sPutEdit (OrderedListEditDelete (fromIntegral -> pos)) _ = do
        ol <- get
        case olGetByPos ol pos of
            Nothing -> return $ Just []
            Just (_, key, _) -> do
                put $ olDeleteByPos pos ol
                return $ Just [KeyEditDelete key]
    sPutEdit (OrderedListEditItem (fromIntegral -> oldPos) edit) oldmr = do
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
                        ws <- lift $ clUpdate (rOrdLens ordr) update $ knownKeyItemReadFunction oldkey oldmr
                        case lastReadOnlyWholeUpdate ws of
                                    -- order hasn't changed
                            Nothing -> return $ Just [KeyEditItem oldkey edit] -- key & order unchanged
                            Just newO -> do
                                let (_, newOL) = olInsert (newO, oldkey, ordr) $ olDeleteByPos oldPos ol
                                put newOL
                                return $ Just [KeyEditItem oldkey edit]
                        -- key changed
                    Just newkey -> do
                        ws <- lift $ clUpdate (rOrdLens ordr) update $ knownKeyItemReadFunction newkey oldmr
                        let
                            newO =
                                case lastReadOnlyWholeUpdate ws of
                                    Just o -> o
                                    Nothing -> oldO
                        let (_, newOL) = olInsert (newO, newkey, ordr) $ olDeleteByPos oldPos ol
                        put newOL
                        return $ Just [KeyEditItem oldkey edit]
    sclPutEdits ::
           forall m. MonadIO m
        => [OrderedListEdit (UpdateEdit update)]
        -> Readable m (KeyReader cont (UpdateReader update))
        -> StateT (OrderedList (o, ContainerKey cont, or)) m (Maybe [KeyEdit cont (UpdateEdit update)])
    sclPutEdits = clPutEditsFromPutEdit sPutEdit
    in makeStateLens @'NonLinear MkStateChangeLens {..}

contextOrderedSetLens ::
       forall updateX updateN cont.
       ( HasCallStack
       , HasKeyUpdate cont updateN
       , FullSubjectReader (UpdateReader updateN)
       , Item cont ~ UpdateSubject updateN
       , ApplicableEdit (UpdateEdit updateN)
       , IsUpdate updateN
       )
    => UpdateOrder (ContextUpdate updateX updateN)
    -> FloatingChangeLens (ContextUpdate updateX (KeyUpdate cont updateN)) (ContextUpdate updateX (OrderedListUpdate updateN))
contextOrderedSetLens (MkUpdateOrder (cmp :: o -> o -> Ordering) (MkFloatingChangeLens (ordInit :: FloatInit _ or) rOrdLens)) = let
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
        -> Readable m (ContextUpdateReader updateX (KeyUpdate cont updateN))
        -> ContainerKey cont
        -> m (Maybe o)
    getMaybeO ordr mr k = let
        cmr :: Readable (ComposeInner Maybe m) (TupleUpdateReader (WithContextSelector updateX updateN))
        cmr (MkTupleUpdateReader SelectContext rt) =
            MkComposeInner $ fmap Just $ mr $ MkTupleUpdateReader SelectContext rt
        cmr (MkTupleUpdateReader SelectContent rt) =
            MkComposeInner $ mr $ MkTupleUpdateReader SelectContent $ KeyReadItem k rt
        in unComposeInner $ clRead (rOrdLens ordr) cmr ReadWhole
    getO ::
           forall m. MonadIO m
        => or
        -> Readable m (ContextUpdateReader updateX (KeyUpdate cont updateN))
        -> ContainerKey cont
        -> m o
    getO ordr mr k = do
        mo <- getMaybeO ordr mr k
        case mo of
            Just o -> return o
            Nothing -> liftIO $ fail "orderedSetLens: missing key"
    keyRF ::
           HasCallStack
        => ContainerKey cont
        -> ReadFunction (ContextUpdateReader updateX (KeyUpdate cont updateN)) (ContextUpdateReader updateX updateN)
    keyRF _ mr (MkTupleUpdateReader SelectContext rt) = mr $ MkTupleUpdateReader SelectContext rt
    keyRF key mr (MkTupleUpdateReader SelectContent rt) =
        knownKeyItemReadFunction key (tupleReadFunction SelectContent mr) rt
    sclInit ::
           forall m. (HasCallStack, MonadIO m)
        => Readable m (ContextUpdateReader updateX (KeyUpdate cont updateN))
        -> m (OrderedList (o, ContainerKey cont, or))
    sclInit mr = do
        MkFiniteSet kk <- mr $ MkTupleUpdateReader SelectContent KeyReadKeys
        pairs <-
            for kk $ \k -> do
                ordr <- runFloatInit ordInit $ keyRF k mr
                o <- getO ordr mr k
                return (o, k, ordr)
        return $ olFromList kcmp pairs
    sclRead ::
           ReadFunctionT (StateT (OrderedList (o, ContainerKey cont, or))) (ContextUpdateReader updateX (KeyUpdate cont updateN)) (ContextUpdateReader updateX (OrderedListUpdate updateN))
    sclRead mr (MkTupleUpdateReader SelectContext rt) = lift $ mr $ MkTupleUpdateReader SelectContext rt
    sclRead _ (MkTupleUpdateReader SelectContent ListReadLength) = do
        ol <- get
        return $ fromIntegral $ olLength ol
    sclRead mr (MkTupleUpdateReader SelectContent (ListReadItem (fromIntegral -> i) rt)) = do
        ol <- get
        case olGetByPos ol i of
            Just (_, key, _) -> lift $ mr $ MkTupleUpdateReader SelectContent $ KeyReadItem key rt
            Nothing -> return Nothing
    lookUpByKey :: OrderedList (o, ContainerKey cont, or) -> ContainerKey cont -> Maybe (o, Int, or)
    lookUpByKey ol key = do
        ((o, _, ordr), pos) <- olLookupByPredicate ol $ \(_, k, _) -> k == key
        return (o, pos, ordr)
    sclUpdate ::
           forall m. (HasCallStack, MonadIO m)
        => ContextUpdate updateX (KeyUpdate cont updateN)
        -> Readable m (ContextUpdateReader updateX (KeyUpdate cont updateN))
        -> StateT (OrderedList (o, ContainerKey cont, or)) m [ContextUpdate updateX (OrderedListUpdate updateN)]
    sclUpdate (MkTupleUpdate SelectContext update) mr = do
        firstOL <- get
        moveUpdates <-
            for (toList firstOL) $ \(_, key, ordr) -> do
                oUpdates <- lift $ clUpdate (rOrdLens ordr) (MkTupleUpdate SelectContext update) $ keyRF key mr
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
                                               OrderedListUpdateItem (fromIntegral oldPos) (fromIntegral newPos) []
                                             ]
        return $ mconcat moveUpdates <> [MkTupleUpdate SelectContext update]
    sclUpdate (MkTupleUpdate SelectContent (KeyUpdateItem oldkey (update :: updateN))) newmr = do
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
                        ws <- lift $ clUpdate (rOrdLens ordr) (MkTupleUpdate SelectContent update) $ keyRF oldkey newmr
                        case lastReadOnlyWholeUpdate ws of
                                    -- order hasn't changed
                            Nothing ->
                                return
                                    [ MkTupleUpdate SelectContent $
                                      OrderedListUpdateItem (fromIntegral oldPos) (fromIntegral oldPos) $ pure update
                                    ] -- key & order unchanged
                            Just newO -> do
                                let (newPos, newOL) = olInsert (newO, oldkey, ordr) $ olDeleteByPos oldPos ol
                                put newOL
                                return
                                    [ MkTupleUpdate SelectContent $
                                      OrderedListUpdateItem (fromIntegral oldPos) (fromIntegral newPos) $ pure update
                                    ]
                        -- key changed
                    Just newkey -> do
                        ws <- lift $ clUpdate (rOrdLens ordr) (MkTupleUpdate SelectContent update) $ keyRF newkey newmr
                        let
                            newO =
                                case lastReadOnlyWholeUpdate ws of
                                    Just o -> o
                                    Nothing -> oldO
                        let (newPos, newOL) = olInsert (newO, newkey, ordr) $ olDeleteByPos oldPos ol
                        put newOL
                        return
                            [ MkTupleUpdate SelectContent $
                              OrderedListUpdateItem (fromIntegral oldPos) (fromIntegral newPos) $ pure update
                            ]
    sclUpdate (MkTupleUpdate SelectContent (KeyUpdateDelete key)) _mr = do
        ol <- get
        case lookUpByKey ol key of
            Just (_, pos, _) -> do
                put $ olDeleteByPos pos ol
                return [MkTupleUpdate SelectContent $ OrderedListUpdateDelete $ fromIntegral pos]
            Nothing -> return []
    sclUpdate (MkTupleUpdate SelectContent (KeyUpdateInsertReplace newitem)) mr = do
        ol <- get
        let
            imr :: forall .
                   Readable (StateT (OrderedList (o, ContainerKey cont, or)) m) (ContextUpdateReader updateX updateN)
            imr (MkTupleUpdateReader SelectContext rt) = lift $ mr $ MkTupleUpdateReader SelectContext rt
            imr (MkTupleUpdateReader SelectContent rt) = subjectToReadable newitem rt
        key <- readKey @cont imr
        ordr <- runFloatInit ordInit imr
        o <- clRead (rOrdLens ordr) imr ReadWhole
        let (found, fromIntegral -> pos) = olLookupByItem ol (o, key, ordr)
        if found
            then return
                     [ MkTupleUpdate SelectContent $ OrderedListUpdateDelete pos
                     , MkTupleUpdate SelectContent $ OrderedListUpdateInsert pos newitem
                     ]
            else case lookUpByKey ol key of
                     Just (_, oldpos, _) -> do
                         put $ snd $ olInsert (o, key, ordr) $ olDeleteByPos oldpos ol
                         return
                             [ MkTupleUpdate SelectContent $ OrderedListUpdateDelete $ fromIntegral oldpos
                             , MkTupleUpdate SelectContent $ OrderedListUpdateInsert pos newitem
                             ]
                     Nothing -> do
                         put $ snd $ olInsert (o, key, ordr) ol
                         return [MkTupleUpdate SelectContent $ OrderedListUpdateInsert pos newitem]
    sclUpdate (MkTupleUpdate SelectContent KeyUpdateClear) _ = do
        put $ olEmpty kcmp
        return [MkTupleUpdate SelectContent $ OrderedListUpdateClear]
    sPutEdit ::
           forall m. (HasCallStack, MonadIO m)
        => ContextUpdateEdit updateX (OrderedListUpdate updateN)
        -> Readable m (ContextUpdateReader updateX (KeyUpdate cont updateN))
        -> StateT (OrderedList (o, ContainerKey cont, or)) m (Maybe [ContextUpdateEdit updateX (KeyUpdate cont updateN)])
    sPutEdit (MkTupleUpdateEdit SelectContext edit) _ = return $ Just [MkTupleUpdateEdit SelectContext edit]
    sPutEdit (MkTupleUpdateEdit SelectContent OrderedListEditClear) _ = do
        put $ olEmpty kcmp
        return $ Just [MkTupleUpdateEdit SelectContent $ KeyEditClear]
    sPutEdit (MkTupleUpdateEdit SelectContent (OrderedListEditDelete (fromIntegral -> pos))) _ = do
        ol <- get
        case olGetByPos ol pos of
            Nothing -> return $ Just []
            Just (_, key, _) -> do
                put $ olDeleteByPos pos ol
                return $ Just [MkTupleUpdateEdit SelectContent $ KeyEditDelete key]
    sPutEdit (MkTupleUpdateEdit SelectContent (OrderedListEditItem (fromIntegral -> oldPos) edit)) oldmr = do
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
                        ws <- lift $ clUpdate (rOrdLens ordr) (MkTupleUpdate SelectContent update) $ keyRF oldkey oldmr
                        case lastReadOnlyWholeUpdate ws of
                                    -- order hasn't changed
                            Nothing -> return $ Just [MkTupleUpdateEdit SelectContent $ KeyEditItem oldkey edit] -- key & order unchanged
                            Just newO -> do
                                let (_, newOL) = olInsert (newO, oldkey, ordr) $ olDeleteByPos oldPos ol
                                put newOL
                                return $ Just [MkTupleUpdateEdit SelectContent $ KeyEditItem oldkey edit]
                        -- key changed
                    Just newkey -> do
                        ws <- lift $ clUpdate (rOrdLens ordr) (MkTupleUpdate SelectContent update) $ keyRF newkey oldmr
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
    sclPutEdits ::
           forall m. (HasCallStack, MonadIO m)
        => [ContextUpdateEdit updateX (OrderedListUpdate updateN)]
        -> Readable m (ContextUpdateReader updateX (KeyUpdate cont updateN))
        -> StateT (OrderedList (o, ContainerKey cont, or)) m (Maybe [ContextUpdateEdit updateX (KeyUpdate cont updateN)])
    sclPutEdits [] _ = unComposeInner $ return []
    sclPutEdits (e:ee) mr =
        unComposeInner $ do
            ea <- MkComposeInner $ sPutEdit e mr
            eea <- MkComposeInner $ sclPutEdits ee $ contentOnlyApplyEdits ea mr
            return $ ea ++ eea
    in makeStateLens @'NonLinear MkStateChangeLens {..}
