module Changes.Core.Types.Key.Key
    ( KeyReader(..)
    , KeyEdit(..)
    , KeyUpdate(..)
    , knownKeyItemReadFunction
    , keyElementChangeLens
    , fixedKeyElementChangeLens
    , liftKeyElementChangeLens
    , liftKeyElementFloatingChangeLens
    , contextKeyChangeLens
    ) where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Read
import Changes.Core.Types.Key.HasKey
import Changes.Core.Types.One.FullResult
import Changes.Core.Types.One.Read
import Changes.Core.Types.One.Result
import Changes.Core.Types.Tuple.Context
import Changes.Core.Types.Tuple.Tuple

--- reader
data KeyReader cont reader t where
    KeyReadKeys :: KeyReader cont reader (FiniteSet (ContainerKey cont))
    KeyReadItem :: ContainerKey cont -> reader t -> KeyReader cont reader (Maybe t)

instance (Show (ContainerKey cont), AllConstraint Show reader) => Show (KeyReader cont reader t) where
    show KeyReadKeys = "keys"
    show (KeyReadItem key rt) = "item " ++ show key ++ " " ++ allShow rt

instance (Show (ContainerKey cont), AllConstraint Show reader) => AllConstraint Show (KeyReader cont reader) where
    allConstraint = Dict

instance (Show (ContainerKey cont), WitnessConstraint Show reader) => WitnessConstraint Show (KeyReader cont reader) where
    witnessConstraint KeyReadKeys = Dict
    witnessConstraint (KeyReadItem _ rt) =
        case witnessConstraint @_ @Show rt of
            Dict -> Dict

keyItemReadFunction :: forall cont reader. ContainerKey cont -> ReadFunctionF Maybe (KeyReader cont reader) reader
keyItemReadFunction key mr rt = MkComposeInner $ mr $ KeyReadItem key rt

knownKeyItemReadFunction ::
       forall cont reader. HasCallStack
    => ContainerKey cont
    -> ReadFunction (KeyReader cont reader) reader
knownKeyItemReadFunction key mr rt = do
    mt <- unComposeInner $ keyItemReadFunction key mr rt
    case mt of
        Just t -> return t
        Nothing -> error $ "missing item in list"

instance (KeyContainer cont, SubjectReader reader, ReaderSubject reader ~ Item cont) =>
             SubjectReader (KeyReader cont reader) where
    type ReaderSubject (KeyReader cont reader) = cont
    subjectToRead cont KeyReadKeys = MkFiniteSet $ keys cont
    subjectToRead cont (KeyReadItem key rd) = fmap (\e -> subjectToRead e rd) $ lookupItem key cont

instance (KeyContainer cont, FullSubjectReader reader, ReaderSubject reader ~ Item cont) =>
             FullSubjectReader (KeyReader cont reader) where
    readableToSubject mr = do
        MkFiniteSet allkeys <- mr KeyReadKeys
        list <- for allkeys $ \key -> readableToSubject $ knownKeyItemReadFunction key mr
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

type instance EditReader (KeyEdit cont edit) = KeyReader cont (EditReader edit)

instance ( FullSubjectReader (EditReader edit)
         , ApplicableEdit edit
         , HasKeyReader cont (EditReader edit)
         , Item cont ~ EditSubject edit
         ) => ApplicableEdit (KeyEdit cont edit) where
    applyEdit (KeyEditItem oldkey edit) mr kreader@(KeyReadItem key rt) = do
        mnewkey <- unComposeInner $ readKey @cont $ applyEdit edit $ keyItemReadFunction oldkey mr -- the edit may change the element's key
        case mnewkey of
            Just newkey
                | key == newkey -> unComposeInner $ (applyEdit edit $ keyItemReadFunction key mr) rt
            _ ->
                if key == oldkey
                    then return Nothing
                    else mr kreader
    applyEdit (KeyEditItem oldkey edit) mr KeyReadKeys = do
        oldkeys <- mr KeyReadKeys
        mnewkey <-
            unComposeInner $ readKey @cont $ applyEdit edit $ keyItemReadFunction oldkey mr -- the edit may change the element's key
        return $
            case mnewkey of
                Just newkey -> replace oldkey newkey oldkeys
                _ -> oldkeys
    applyEdit (KeyEditDelete key) mr KeyReadKeys = do
        allkeys <- mr KeyReadKeys
        return $ deleteSet key allkeys
    applyEdit (KeyEditDelete key') _mr (KeyReadItem key _reader)
        | key' == key = return Nothing
    applyEdit (KeyEditDelete _) mr (KeyReadItem key rd) = mr $ KeyReadItem key rd
    applyEdit (KeyEditInsertReplace item) mr KeyReadKeys = do
        allkeys <- mr KeyReadKeys
        let newkey = itemKey @cont item
        if elem newkey allkeys
            then return allkeys
            else return $ insertSet newkey allkeys
    applyEdit (KeyEditInsertReplace item) _mr (KeyReadItem key rd)
        | itemKey @cont item == key = return $ Just $ subjectToRead item rd
    applyEdit (KeyEditInsertReplace _) mr (KeyReadItem key rd) = mr $ KeyReadItem key rd
    applyEdit KeyEditClear _mr rd = mSubjectToReadable (return mempty) rd

instance ( FullSubjectReader (EditReader edit)
         , ApplicableEdit edit
         , SubjectMapEdit edit
         , InvertibleEdit edit
         , HasKeyReader cont (EditReader edit)
         , Item cont ~ EditSubject edit
         ) => InvertibleEdit (KeyEdit cont edit) where
    invertEdit (KeyEditItem key edit) mr = do
        minvedits <- unComposeInner $ invertEdit edit $ keyItemReadFunction key mr
        case minvedits of
            Just invedits -> return $ fmap (KeyEditItem key) invedits
            Nothing -> return []
    invertEdit (KeyEditInsertReplace item) mr = do
        let newkey = itemKey @cont item
        molditem <- unComposeInner $ readableToSubject $ keyItemReadFunction newkey mr
        case molditem of
            Just olditem -> return [KeyEditInsertReplace olditem]
            Nothing -> return [KeyEditDelete newkey]
    invertEdit (KeyEditDelete key) mr = do
        ma <- unComposeInner $ readableToSubject $ keyItemReadFunction key mr
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
            item <- readableToSubject $ knownKeyItemReadFunction key mr
            write $ KeyEditInsertReplace item

--- update
data KeyUpdate cont update where
    KeyUpdateItem :: ContainerKey cont -> update -> KeyUpdate cont update
    KeyUpdateDelete :: ContainerKey cont -> KeyUpdate cont update
    KeyUpdateInsertReplace :: Item cont -> KeyUpdate cont update
    KeyUpdateClear :: KeyUpdate cont update

type instance UpdateEdit (KeyUpdate cont update) = KeyEdit cont (UpdateEdit update)

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

instance (FullSubjectReader (UpdateReader update), Item cont ~ UpdateSubject update) =>
             FullUpdate (KeyUpdate cont update) where
    replaceUpdate mr write = do
        write KeyUpdateClear
        allkeys <- mr KeyReadKeys
        for_ allkeys $ \key -> do
            item <- readableToSubject $ knownKeyItemReadFunction key mr
            write $ KeyUpdateInsertReplace item

keyElementChangeLens ::
       forall cont update.
       ( HasKeyUpdate cont update
       , ApplicableEdit (UpdateEdit update)
       , FullSubjectReader (UpdateReader update)
       , Item cont ~ UpdateSubject update
       )
    => ContainerKey cont
    -> FloatingChangeLens (KeyUpdate cont update) (MaybeUpdate update)
keyElementChangeLens initKey = let
    sclInit ::
           forall m. MonadIO m
        => Readable m (KeyReader cont (UpdateReader update))
        -> m (ContainerKey cont)
    sclInit _ = return initKey
    sclRead ::
           ReadFunctionT (StateT (ContainerKey cont)) (KeyReader cont (UpdateReader update)) (OneReader Maybe (UpdateReader update))
    sclRead mr ReadHasOne = do
        kk <- lift $ mr KeyReadKeys
        key <- get
        return $
            if elem key kk
                then Just ()
                else Nothing
    sclRead mr (ReadOne rt) = do
        key <- get
        lift $ mr $ KeyReadItem key rt
    sclUpdate ::
           forall m. MonadIO m
        => KeyUpdate cont update
        -> Readable m (KeyReader cont (UpdateReader update))
        -> StateT (ContainerKey cont) m [MaybeUpdate update]
    sclUpdate KeyUpdateClear _ = return [MkFullResultOneUpdate $ NewResultOneUpdate Nothing]
    sclUpdate (KeyUpdateDelete k) _ = do
        key <- get
        return $
            if k == key
                then [MkFullResultOneUpdate $ NewResultOneUpdate Nothing]
                else []
    sclUpdate (KeyUpdateItem k update) _ = do
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
    sclUpdate (KeyUpdateInsertReplace item) _ = do
        key <- get
        return $
            if itemKey @cont item == key
                then [MkFullResultOneUpdate $ NewResultOneUpdate $ Just ()]
                else []
    sPutEdit ::
           forall m. MonadIO m
        => MaybeEdit (UpdateEdit update)
        -> Readable m (KeyReader cont (UpdateReader update))
        -> StateT (ContainerKey cont) m (Maybe [KeyEdit cont (UpdateEdit update)])
    sPutEdit (NewFullResultOneEdit (Just subj)) _ = return $ Just [KeyEditInsertReplace subj]
    sPutEdit (NewFullResultOneEdit Nothing) _ = do
        key <- get
        return $ Just [KeyEditDelete key]
    sPutEdit (SuccessFullResultOneEdit edit) mr = do
        oldkey <- get
        mnewkey <- lift $ unComposeInner $ readKey @cont $ applyEdit edit $ keyItemReadFunction oldkey mr
        case mnewkey of
            Just newkey -> do
                put newkey
                return $ Just [KeyEditItem oldkey edit]
            Nothing -> return $ Just []
    sclPutEdits ::
           forall m. MonadIO m
        => [MaybeEdit (UpdateEdit update)]
        -> Readable m (KeyReader cont (UpdateReader update))
        -> StateT (ContainerKey cont) m (Maybe [KeyEdit cont (UpdateEdit update)])
    sclPutEdits = clPutEditsFromPutEdit sPutEdit
    in makeStateLens @'NonLinear MkStateChangeLens {..}

fixedKeyElementChangeLens ::
       forall cont update.
       ( HasKeyUpdate cont update
       , ApplicableEdit (UpdateEdit update)
       , FullSubjectReader (UpdateReader update)
       , Item cont ~ UpdateSubject update
       )
    => ContainerKey cont
    -> ChangeLens (KeyUpdate cont update) (MaybeUpdate update)
fixedKeyElementChangeLens key = floatingToDiscardingChangeLens $ keyElementChangeLens key

type InternalKeyMap key r = [(key, r)]

liftKeyElementChangeLens ::
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
    -> ChangeLens updateA updateB
    -> ChangeLens (KeyUpdate conta updateA) (KeyUpdate contb updateB)
liftKeyElementChangeLens bma (MkChangeLens g u pe) = let
    clRead :: ReadFunction (KeyReader conta (UpdateReader updateA)) (KeyReader contb (UpdateReader updateB))
    clRead mr KeyReadKeys = mr KeyReadKeys
    clRead (mr :: Readable m _) (KeyReadItem key rt) = unComposeInner $ g (keyItemReadFunction key mr) rt
    clUpdate ::
           forall m. MonadIO m
        => KeyUpdate conta updateA
        -> Readable m (KeyReader conta (UpdateReader updateA))
        -> m [KeyUpdate contb updateB]
    clUpdate KeyUpdateClear _ = return [KeyUpdateClear]
    clUpdate (KeyUpdateInsertReplace itema) _ = do
        itemb <- readableToSubject $ g $ subjectToReadable @m itema
        return [KeyUpdateInsertReplace itemb]
    clUpdate (KeyUpdateDelete key) _ = return [KeyUpdateDelete key]
    clUpdate (KeyUpdateItem key ea) mr = do
        mresult <- unComposeInner $ u ea (keyItemReadFunction @conta key mr)
        case mresult of
            Just ebs -> return $ fmap (KeyUpdateItem key) ebs
            Nothing -> return []
    clPutEdit ::
           forall m. MonadIO m
        => KeyEdit contb (UpdateEdit updateB)
        -> Readable m (KeyReader conta (UpdateReader updateA))
        -> m (Maybe [KeyEdit conta (UpdateEdit updateA)])
    clPutEdit KeyEditClear _ = return $ Just [KeyEditClear]
    clPutEdit (KeyEditInsertReplace itemb) _ = do
        fitema <- bma itemb
        return $ fmap (\itema -> [KeyEditInsertReplace itema]) fitema
    clPutEdit (KeyEditDelete key) _ = return $ Just [KeyEditDelete key]
    clPutEdit (KeyEditItem key eb) mr = do
        mfresult <- unComposeInner $ pe [eb] (keyItemReadFunction @conta key mr)
        return $
            case mfresult of
                Just fsea -> fmap (fmap $ KeyEditItem key) fsea
                Nothing -> Just []
    clPutEdits ::
           forall m. MonadIO m
        => [KeyEdit contb (UpdateEdit updateB)]
        -> Readable m (KeyReader conta (UpdateReader updateA))
        -> m (Maybe [KeyEdit conta (UpdateEdit updateA)])
    clPutEdits = clPutEditsFromPutEdit clPutEdit
    in MkChangeLens {..}

liftKeyElementFloatingChangeLens ::
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
    -> FloatingChangeLens updateA updateB
    -> FloatingChangeLens (KeyUpdate conta updateA) (KeyUpdate contb updateB)
liftKeyElementFloatingChangeLens bma (MkFloatingChangeLens (NoFloatInit r) rlens) =
    changeLensToFloating $ liftKeyElementChangeLens bma $ rlens r
liftKeyElementFloatingChangeLens bma (MkFloatingChangeLens (ReadFloatInit finit :: FloatInit _ r) rlens) = let
    sclInit :: StateLensInit (KeyReader conta (UpdateReader updateA)) (InternalKeyMap (ContainerKey conta) r)
    sclInit _ = return mempty
    getR ::
           forall m. MonadIO m
        => Readable m (KeyReader conta (UpdateReader updateA))
        -> ContainerKey conta
        -> StateT (InternalKeyMap (ContainerKey conta) r) m (Maybe r)
    getR mr key = do
        rmap <- get
        case lookup key rmap of
            Just r -> return $ Just r
            Nothing -> do
                mnewr <- lift $ unComposeInner $ finit $ \rt -> MkComposeInner $ mr $ KeyReadItem key rt
                case mnewr of
                    Just newr -> put $ insertMap key newr rmap
                    Nothing -> return ()
                return mnewr
    sclRead ::
           ReadFunctionT (StateT (InternalKeyMap (ContainerKey conta) r)) (KeyReader conta (UpdateReader updateA)) (KeyReader contb (UpdateReader updateB))
    sclRead mr KeyReadKeys = lift $ mr KeyReadKeys
    sclRead (mr :: Readable m _) (KeyReadItem key rt) = do
        mrc <- getR mr key
        lift $
            unComposeInner $ do
                r <- MkComposeInner $ return mrc
                clRead (rlens r) (keyItemReadFunction key mr) rt
    sclUpdate ::
           forall m. MonadIO m
        => KeyUpdate conta updateA
        -> Readable m (KeyReader conta (UpdateReader updateA))
        -> StateT (InternalKeyMap (ContainerKey conta) r) m [KeyUpdate contb updateB]
    sclUpdate KeyUpdateClear _ = return [KeyUpdateClear]
    sclUpdate (KeyUpdateInsertReplace itema) _ = do
        let
            imr :: Readable (StateT (InternalKeyMap (ContainerKey conta) r) m) (UpdateReader updateA)
            imr = subjectToReadable itema
        r <- finit imr
        itemb <- lift $ readableToSubject $ clRead (rlens r) $ subjectToReadable @m itema
        rmap <- get
        key <- readKey @conta imr
        put $ insertMap key r rmap
        return [KeyUpdateInsertReplace itemb]
    sclUpdate (KeyUpdateDelete key) _ = return [KeyUpdateDelete key]
    sclUpdate (KeyUpdateItem key ea) mr = do
        mrc <- getR mr key
        mresult <-
            lift $
            unComposeInner $ do
                r <- MkComposeInner $ return mrc
                clUpdate (rlens r) ea (keyItemReadFunction @conta key mr)
        case mresult of
            Just ebs -> return $ fmap (KeyUpdateItem key) ebs
            Nothing -> return []
    sPutEdit ::
           forall m. MonadIO m
        => KeyEdit contb (UpdateEdit updateB)
        -> Readable m (KeyReader conta (UpdateReader updateA))
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
            unComposeInner $ do
                r <- MkComposeInner $ return mrc
                clPutEdits (rlens r) [eb] (keyItemReadFunction @conta key mr)
        return $
            case mfresult of
                Just fsea -> fmap (fmap $ KeyEditItem key) fsea
                Nothing -> Just []
    sclPutEdits ::
           forall m. MonadIO m
        => [KeyEdit contb (UpdateEdit updateB)]
        -> Readable m (KeyReader conta (UpdateReader updateA))
        -> StateT (InternalKeyMap (ContainerKey conta) r) m (Maybe [KeyEdit conta (UpdateEdit updateA)])
    sclPutEdits = clPutEditsFromPutEdit sPutEdit
    in makeStateLens @'NonLinear MkStateChangeLens {..}

contextKeyChangeLens ::
       forall cont1 cont2 ua ub.
       ( ContainerKey cont1 ~ ContainerKey cont2
       , Item cont1 ~ Item cont2
       , ApplicableEdit (UpdateEdit ua)
       , ApplicableEdit (UpdateEdit ub)
       , HasKeyReader cont1 (UpdateReader ub)
       , FullSubjectReader (UpdateReader ub)
       , Item cont1 ~ UpdateSubject ub
       )
    => ChangeLens (ContextUpdate ua (KeyUpdate cont1 ub)) (KeyUpdate cont2 (ContextUpdate ua ub))
contextKeyChangeLens = let
    clRead :: ReadFunction (ContextUpdateReader ua (KeyUpdate cont1 ub)) (KeyReader cont2 (ContextUpdateReader ua ub))
    clRead mr KeyReadKeys = mr $ MkTupleUpdateReader SelectContent KeyReadKeys
    clRead mr (KeyReadItem _ (MkTupleUpdateReader SelectContext rt)) =
        fmap Just $ mr $ MkTupleUpdateReader SelectContext rt
    clRead mr (KeyReadItem key (MkTupleUpdateReader SelectContent rt)) =
        mr $ MkTupleUpdateReader SelectContent $ KeyReadItem key rt
    clUpdate ::
           forall m. MonadIO m
        => ContextUpdate ua (KeyUpdate cont1 ub)
        -> Readable m (ContextUpdateReader ua (KeyUpdate cont1 ub))
        -> m [KeyUpdate cont2 (ContextUpdate ua ub)]
    clUpdate (MkTupleUpdate SelectContext update) mr = do
        MkFiniteSet kk <- mr $ MkTupleUpdateReader SelectContent KeyReadKeys
        return $ fmap (\key -> KeyUpdateItem key $ MkTupleUpdate SelectContext update) kk
    clUpdate (MkTupleUpdate SelectContent (KeyUpdateItem key update)) _ =
        return [KeyUpdateItem key $ MkTupleUpdate SelectContent update]
    clUpdate (MkTupleUpdate SelectContent (KeyUpdateDelete key)) _ = return [KeyUpdateDelete key]
    clUpdate (MkTupleUpdate SelectContent (KeyUpdateInsertReplace e)) _ = return [KeyUpdateInsertReplace e]
    clUpdate (MkTupleUpdate SelectContent KeyUpdateClear) _ = return [KeyUpdateClear]
    clPutEdit ::
           forall m. MonadIO m
        => KeyEdit cont2 (ContextUpdateEdit ua ub)
        -> Readable m (ContextUpdateReader ua (KeyUpdate cont1 ub))
        -> m (Maybe [ContextUpdateEdit ua (KeyUpdate cont1 ub)])
    clPutEdit (KeyEditItem _ (MkTupleUpdateEdit SelectContext edit)) _ =
        return $ Just [MkTupleUpdateEdit SelectContext edit]
    clPutEdit (KeyEditItem key (MkTupleUpdateEdit SelectContent edit)) _ =
        return $ Just [MkTupleUpdateEdit SelectContent $ KeyEditItem key edit]
    clPutEdit (KeyEditDelete key) _ = return $ Just [MkTupleUpdateEdit SelectContent $ KeyEditDelete key]
    clPutEdit (KeyEditInsertReplace e) _ = return $ Just [MkTupleUpdateEdit SelectContent $ KeyEditInsertReplace e]
    clPutEdit KeyEditClear _ = return $ Just [MkTupleUpdateEdit SelectContent KeyEditClear]
    clPutEdits ::
           forall m. MonadIO m
        => [KeyEdit cont2 (ContextUpdateEdit ua ub)]
        -> Readable m (ContextUpdateReader ua (KeyUpdate cont1 ub))
        -> m (Maybe [ContextUpdateEdit ua (KeyUpdate cont1 ub)])
    clPutEdits = clPutEditsFromPutEdit clPutEdit
    in MkChangeLens {..}
