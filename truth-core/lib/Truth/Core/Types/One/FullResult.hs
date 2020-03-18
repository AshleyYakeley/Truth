module Truth.Core.Types.One.FullResult where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Lens
import Truth.Core.Read
import Truth.Core.Types.One.Read
import Truth.Core.Types.One.Result

data FullResultOneEdit (f :: Type -> Type) edit where
    SuccessFullResultOneEdit :: edit -> FullResultOneEdit f edit
    NewFullResultOneEdit :: f (EditSubject edit) -> FullResultOneEdit f edit

type instance EditReader (FullResultOneEdit f edit) =
     OneReader f (EditReader edit)

instance Floating edit edit => Floating (FullResultOneEdit f edit) (FullResultOneEdit f edit) where
    floatingUpdate (SuccessFullResultOneEdit e1) (SuccessFullResultOneEdit e2) =
        SuccessFullResultOneEdit $ floatingUpdate e1 e2
    floatingUpdate _ t = t

instance (MonadOne f, SubjectReader (EditReader edit), ApplicableEdit edit) => ApplicableEdit (FullResultOneEdit f edit) where
    applyEdit (SuccessFullResultOneEdit _edita) mr ReadHasOne = mr ReadHasOne
    applyEdit (SuccessFullResultOneEdit edita) mr (ReadOne reader) =
        getComposeM $ applyEdit edita (oneReadFunctionF mr) reader
    applyEdit (NewFullResultOneEdit fa) _mr ReadHasOne = return $ fmap (\_ -> ()) fa
    applyEdit (NewFullResultOneEdit fa) _mr (ReadOne reader) =
        return $
        case retrieveOne fa of
            SuccessResult a -> pure $ subjectToRead a reader
            FailureResult (MkLimit fx) -> fx

instance (MonadOne f, FullSubjectReader (EditReader edit), ApplicableEdit edit) =>
             SubjectMapEdit (FullResultOneEdit f edit)

instance (MonadOne f, FullSubjectReader (EditReader edit), ApplicableEdit edit) => FullEdit (FullResultOneEdit f edit) where
    replaceEdit mr writer = do
        fsubj <- mutableReadToSubject mr
        writer $ NewFullResultOneEdit fsubj

instance (MonadOne f, FullSubjectReader (EditReader edit), ApplicableEdit edit, InvertibleEdit edit) =>
             InvertibleEdit (FullResultOneEdit f edit) where
    invertEdit (SuccessFullResultOneEdit edit) mr = do
        funedits <- getComposeM $ invertEdit edit $ oneReadFunctionF mr
        case retrieveOne funedits of
            SuccessResult unedits -> return $ fmap SuccessFullResultOneEdit unedits
            FailureResult _ -> return []
    invertEdit (NewFullResultOneEdit _) mr = getReplaceEdits mr

newtype FullResultOneUpdate f update =
    MkFullResultOneUpdate (ResultOneUpdate f update)

type instance UpdateEdit (FullResultOneUpdate f update) =
     FullResultOneEdit f (UpdateEdit update)

instance (Functor f, IsUpdate update) => IsUpdate (FullResultOneUpdate f update) where
    editUpdate (SuccessFullResultOneEdit edit) = MkFullResultOneUpdate $ SuccessResultOneUpdate $ editUpdate edit
    editUpdate (NewFullResultOneEdit fa) = MkFullResultOneUpdate $ NewResultOneUpdate $ fmap (\_ -> ()) fa

type MaybeEdit edit = FullResultOneEdit Maybe edit

type MaybeUpdate update = FullResultOneUpdate Maybe update

-- | suitable for Results; trying to put a failure code will be rejected
liftFullResultOneEditLens ::
       forall f updateA updateB.
       ( MonadOne f
       , FullSubjectReader (UpdateReader updateA)
       , ApplicableEdit (UpdateEdit updateA)
       , FullEdit (UpdateEdit updateB)
       )
    => EditLens updateA updateB
    -> EditLens (FullResultOneUpdate f updateA) (FullResultOneUpdate f updateB)
liftFullResultOneEditLens (MkEditLens g u pe) = let
    elGet :: ReadFunction (OneReader f (UpdateReader updateA)) (OneReader f (UpdateReader updateB))
    elGet = liftOneReadFunction g
    elUpdate ::
           forall m. MonadIO m
        => FullResultOneUpdate f updateA
        -> MutableRead m (OneReader f (UpdateReader updateA))
        -> m [FullResultOneUpdate f updateB]
    elUpdate (MkFullResultOneUpdate (SuccessResultOneUpdate upda)) mr =
        fmap (fmap (MkFullResultOneUpdate . SuccessResultOneUpdate) . fromMaybe [] . getMaybeOne) $
        getComposeM $ u upda $ oneReadFunctionF mr
    elUpdate (MkFullResultOneUpdate (NewResultOneUpdate fu)) _mr =
        return $ [MkFullResultOneUpdate $ NewResultOneUpdate fu]
    reshuffle :: forall a. f (Maybe a) -> Maybe (f a)
    reshuffle fma =
        case retrieveOne fma of
            SuccessResult (Just a) -> Just $ pure a
            SuccessResult Nothing -> Nothing
            FailureResult (MkLimit fx) -> Just fx
    elPutEdit ::
           forall m. MonadIO m
        => FullResultOneEdit f (UpdateEdit updateB)
        -> MutableRead m (OneReader f (UpdateReader updateA))
        -> m (Maybe [FullResultOneEdit f (UpdateEdit updateA)])
    elPutEdit (SuccessFullResultOneEdit eb) mr = do
        fme <- getComposeM $ pe [eb] $ oneReadFunctionF mr
        return $
            case getMaybeOne fme of
                Just me -> fmap (fmap SuccessFullResultOneEdit) me
                Nothing -> Just []
    elPutEdit (NewFullResultOneEdit fb) mr = do
        case retrieveOne fb of
            SuccessResult b -> do
                fma <-
                    getComposeM $ do
                        editbs <- getReplaceEditsFromSubject b
                        meditas <- pe editbs $ oneReadFunctionF mr
                        for meditas $ \editas -> mutableReadToSubject $ applyEdits editas $ oneReadFunctionF mr
                return $ do
                    fa <- reshuffle fma
                    return [NewFullResultOneEdit fa]
            FailureResult (MkLimit fx) -> do return $ Just [NewFullResultOneEdit fx]
    elPutEdits ::
           forall m. MonadIO m
        => [FullResultOneEdit f (UpdateEdit updateB)]
        -> MutableRead m (OneReader f (UpdateReader updateA))
        -> m (Maybe [FullResultOneEdit f (UpdateEdit updateA)])
    elPutEdits = elPutEditsFromPutEdit elPutEdit
    in MkEditLens {..}

-- | suitable for Results; trying to put a failure code will be rejected
liftFullResultOneFloatingEditLens ::
       forall f updateA updateB.
       ( MonadOne f
       , FullSubjectReader (UpdateReader updateA)
       , ApplicableEdit (UpdateEdit updateA)
       , FullEdit (UpdateEdit updateB)
       )
    => FloatingEditLens updateA updateB
    -> FloatingEditLens (FullResultOneUpdate f updateA) (FullResultOneUpdate f updateB)
liftFullResultOneFloatingEditLens (MkFloatingEditLens (init :: FloatInit _ r) rlens) = let
    sInit :: StateLensInit (OneReader f (UpdateReader updateA)) (f r)
    sInit mr = getComposeM $ runFloatInit init $ oneReadFunctionF mr
    reInit ::
           forall m. MonadIO m
        => MutableRead m (OneReader f (UpdateReader updateA))
        -> StateT (f r) m r
    reInit mr = do
        r <-
            lift $
            runFloatInit init $ \rt -> do
                ft <- mr $ ReadOne rt
                case retrieveOne ft of
                    SuccessResult t -> return t
                    FailureResult _ -> liftIO $ fail "liftFullResultOneFloatingEditLens: missing"
        put $ pure r
        return r
    sGet :: ReadFunctionT (StateT (f r)) (OneReader f (UpdateReader updateA)) (OneReader f (UpdateReader updateB))
    sGet mr rt = do
        fr <- get
        case retrieveOne fr of
            SuccessResult r -> lift $ liftOneReadFunction (elGet $ rlens r) mr rt
            FailureResult (MkLimit fx) ->
                case rt of
                    ReadHasOne -> return fx
                    ReadOne _ -> return fx
    sUpdate ::
           forall m. MonadIO m
        => FullResultOneUpdate f updateA
        -> MutableRead m (OneReader f (UpdateReader updateA))
        -> StateT (f r) m [FullResultOneUpdate f updateB]
    sUpdate (MkFullResultOneUpdate (SuccessResultOneUpdate upda)) mr = do
        fr <- get
        case retrieveOne fr of
            SuccessResult r ->
                lift $
                fmap (fmap (MkFullResultOneUpdate . SuccessResultOneUpdate) . fromMaybe [] . getMaybeOne) $
                getComposeM $ elUpdate (rlens r) upda $ oneReadFunctionF mr
            FailureResult _ -> return []
    sUpdate (MkFullResultOneUpdate (NewResultOneUpdate fu)) mr = do
        case retrieveOne fu of
            SuccessResult () -> do
                _ <- reInit mr
                return ()
            FailureResult (MkLimit fx) -> put fx
        return [MkFullResultOneUpdate $ NewResultOneUpdate fu]
    reshuffle :: forall a. f (Maybe a) -> Maybe (f a)
    reshuffle fma =
        case retrieveOne fma of
            SuccessResult (Just a) -> Just $ pure a
            SuccessResult Nothing -> Nothing
            FailureResult (MkLimit fx) -> Just fx
    sPutEdit ::
           forall m. MonadIO m
        => FullResultOneEdit f (UpdateEdit updateB)
        -> MutableRead m (OneReader f (UpdateReader updateA))
        -> StateT (f r) m (Maybe [FullResultOneEdit f (UpdateEdit updateA)])
    sPutEdit (SuccessFullResultOneEdit eb) mr = do
        fr <- get
        case retrieveOne fr of
            SuccessResult r -> do
                fme <- lift $ getComposeM $ elPutEdits (rlens r) [eb] $ oneReadFunctionF mr
                return $
                    case getMaybeOne fme of
                        Just me -> fmap (fmap SuccessFullResultOneEdit) me
                        Nothing -> Just []
            FailureResult _ -> return $ Just []
    sPutEdit (NewFullResultOneEdit fb) mr = do
        case retrieveOne fb of
            SuccessResult b -> do
                r <- reInit mr
                fma <-
                    lift $
                    getComposeM $ do
                        editbs <- getReplaceEditsFromSubject b
                        meditas <- elPutEdits (rlens r) editbs $ oneReadFunctionF mr
                        for meditas $ \editas -> mutableReadToSubject $ applyEdits editas $ oneReadFunctionF mr
                return $ do
                    fa <- reshuffle fma
                    return [NewFullResultOneEdit fa]
            FailureResult (MkLimit fx) -> do
                put fx
                return $ Just [NewFullResultOneEdit fx]
    sPutEdits ::
           forall m. MonadIO m
        => [FullResultOneEdit f (UpdateEdit updateB)]
        -> MutableRead m (OneReader f (UpdateReader updateA))
        -> StateT (f r) m (Maybe [FullResultOneEdit f (UpdateEdit updateA)])
    sPutEdits = elPutEditsFromPutEdit sPutEdit
    in makeStateLens MkStateEditLens {..}

mustExistOneEditLens ::
       forall f update. (MonadOne f, IsUpdate update)
    => String
    -> EditLens (FullResultOneUpdate f update) update
mustExistOneEditLens err = let
    elGet :: ReadFunction (OneReader f (UpdateReader update)) (UpdateReader update)
    elGet mr rt = do
        ft <- mr $ ReadOne rt
        case retrieveOne ft of
            SuccessResult t -> return t
            FailureResult _ -> liftIO $ fail $ err ++ ": not found"
    elUpdate ::
           forall m. MonadIO m
        => FullResultOneUpdate f update
        -> MutableRead m (OneReader f (UpdateReader update))
        -> m [update]
    elUpdate (MkFullResultOneUpdate (NewResultOneUpdate _fu)) _mr = liftIO $ fail $ err ++ ": replaced"
    elUpdate (MkFullResultOneUpdate (SuccessResultOneUpdate update)) _ = return [update]
    elPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit update]
        -> MutableRead m (OneReader f (UpdateReader update))
        -> m (Maybe [FullResultOneEdit f (UpdateEdit update)])
    elPutEdits edits _ = return $ Just $ fmap SuccessFullResultOneEdit edits
    in MkEditLens {..}
