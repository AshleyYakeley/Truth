module Changes.Core.Types.One.FullResult where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Read
import Changes.Core.Types.One.Read
import Changes.Core.Types.One.Result

data FullResultOneEdit (f :: Type -> Type) edit where
    SuccessFullResultOneEdit :: edit -> FullResultOneEdit f edit
    NewFullResultOneEdit :: f (EditSubject edit) -> FullResultOneEdit f edit

type instance EditReader (FullResultOneEdit f edit) = OneReader f (EditReader edit)

instance FloatingOn edit edit => FloatingOn (FullResultOneEdit f edit) (FullResultOneEdit f edit) where
    floatingUpdate (SuccessFullResultOneEdit e1) (SuccessFullResultOneEdit e2) =
        SuccessFullResultOneEdit $ floatingUpdate e1 e2
    floatingUpdate _ t = t

instance
    (MonadInner f, SubjectReader (EditReader edit), ApplicableEdit edit) =>
    ApplicableEdit (FullResultOneEdit f edit)
    where
    applyEdit (SuccessFullResultOneEdit _edita) mr ReadHasOne = mr ReadHasOne
    applyEdit (SuccessFullResultOneEdit edita) mr (ReadOne rd) =
        unComposeInner $ applyEdit edita (oneReadFunctionF mr) rd
    applyEdit (NewFullResultOneEdit fa) _mr ReadHasOne = return $ fmap (\_ -> ()) fa
    applyEdit (NewFullResultOneEdit fa) _mr (ReadOne rd) =
        return
            $ case retrieveInner fa of
                SuccessResult a -> pure $ subjectToRead a rd
                FailureResult e -> throwExc e

instance
    (MonadInner f, FullSubjectReader (EditReader edit), ApplicableEdit edit) =>
    SubjectMapEdit (FullResultOneEdit f edit)

instance (MonadInner f, FullSubjectReader (EditReader edit), ApplicableEdit edit) => FullEdit (FullResultOneEdit f edit) where
    replaceEdit mr wrt = do
        fsubj <- readableToSubject mr
        wrt $ NewFullResultOneEdit fsubj

instance
    (MonadInner f, FullSubjectReader (EditReader edit), ApplicableEdit edit, InvertibleEdit edit) =>
    InvertibleEdit (FullResultOneEdit f edit)
    where
    invertEdit (SuccessFullResultOneEdit edit) mr = do
        funedits <- unComposeInner $ invertEdit edit $ oneReadFunctionF mr
        case retrieveInner funedits of
            SuccessResult unedits -> return $ fmap SuccessFullResultOneEdit unedits
            FailureResult _ -> return []
    invertEdit (NewFullResultOneEdit _) mr = getReplaceEdits mr

newtype FullResultOneUpdate f update
    = MkFullResultOneUpdate (ResultOneUpdate f update)

type instance UpdateEdit (FullResultOneUpdate f update) = FullResultOneEdit f (UpdateEdit update)

instance (Functor f, IsUpdate update) => IsUpdate (FullResultOneUpdate f update) where
    editUpdate (SuccessFullResultOneEdit edit) = MkFullResultOneUpdate $ SuccessResultOneUpdate $ editUpdate edit
    editUpdate (NewFullResultOneEdit fa) = MkFullResultOneUpdate $ NewResultOneUpdate $ fmap (\_ -> ()) fa

instance (MonadInner f, FullSubjectReader (UpdateReader update)) => FullUpdate (FullResultOneUpdate f update) where
    replaceUpdate mr wrt = do
        fsubj <- readableToSubject mr
        wrt $ MkFullResultOneUpdate $ NewResultOneUpdate $ fmap (\_ -> ()) fsubj

type MaybeEdit edit = FullResultOneEdit Maybe edit

type MaybeUpdate update = FullResultOneUpdate Maybe update

-- | suitable for Results; trying to put a failure code will be rejected
liftFullResultOneChangeLens ::
    forall f updateA updateB.
    ( MonadInner f
    , FullSubjectReader (UpdateReader updateA)
    , ApplicableEdit (UpdateEdit updateA)
    , FullEdit (UpdateEdit updateB)
    ) =>
    ChangeLens updateA updateB ->
    ChangeLens (FullResultOneUpdate f updateA) (FullResultOneUpdate f updateB)
liftFullResultOneChangeLens (MkChangeLens g u pe) = let
    clRead :: ReadFunction (OneReader f (UpdateReader updateA)) (OneReader f (UpdateReader updateB))
    clRead = liftOneReadFunction g
    clUpdate ::
        forall m.
        MonadIO m =>
        FullResultOneUpdate f updateA ->
        Readable m (OneReader f (UpdateReader updateA)) ->
        m [FullResultOneUpdate f updateB]
    clUpdate (MkFullResultOneUpdate (SuccessResultOneUpdate upda)) mr =
        fmap (fmap (MkFullResultOneUpdate . SuccessResultOneUpdate) . fromMaybe [] . mToMaybe)
            $ unComposeInner
            $ u upda
            $ oneReadFunctionF mr
    clUpdate (MkFullResultOneUpdate (NewResultOneUpdate fu)) _mr =
        return $ [MkFullResultOneUpdate $ NewResultOneUpdate fu]
    reshuffle :: forall a. f (Maybe a) -> Maybe (f a)
    reshuffle fma =
        case retrieveInner fma of
            SuccessResult (Just a) -> Just $ pure a
            SuccessResult Nothing -> Nothing
            FailureResult e -> Just $ throwExc e
    clPutEdit ::
        forall m.
        MonadIO m =>
        FullResultOneEdit f (UpdateEdit updateB) ->
        Readable m (OneReader f (UpdateReader updateA)) ->
        m (Maybe [FullResultOneEdit f (UpdateEdit updateA)])
    clPutEdit (SuccessFullResultOneEdit eb) mr = do
        fme <- unComposeInner $ pe [eb] $ oneReadFunctionF mr
        return
            $ case mToMaybe fme of
                Just me -> fmap (fmap SuccessFullResultOneEdit) me
                Nothing -> Just []
    clPutEdit (NewFullResultOneEdit fb) mr = do
        case retrieveInner fb of
            SuccessResult b -> do
                fma <-
                    unComposeInner $ do
                        editbs <- getReplaceEditsFromSubject b
                        meditas <- pe editbs $ oneReadFunctionF mr
                        for meditas $ \editas -> readableToSubject $ applyEdits editas $ oneReadFunctionF mr
                return $ do
                    fa <- reshuffle fma
                    return [NewFullResultOneEdit fa]
            FailureResult e -> do return $ Just [NewFullResultOneEdit $ throwExc e]
    clPutEdits ::
        forall m.
        MonadIO m =>
        [FullResultOneEdit f (UpdateEdit updateB)] ->
        Readable m (OneReader f (UpdateReader updateA)) ->
        m (Maybe [FullResultOneEdit f (UpdateEdit updateA)])
    clPutEdits = clPutEditsFromPutEdit clPutEdit
    in MkChangeLens{..}

-- | suitable for Results; trying to put a failure code will be rejected
liftFullResultOneFloatingChangeLens ::
    forall f updateA updateB.
    ( MonadInner f
    , FullSubjectReader (UpdateReader updateA)
    , ApplicableEdit (UpdateEdit updateA)
    , FullEdit (UpdateEdit updateB)
    ) =>
    FloatingChangeLens updateA updateB ->
    FloatingChangeLens (FullResultOneUpdate f updateA) (FullResultOneUpdate f updateB)
liftFullResultOneFloatingChangeLens (MkFloatingChangeLens (finit :: FloatInit _ r) rlens) = let
    sclInit :: StateLensInit (OneReader f (UpdateReader updateA)) (f r)
    sclInit mr = unComposeInner $ runFloatInit finit $ oneReadFunctionF mr
    reInit ::
        forall m.
        MonadIO m =>
        Readable m (OneReader f (UpdateReader updateA)) ->
        StateT (f r) m r
    reInit mr = do
        r <-
            lift
                $ runFloatInit finit
                $ \rt -> do
                    ft <- mr $ ReadOne rt
                    case retrieveInner ft of
                        SuccessResult t -> return t
                        FailureResult _ -> liftIO $ fail "liftFullResultOneFloatingChangeLens: missing"
        put $ pure r
        return r
    sclRead :: ReadFunctionT (StateT (f r)) (OneReader f (UpdateReader updateA)) (OneReader f (UpdateReader updateB))
    sclRead mr rt = do
        fr <- get
        case retrieveInner fr of
            SuccessResult r -> lift $ liftOneReadFunction (clRead $ rlens r) mr rt
            FailureResult e ->
                return
                    $ case rt of
                        ReadHasOne -> throwExc e
                        ReadOne _ -> throwExc e
    sclUpdate ::
        forall m.
        MonadIO m =>
        FullResultOneUpdate f updateA ->
        Readable m (OneReader f (UpdateReader updateA)) ->
        StateT (f r) m [FullResultOneUpdate f updateB]
    sclUpdate (MkFullResultOneUpdate (SuccessResultOneUpdate upda)) mr = do
        fr <- get
        case retrieveInner fr of
            SuccessResult r ->
                lift
                    $ fmap (fmap (MkFullResultOneUpdate . SuccessResultOneUpdate) . fromMaybe [] . mToMaybe)
                    $ unComposeInner
                    $ clUpdate (rlens r) upda
                    $ oneReadFunctionF mr
            FailureResult _ -> return []
    sclUpdate (MkFullResultOneUpdate (NewResultOneUpdate fu)) mr = do
        case retrieveInner fu of
            SuccessResult () -> do
                _ <- reInit mr
                return ()
            FailureResult e -> put $ throwExc e
        return [MkFullResultOneUpdate $ NewResultOneUpdate fu]
    reshuffle :: forall a. f (Maybe a) -> Maybe (f a)
    reshuffle fma =
        case retrieveInner fma of
            SuccessResult (Just a) -> Just $ pure a
            SuccessResult Nothing -> Nothing
            FailureResult e -> Just $ throwExc e
    sPutEdit ::
        forall m.
        MonadIO m =>
        FullResultOneEdit f (UpdateEdit updateB) ->
        Readable m (OneReader f (UpdateReader updateA)) ->
        StateT (f r) m (Maybe [FullResultOneEdit f (UpdateEdit updateA)])
    sPutEdit (SuccessFullResultOneEdit eb) mr = do
        fr <- get
        case retrieveInner fr of
            SuccessResult r -> do
                fme <- lift $ unComposeInner $ clPutEdits (rlens r) [eb] $ oneReadFunctionF mr
                return
                    $ case mToMaybe fme of
                        Just me -> fmap (fmap SuccessFullResultOneEdit) me
                        Nothing -> Just []
            FailureResult _ -> return $ Just []
    sPutEdit (NewFullResultOneEdit fb) mr = do
        case retrieveInner fb of
            SuccessResult b -> do
                r <- reInit mr
                fma <-
                    lift
                        $ unComposeInner
                        $ do
                            editbs <- getReplaceEditsFromSubject b
                            meditas <- clPutEdits (rlens r) editbs $ oneReadFunctionF mr
                            for meditas $ \editas -> readableToSubject $ applyEdits editas $ oneReadFunctionF mr
                return $ do
                    fa <- reshuffle fma
                    return [NewFullResultOneEdit fa]
            FailureResult e -> do
                put $ throwExc e
                return $ Just [NewFullResultOneEdit $ throwExc e]
    sclPutEdits ::
        forall m.
        MonadIO m =>
        [FullResultOneEdit f (UpdateEdit updateB)] ->
        Readable m (OneReader f (UpdateReader updateA)) ->
        StateT (f r) m (Maybe [FullResultOneEdit f (UpdateEdit updateA)])
    sclPutEdits = clPutEditsFromPutEdit sPutEdit
    in makeStateLens @'NonLinear MkStateChangeLens{..}

-- | for use in UIs where items can be deleted
mustExistOneChangeLens ::
    forall f update.
    MonadInner f =>
    String ->
    ChangeLens (FullResultOneUpdate f update) update
mustExistOneChangeLens err = let
    clRead :: ReadFunction (OneReader f (UpdateReader update)) (UpdateReader update)
    clRead mr rt = do
        ft <- mr $ ReadOne rt
        case retrieveInner ft of
            SuccessResult t -> return t
            FailureResult _ -> liftIO $ fail $ err ++ ": not found"
    clUpdate ::
        forall m.
        MonadIO m =>
        FullResultOneUpdate f update ->
        Readable m (OneReader f (UpdateReader update)) ->
        m [update]
    clUpdate (MkFullResultOneUpdate (NewResultOneUpdate _fu)) _mr = return [] -- just do nothing; it's expected that the UI will delete the item or whatever
    clUpdate (MkFullResultOneUpdate (SuccessResultOneUpdate update)) _ = return [update]
    clPutEdits ::
        forall m.
        MonadIO m =>
        [UpdateEdit update] ->
        Readable m (OneReader f (UpdateReader update)) ->
        m (Maybe [FullResultOneEdit f (UpdateEdit update)])
    clPutEdits edits _ = return $ Just $ fmap SuccessFullResultOneEdit edits
    in MkChangeLens{..}
