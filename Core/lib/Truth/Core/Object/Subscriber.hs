module Truth.Core.Object.Subscriber
(
    Subscriber,SubscriptionW(..),makeObjectSubscriber,
    liftIO,
    -- objectSubscriber,makeSharedSubscriber,
) where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Object.MutableEdit;
    import Truth.Core.Object.Object;


    type Subscriber edit actions = forall editor userstate.
        userstate ->
        (Object edit userstate -> IO editor) -> -- initialise: provides read MutableEdit, initial allowed, write MutableEdit
        (forall m. IsStateIO m => editor -> MutableRead m (EditReader edit) -> [edit] -> StateT userstate m ()) -> -- receive: get updates (both others and from your mutableEdit calls)
        IO (editor,IO (),actions);

    newtype SubscriptionW edit a = MkSubscriptionW (Subscriber edit a);

    instance Functor (SubscriptionW edit) where
    {
        fmap ab (MkSubscriptionW sub) = MkSubscriptionW $ \fs initialise receive -> do
        {
            (editor,cl,a) <- sub fs initialise receive;
            return (editor,cl,ab a);
        };
    };

    data UpdateStoreEntry edit userstate = MkStoreEntry (forall m. IsStateIO m => MutableRead m (EditReader edit) -> [edit] -> StateT userstate m ()) userstate;

    useStateLens :: Lens' Identity (UpdateStoreEntry edit userstate) userstate;
    useStateLens = let
    {
        lensGet (MkStoreEntry _ s) = s;
        lensPutback s (MkStoreEntry u _) = Identity $ MkStoreEntry u s;
    } in MkLens{..};

    type UpdateStore edit = IOWitnessStore (UpdateStoreEntry edit);

    runUpdateStoreEntry :: IsStateIO m => ((MutableRead m (EditReader edit) -> [edit] -> StateT userstate m ()) -> StateT userstate m ()) -> StateT (UpdateStoreEntry edit userstate) m userstate;
    runUpdateStoreEntry call = do
    {
        MkStoreEntry update _ <- get;
        lensStateT useStateLens $ do
        {
            call update;
            get;
        };
    };

    runUpdateStore :: IsStateIO m => (forall userstate. IOWitnessKey userstate -> (MutableRead m (EditReader edit) -> [edit] -> StateT userstate m ()) -> StateT userstate m ()) -> StateT (UpdateStore edit) m ();
    runUpdateStore call = traverseWitnessStoreStateT $ \key -> (runUpdateStoreEntry $ call key) >> return ();

    updateStore :: IsStateIO m => MutableRead m (EditReader edit) -> [edit] -> StateT (UpdateStore edit) m ();
    updateStore mutr edits = runUpdateStore $ \_ ff -> ff mutr edits;

    childMuted :: IsStateIO m => IOWitnessKey userstate -> MutableEdit m edit (UpdateStore edit) -> MutableEdit (StateT (UpdateStore edit) m) edit userstate;
    childMuted key mutedP = MkMutableEdit
    {
        mutableRead = \rt -> lift $ mutableRead mutedP rt,
        mutableEdit = \edits -> do
        {
            mactionP <- lift $ mutableEdit mutedP edits;
            return $ case mactionP of
            {
                Just actionP -> Just $ do
                {
                    nextTokenP <- lift actionP;
                    put nextTokenP;
                    lnextTokenC <- traverseWitnessStoreStateT $ \keyf -> do
                    {
                        newtoken <- runUpdateStoreEntry $ \u -> u (mutableRead mutedP) edits;
                        case testEquality key keyf of
                        {
                            Just Refl -> return [newtoken];
                            Nothing -> return [];
                        };
                    };
                    case lnextTokenC of
                    {
                        [nextTokenC] -> return nextTokenC;
                        _ -> fail "missing updatestore entry";
                    };
                };
                Nothing -> Nothing;
            }
        }
    };

    makeSharedSubscriber :: forall edit actions. Subscriber edit actions -> IO (SubscriptionW edit actions);
    makeSharedSubscriber parent = do
    {
        let
        {
            firstP :: UpdateStore edit;
            firstP = emptyWitnessStore;

            initP :: Object edit (UpdateStore edit) -> IO (Object edit (UpdateStore edit));
            initP objectP = return objectP;

            updateP :: forall m. IsStateIO m => Object edit (UpdateStore edit) -> MutableRead m (EditReader edit) -> [edit] -> StateT (UpdateStore edit) m ();
            updateP _ mutrP edits = updateStore mutrP edits;
        };
        (MkObject objectP,closerP,actions) <- parent firstP initP updateP;
        let
        {
            objectC :: forall userstate. IOWitnessKey userstate -> Object edit userstate;
            objectC key = MkObject $ \call -> objectP $ \(mutedP :: MutableEdit m edit (UpdateStore edit)) -> do
            {
                oldstore <- get;
                let
                {
                    mutedC :: MutableEdit (StateT (UpdateStore edit) m) edit userstate;
                    mutedC = childMuted key mutedP;

                    oldStateC :: userstate;
                    (MkStoreEntry _ oldStateC) = fromJust $ lookupWitnessStore key oldstore;
                };
                (r,newStateC) <- runStateT (call mutedC) oldStateC;
                replaceWitnessStoreStateT key $ lensStateT useStateLens $ put newStateC;
                return r;
            };

            child :: Subscriber edit actions;
            child firstC initC updateC = do
            {
                rec
                {
                    key <- objectP $ \_ -> remonad liftIO $ addIOWitnessStoreStateT (MkStoreEntry (updateC editorC) firstC);
                    editorC <- initC (objectC key);
                };
                let
                {
                    closerC = objectP $ \_ -> do
                    {
                        deleteWitnessStoreStateT key;
                        newstore <- get;
                        if isEmptyWitnessStore newstore
                         then liftIO closerP;
                         else return ();
                    };
                };
                return (editorC,closerC,actions);
            };
        };
        return $ MkSubscriptionW child;
    };

    objectSubscriber :: Object edit () -> SubscriptionW edit ();
    objectSubscriber (MkObject object) = MkSubscriptionW $ \firstState initr _update -> do
    {
        var <- newMVar firstState;
        rec
        {
            editor <- initr $ MkObject $ \call -> object $ \muted -> unitStateT $ modifyMVarStateIO var $ do
            {
                oldst <- get;
                let
                {
                    muted' = MkMutableEdit
                    {
                        mutableRead = mutableRead muted,
                        mutableEdit = \edits -> do
                        {
                            maction <- mutableEdit muted edits;
                            case maction of
                            {
                                Nothing -> return Nothing;
                                Just action -> return $ Just $ do
                                {
                                    action;
                                    ((),nst) <- runStateT ({- update editor (mutableRead muted) edits-} return ()) oldst;
                                    return nst;
                                }
                            }
                        }
                    };
                };
                call muted';
            };
        };
        return (editor,return (),());
    };

    makeObjectSubscriber :: Object edit () -> IO (SubscriptionW edit ());
    makeObjectSubscriber object = let
    {
        MkSubscriptionW sub = objectSubscriber object;
    } in makeSharedSubscriber sub;
}
