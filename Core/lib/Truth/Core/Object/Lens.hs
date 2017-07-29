module Truth.Core.Object.Lens where
{
    import Truth.Core.Import;
    import Truth.Core.Edit;
    import Truth.Core.Read;
    import Truth.Core.Object.Object;
    import Truth.Core.Object.Subscriber;


    mapSubscriber :: forall f edita editb action. (MonadOne f,Edit edita) => GeneralLens' f edita editb -> Subscriber edita action -> Subscriber editb action;
    mapSubscriber (MkCloseFloat (lens@MkFloatingEditLens{..} :: IOFloatingEditLens' f lensstate edita editb)) sub (initialB :: Object editb -> IO editor) updateB = do
    {
        let
        {
            MkFloatingEditFunction{..} = floatingEditLensFunction;
        };
        lensvar <- newMVar floatingEditInitial;
        let
        {
            initialA :: Object edita -> IO (Object edita,editor);
            initialA objectA = do
            {
                ed <- initialB $ floatingMapObject (mvarStateAccess lensvar) lens objectA;
                return (objectA,ed);
            };

            updateA :: forall m. IsStateIO m => (Object edita,editor) -> MutableRead m (EditReader edita) -> [edita] -> m ();
            updateA (_objectA,editor) mr editAs = mvarStateAccess lensvar $ StateT $ \oldls -> do
            {
                (newls,editBs) <- unReadable (floatingEditUpdates floatingEditLensFunction editAs oldls) mr;
                updateB editor (mapMutableRead (floatingEditGet newls) mr) editBs;
                return ((),newls);
            };
        };
        ((_,editor),closer,action) <- sub initialA updateA;
        return (editor,closer,action);
    };

    convertSubscriber :: forall edita editb actions. (EditSubject edita ~ EditSubject editb,FullEdit edita,FullEdit editb) => Subscriber edita actions -> Subscriber editb actions;
    convertSubscriber = mapSubscriber $ toGeneralLens' (convertEditLens :: EditLens' Identity edita editb);

{-

    cacheReferent :: forall edit. (Edit edit,FullReader (EditReader edit)) =>
        Reference edit -> Reference edit;
    cacheReferent ref = objSubscribe (\sendDown -> do
    {
        let
        {
            initref read firstallowed sendUp = do
            {
                firsta <- readStructureW read fromReader;   -- read the whole thing in
                statevar :: MVar (EditSubject edit,Allowed edit) <- newMVar (firsta,firstallowed);
                return (statevar,sendUp);
            };

            receive (statevar,_sendUp) edit newallowed = modifyMVar_ statevar $ \(olda,oldallowed) -> let
            {
                newa = fromReadFunction (applyEdit edit) olda;  -- read from cache
            } in do
            {
                sendDown edit newallowed;
                return (newa,newallowed);
            };
        };
        ((statevar,sendUp),sub) <- ref initref receive;
        let
        {
            objGetInitial :: forall r. (Allowed edit -> IO r) -> IO r;
            objGetInitial initialise = withMVar statevar $ \(_a,allowed) -> initialise allowed;

            objRead :: StructureW IO (EditReader edit);
            objRead = MkStructureW $ readFromM $ withMVar statevar $ \(a,_allowed) -> return a;

            objSend :: edit -> IO (Maybe (Allowed edit));
            objSend edit = modifyMVar statevar $ \olds@(olda,_oldallowed) -> do
            {
                mv <- sendUp edit;
                return (case mv of
                {
                    Just newallowed -> (fromReadFunction (applyEdit edit) olda,newallowed);
                    _ -> olds;
                },mv);
            };

            objClose :: IO ();
            objClose = subClose sub;
        };
        return MkObject{..};
    });
-}

{-
    wantedA :: ReadFunction (EditReader ea) (PairEditReader ea eb);
    wantedA = undefined;

    wantedB :: ReadFunction (EditReader eb) (PairEditReader ea eb);
    wantedB = undefined;

    pairSubscriber :: forall ea eb acta actb. Subscriber ea acta -> Subscriber eb actb -> Subscriber (PairEdit ea eb) (acta,actb);
    pairSubscriber subA subB (initAB :: Object (PairEdit ea eb) -> IO editor) updateAB = do
    {
        let
        {
            initA :: Object ea -> IO (editor,IO (),actb);
            initA objectA = do
            {
                let
                {
                    initB :: Object eb -> IO editor;
                    initB objectB = initAB $ pairObject objectA objectB;

                    updateB :: forall m. IsStateIO m => editor -> MutableRead m (EditReader eb) -> [eb] -> m ();
                    updateB editor mr edits = updateAB editor (mapMutableRead wantedB mr) (fmap (MkTupleEdit EditSecond) edits);
                };
                subB initB updateB;
            };

            updateA :: forall m. IsStateIO m => (editor,IO (),actb) -> MutableRead m (EditReader ea) -> [ea] -> m ();
            updateA (editor,_,_) mr edits = updateAB editor (mapMutableRead wantedA mr) (fmap (MkTupleEdit EditFirst) edits);
        };
        ((ed,closeB,actionsB),closeA,actionsA) <- subA initA updateA;
        return (ed,closeB >> closeA,(actionsA,actionsB));
    };
-}
}
