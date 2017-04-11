module Truth.Core.Object.Lens where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types;
    import Truth.Core.Object.API;
    import Truth.Core.Object.Object;


    class ObjectLens lens where
    {
        type LensDomain lens;
        type LensRange lens;

        lensObject :: lens -> Object (LensDomain lens) -> Object (LensRange lens);
    };

    instance (Monad f,MonadOne f,Edit edita) => ObjectLens (FloatingEditLens' f lensstate edita editb) where
    {
        type LensDomain (FloatingEditLens' f lensstate edita editb) = edita;
        type LensRange (FloatingEditLens' f lensstate edita editb) = editb;

        lensObject lens@MkFloatingEditLens{..} sub (initialB :: LockAPI editb userstate -> IO (editor,userstate)) updateB = let
        {
            MkFloatingEditFunction{..} = floatingEditLensFunction;

            initialA :: LockAPI edita (userstate,lensstate) -> IO (editor,(userstate,lensstate));
            initialA lapiA = do
            {
                (ed,us) <- initialB $ mapLockAPI lens lapiA;
                return (ed,(us,floatingEditInitial));
            };

            updateA :: editor -> (userstate,lensstate) -> [edita] -> IO (userstate,lensstate);
            updateA editor (oldus,oldls) editAs = do
            {
                let
                {
                    (newls,editBs) = floatingEditUpdates floatingEditLensFunction editAs oldls;
                };
                newus <- updateB editor oldus editBs;
                return (newus,newls);
            };
        } in sub initialA updateA;;
    };

    instance (Applicative f,MonadOne f,Edit edita) => ObjectLens (EditLens' f edita editb) where
    {
        type LensDomain (EditLens' f edita editb) = edita;
        type LensRange (EditLens' f edita editb) = editb;

        lensObject lens@MkEditLens{..} sub (initialB :: LockAPI editb userstate -> IO (editor,userstate)) updateB = do
        {
            let
            {
                MkEditFunction{..} = editLensFunction;

                initialA :: LockAPI edita userstate -> IO (editor,userstate);
                initialA (MkLockAPI lapiA) = let
                {
                    lapiB :: LockAPI editb userstate;
                    lapiB = MkLockAPI $ \(callB :: forall m. MonadIOInvert m => userstate -> API m editb userstate -> m r) -> let
                    {
                        callA :: forall m. MonadIOInvert m => userstate -> API m edita userstate -> m r;
                        callA userstate (apiA :: API m edita userstate) = let
                        {
                            readA :: Structure m (EditReader edita);
                            _allowedA :: edita -> m Bool;
                            pushEditA :: [edita] -> m (Maybe userstate);
                            MkAPI readA _allowedA pushEditA = apiA;

                            readB :: Structure m (EditReader editb);
                            readB = mapStructure editGet readA;

                            convertEdits :: [editb] -> m (Maybe [edita]);
                            convertEdits editBs = do
                            {
                                feditA :: f [edita] <- unReadable (editLensPutEdits lens editBs) readA;
                                return $ getMaybeOne feditA;
                            };

                            allowedB :: editb -> m Bool;
                            allowedB editB = do
                            {
                                lensOK <- unReadable (editLensAllowed lens editB) readA;
                                case lensOK of
                                {
                                    False -> return False;
                                    True -> do
                                    {
                                        meditA <- convertEdits [editB];
                                        case meditA of
                                        {
                                            Nothing -> return False; -- is this correct?
                                            Just editA -> apiAlloweds apiA editA;
                                        };
                                    };
                                };
                            };

                            pushEditB :: [editb] -> m (Maybe userstate);
                            pushEditB editBs = do
                            {
                                meditA <- convertEdits editBs;
                                case meditA of
                                {
                                    Nothing -> return $ Just userstate; -- is this correct?
                                    Just editAs -> pushEditA editAs;
                                };
                            };

                            apiB :: API m editb userstate;
                            apiB = MkAPI readB allowedB pushEditB;
                        }
                        in callB userstate apiB;
                    } in lapiA callA;
                } in initialB lapiB;

                updateA :: editor -> userstate -> [edita] -> IO userstate;
                updateA editor oldtoken editAs = do
                {
                    let
                    {
                        leditB = editUpdates editLensFunction editAs;
                    };
                    updateB editor oldtoken leditB;
                };
            };
            sub initialA updateA;
        };
    };

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
    instance (Applicative m,MonadOne m,Edit edita) => ObjectLens (CleanEditLens' m edita editb) where
    {
        type LensDomain (CleanEditLens' m edita editb) = edita;
        type LensRange (CleanEditLens' m edita editb) = editb;

        lensObject celens = lensObject (cleanEditLens celens);
    };

    instance (MonadOne f) => ObjectLens (Lens' f a b) where
    {
        type LensDomain (Lens' f a b) = WholeEdit (WholeReader a);
        type LensRange (Lens' f a b) = WholeEdit (WholeReader b);

        lensObject MkLens{..} sub (initialB :: LockAPI (WholeEdit (WholeReader b)) userstate -> IO (editor,userstate)) updateB = do
        {
            let
            {
                initialA :: LockAPI (WholeEdit (WholeReader a)) userstate -> IO (editor,userstate);
                initialA (MkLockAPI lapiA) = let
                {
                    lapiB :: LockAPI (WholeEdit (WholeReader b)) userstate;
                    lapiB = MkLockAPI $ \(callB :: forall m. MonadIOInvert m => userstate -> API m (WholeEdit (WholeReader b)) userstate -> m r) -> let
                    {
                        callA :: forall m. MonadIOInvert m => userstate -> API m (WholeEdit (WholeReader a)) userstate -> m r;
                        callA userstate (apiA :: API m (WholeEdit (WholeReader a)) userstate) = let
                        {
                            readA :: Structure m (WholeReader a);
                            allowedA :: WholeEdit (WholeReader a) -> m Bool;
                            pushEditA :: [WholeEdit (WholeReader a)] -> m (Maybe userstate);
                            MkAPI readA allowedA pushEditA = apiA;

                            readB :: Structure m (WholeReader b);
                            readB ReadWhole = fmap lensGet $ readA ReadWhole;

                            last' :: forall x. [x] -> Maybe x;
                            last' [] = Nothing;
                            last' [x] = Just x;
                            last' (_:xx) = last' xx;

                            convertEdits :: [WholeEdit (WholeReader b)] -> m (Maybe [WholeEdit (WholeReader a)]);
                            convertEdits editBs = case last' editBs of
                            {
                                Nothing -> return $ pure [];
                                Just (MkWholeEdit b) -> do
                                {
                                    oldA <- readA ReadWhole;
                                    let
                                    {
                                        fnewA :: f a;
                                        fnewA = lensPutback b oldA;

                                        mnewA :: Maybe a;
                                        mnewA = getMaybeOne fnewA;
                                    };
                                    return $ fmap (\a -> [MkWholeEdit a]) mnewA;
                                };
                            };

                            allowedB :: (WholeEdit (WholeReader b)) -> m Bool;
                            allowedB (MkWholeEdit b) = do
                            {
                                oldA <- readA ReadWhole;
                                let
                                {
                                    fnewA :: f a;
                                    fnewA = lensPutback b oldA;

                                    mnewA :: Maybe a;
                                    mnewA = getMaybeOne fnewA;
                                };
                                case mnewA of
                                {
                                    Nothing -> return False;
                                    Just newA -> allowedA $ MkWholeEdit newA;
                                };
                            };

                            pushEditB :: [WholeEdit (WholeReader b)] -> m (Maybe userstate);
                            pushEditB editB = do
                            {
                                meditA <- convertEdits editB;
                                case meditA of
                                {
                                    Nothing -> return $ Just userstate; -- is this correct?
                                    Just editA -> pushEditA editA;
                                };
                            };

                            apiB :: API m (WholeEdit (WholeReader b)) userstate;
                            apiB = MkAPI readB allowedB pushEditB;
                        }
                        in callB userstate apiB;
                    } in lapiA callA;
                } in initialB lapiB;

                updateA :: editor -> userstate -> [WholeEdit (WholeReader a)] -> IO userstate;
                updateA editor oldtoken editAs = do
                {
                    let
                    {
                        leditB = fmap (\(MkWholeEdit a) -> MkWholeEdit $ lensGet a) editAs;
                    };
                    updateB editor oldtoken leditB;
                };
            };
            sub initialA updateA;
        };
    };

    instance (MonadOne m) => ObjectLens (Injection' m a b) where
    {
        type LensDomain (Injection' m a b) = WholeEdit (WholeReader a);
        type LensRange (Injection' m a b) = WholeEdit (WholeReader b);

        lensObject inj = lensObject (injectionLens inj);
    };

    instance ObjectLens (Bijection a b) where
    {
        type LensDomain (Bijection a b) = WholeEdit (WholeReader a);
        type LensRange (Bijection a b) = WholeEdit (WholeReader b);

        lensObject bi = lensObject (bijectionInjection bi);
    };

    instance ObjectLens (Codec a b) where
    {
        type LensDomain (Codec a b) = WholeEdit (WholeReader a);
        type LensRange (Codec a b) = WholeEdit (WholeReader (Maybe b));

        lensObject codec = lensObject (codecInjection codec);
    };
}
