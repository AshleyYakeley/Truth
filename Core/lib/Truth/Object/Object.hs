module Truth.Object.Object where
{
    --import Truth.Object.Subscribe;
    import Truth.Edit;
    import Truth.Edit.Import;


    data API m edit token = MkAPI
    {
        apiRead :: Structure m (EditReader edit),
        apiAllowed :: edit -> m Bool,
        apiEdit :: edit -> m token
    };

    instance Functor m => Functor (API m edit) where
    {
        fmap ab (MkAPI r a e) = MkAPI r a $ fmap (fmap ab) e;
    };

    type LockAPI edit token = forall r. (forall m. Monad m => token -> API m edit token -> m r) -> IO r;

    mapLockAPIToken :: (token1 -> token2) -> LockAPI edit token1 -> LockAPI edit token2;
    mapLockAPIToken t1t2 lapi1 ff = lapi1 $ \token api -> ff (t1t2 token) (fmap t1t2 api);

    type Subscribe edit = forall editor token.
        (LockAPI edit token -> IO (editor,token)) -> -- initialise: provides read API, initial allowed, write API
        (editor -> token -> edit -> IO token) -> -- receive: get updates (both others and from your apiEdit calls)
        IO (editor, IO ());

    class IsEditLens lens where
    {
        type LensDomain lens;
        type LensRange lens;

        lensSubscribe :: lens -> Subscribe (LensDomain lens) -> Subscribe (LensRange lens);
    };


    instance (FunctorOne f,Edit edita) => IsEditLens (FloatingEditLens' f state edita editb) where
    {
        type LensDomain (FloatingEditLens' f state edita editb) = edita;
        type LensRange (FloatingEditLens' f state edita editb) = editb;

        lensSubscribe lens@MkFloatingEditLens{..} sub (initialB :: LockAPI editb token -> IO (editor,token)) updateB = do
        {
            let
            {
                MkFloatingEditFunction{..} = floatingEditLensFunction;
            };
            statevar <- newMVar floatingEditInitial;
            let
            {
                initialA :: LockAPI edita token -> IO (editor,token);
                initialA lapiA = let
                {
                    lapiB :: LockAPI editb token;
                    lapiB (callB :: forall m. Monad m => token -> API m editb token -> m r) = withMVar statevar $ \state -> let
                    {
                        callA :: forall m. Monad m => token -> API m edita token -> m r;
                        callA token (apiA :: API m edita token) = let
                        {
                            readA :: Structure m (EditReader edita);
                            allowedA :: edita -> m Bool;
                            pushEditA :: edita -> m token;
                            MkAPI readA allowedA pushEditA = apiA;

                            readB :: Structure m (EditReader editb);
                            readB = mapStructure (floatingEditGet state) readA;

                            convertEdit :: editb -> m (Maybe edita);
                            convertEdit editB = do
                            {
                                feditA :: f editA <- unReadable (floatingEditLensPutEdit state editB) readA;
                                return $ getMaybeOne feditA;
                            };

                            allowedB :: editb -> m Bool;
                            allowedB editB = do
                            {
                                lensOK <- unReadable (floatingEditLensAllowed lens state editB) readA;
                                case lensOK of
                                {
                                    False -> return False;
                                    True -> do
                                    {
                                        meditA <- convertEdit editB;
                                        case meditA of
                                        {
                                            Nothing -> return False; -- is this correct?
                                            Just editA -> allowedA editA;
                                        };
                                    };
                                };
                            };

                            pushEditB :: editb -> m token;
                            pushEditB editB = do
                            {
                                meditA <- convertEdit editB;
                                case meditA of
                                {
                                    Nothing -> return token; -- is this correct?
                                    Just editA -> pushEditA editA;
                                };
                            };

                            apiB :: API m editb token;
                            apiB = MkAPI readB allowedB pushEditB;
                        }
                        in callB token apiB;
                    } in lapiA callA;
                } in initialB lapiB;

                updateA :: editor -> token -> edita -> IO token;
                updateA editor oldtoken editA = modifyMVar statevar $ \oldstate -> do
                {
                    -- readA :: Structure IO (EditReader edita);
                    let
                    {
                        (newstate,meditB) = floatingEditUpdate editA oldstate;
                    };
                    newtoken <- case meditB of
                    {
                        Just editB -> updateB editor oldtoken editB;
                        Nothing -> return oldtoken;
                    };
                    return (newstate,newtoken);
                };
            };
            sub initialA updateA;
        };
    };

    instance (FunctorOne f,Edit edita) => IsEditLens (EditLens' f edita editb) where
    {
        type LensDomain (EditLens' f edita editb) = edita;
        type LensRange (EditLens' f edita editb) = editb;

        lensSubscribe lens@MkEditLens{..} sub (initialB :: LockAPI editb token -> IO (editor,token)) updateB = do
        {
            let
            {
                MkEditFunction{..} = editLensFunction;

                initialA :: LockAPI edita token -> IO (editor,token);
                initialA lapiA = let
                {
                    lapiB :: LockAPI editb token;
                    lapiB (callB :: forall m. Monad m => token -> API m editb token -> m r) = let
                    {
                        callA :: forall m. Monad m => token -> API m edita token -> m r;
                        callA token (apiA :: API m edita token) = let
                        {
                            readA :: Structure m (EditReader edita);
                            allowedA :: edita -> m Bool;
                            pushEditA :: edita -> m token;
                            MkAPI readA allowedA pushEditA = apiA;

                            readB :: Structure m (EditReader editb);
                            readB = mapStructure editGet readA;

                            convertEdit :: editb -> m (Maybe edita);
                            convertEdit editB = do
                            {
                                feditA :: f editA <- unReadable (editLensPutEdit editB) readA;
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
                                        meditA <- convertEdit editB;
                                        case meditA of
                                        {
                                            Nothing -> return False; -- is this correct?
                                            Just editA -> allowedA editA;
                                        };
                                    };
                                };
                            };

                            pushEditB :: editb -> m token;
                            pushEditB editB = do
                            {
                                meditA <- convertEdit editB;
                                case meditA of
                                {
                                    Nothing -> return token; -- is this correct?
                                    Just editA -> pushEditA editA;
                                };
                            };

                            apiB :: API m editb token;
                            apiB = MkAPI readB allowedB pushEditB;
                        }
                        in callB token apiB;
                    } in lapiA callA;
                } in initialB lapiB;

                updateA :: editor -> token -> edita -> IO token;
                updateA editor oldtoken editA = do
                {
                    let
                    {
                        meditB = editUpdate editA;
                    };
                    case meditB of
                    {
                        Just editB -> updateB editor oldtoken editB;
                        Nothing -> return oldtoken;
                    };
                };
            };
            sub initialA updateA;
        };
    };

{-
    -- | change the object.
    -- The action argument and subscription updates are mutually excluded.
    -- Returns Nothing if change is not allowed.
    -- Returns Just () if change succeeded, and updates will be received before this action returns.
    ;
    -- type Push edit = edit -> IO (Maybe ());

    newtype StructureW m reader = MkStructureW (Structure m reader);

    readStructureW :: (Applicative m,Monad m) => StructureW m reader -> Readable reader a -> m a;
    readStructureW (MkStructureW smr) (MkReadable smrma) = smrma smr;

    type EditObject edit = Object (StructureW IO (EditReader edit)) edit;
    type Reference edit = Subscribe (StructureW IO (EditReader edit)) edit;

    -- | A free object is one based on an in-memory value.
    freeObjSubscribe :: forall edit. (Edit edit,FullReader (EditReader edit)) => EditSubject edit -> (EditSubject edit -> edit -> Bool) -> Reference edit;
    freeObjSubscribe initial allowed = objSubscribe (\_ -> do
    {
        statevar :: MVar (EditSubject edit) <- newMVar initial;
        let
        {
            objRead :: StructureW IO (EditReader edit);
            objRead = MkStructureW $ \reader -> withMVar statevar $ \s -> readFromM (return s) reader;

            objGetInitial :: forall r. (Allowed edit -> IO r) -> IO r;
            objGetInitial sair = withMVar statevar (\s -> sair (allowed s));

            objSend :: edit -> IO (Maybe (Allowed edit));
            objSend edit = modifyMVar statevar $ \olda -> return $ if allowed olda edit
                then let {newa = fromReadFunction (applyEdit edit) olda;} in (newa,Just (allowed newa))
                else (olda,Nothing);

            objClose :: IO ();
            objClose = return ();
        };
        return MkObject{..};
    });

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
    instance (FunctorOne m,Edit edita) => IsEditLens (CleanEditLens' m edita editb) where
    {
        type LensDomain (CleanEditLens' m edita editb) = edita;
        type LensRange (CleanEditLens' m edita editb) = editb;

        lensSubscribe celens = lensSubscribe (cleanEditLens celens);
    };

    instance (FunctorOne f) => IsEditLens (Lens' f a b) where
    {
        type LensDomain (Lens' f a b) = WholeEdit (WholeReader a);
        type LensRange (Lens' f a b) = WholeEdit (WholeReader b);

        lensSubscribe MkLens{..} sub (initialB :: LockAPI (WholeEdit (WholeReader b)) token -> IO (editor,token)) updateB = do
        {
            let
            {
                initialA :: LockAPI (WholeEdit (WholeReader a)) token -> IO (editor,token);
                initialA lapiA = let
                {
                    lapiB :: LockAPI (WholeEdit (WholeReader b)) token;
                    lapiB (callB :: forall m. Monad m => token -> API m (WholeEdit (WholeReader b)) token -> m r) = let
                    {
                        callA :: forall m. Monad m => token -> API m (WholeEdit (WholeReader a)) token -> m r;
                        callA token (apiA :: API m (WholeEdit (WholeReader a)) token) = let
                        {
                            readA :: Structure m (WholeReader a);
                            allowedA :: WholeEdit (WholeReader a) -> m Bool;
                            pushEditA :: WholeEdit (WholeReader a) -> m token;
                            MkAPI readA allowedA pushEditA = apiA;

                            readB :: Structure m (WholeReader b);
                            readB ReadWhole = fmap lensGet $ readA ReadWhole;

                            convertEdit :: (WholeEdit (WholeReader b)) -> m (Maybe (WholeEdit (WholeReader a)));
                            convertEdit (MkWholeEdit b) = do
                            {
                                oldA <- readA ReadWhole;
                                let
                                {
                                    fnewA :: f a;
                                    fnewA = lensPutback b oldA;

                                    mnewA :: Maybe a;
                                    mnewA = getMaybeOne fnewA;
                                };
                                return $ fmap MkWholeEdit mnewA;
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

                            pushEditB :: (WholeEdit (WholeReader b)) -> m token;
                            pushEditB editB = do
                            {
                                meditA <- convertEdit editB;
                                case meditA of
                                {
                                    Nothing -> return token; -- is this correct?
                                    Just editA -> pushEditA editA;
                                };
                            };

                            apiB :: API m (WholeEdit (WholeReader b)) token;
                            apiB = MkAPI readB allowedB pushEditB;
                        }
                        in callB token apiB;
                    } in lapiA callA;
                } in initialB lapiB;

                updateA :: editor -> token -> WholeEdit (WholeReader a) -> IO token;
                updateA editor oldtoken (MkWholeEdit a) = updateB editor oldtoken $ MkWholeEdit $ lensGet a;
            };
            sub initialA updateA;
        };
    };

    instance (FunctorOne m) => IsEditLens (Injection' m a b) where
    {
        type LensDomain (Injection' m a b) = WholeEdit (WholeReader a);
        type LensRange (Injection' m a b) = WholeEdit (WholeReader b);

        lensSubscribe inj = lensSubscribe (injectionLens inj);
    };

    instance IsEditLens (Bijection a b) where
    {
        type LensDomain (Bijection a b) = WholeEdit (WholeReader a);
        type LensRange (Bijection a b) = WholeEdit (WholeReader b);

        lensSubscribe bi = lensSubscribe (bijectionInjection bi);
    };

    instance IsEditLens (Codec a b) where
    {
        type LensDomain (Codec a b) = WholeEdit (WholeReader a);
        type LensRange (Codec a b) = WholeEdit (WholeReader (Maybe b));

        lensSubscribe codec = lensSubscribe (codecInjection codec);
    };
}
