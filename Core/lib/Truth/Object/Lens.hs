module Truth.Object.Lens where
{
    import Truth.Edit.Import;
    import Truth.Object.Object;
    import Truth.Edit;


    class ObjectLens lens where
    {
        type LensDomain lens;
        type LensRange lens;

        lensObject :: lens -> Object (LensDomain lens) -> Object (LensRange lens);
    };

    instance (FunctorOne f,Edit edita) => ObjectLens (FloatingEditLens' f state edita editb) where
    {
        type LensDomain (FloatingEditLens' f state edita editb) = edita;
        type LensRange (FloatingEditLens' f state edita editb) = editb;

        lensObject lens@MkFloatingEditLens{..} sub (initialB :: LockAPI editb token -> IO (editor,token)) updateB = do
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
                    lapiB (callB :: token -> API IO editb token -> IO r) = withMVar statevar $ \state -> let
                    {
                        callA :: token -> API IO edita token -> IO r;
                        callA token (apiA :: API IO edita token) = let
                        {
                            readA :: Structure IO (EditReader edita);
                            allowedA :: edita -> IO Bool;
                            pushEditA :: edita -> IO token;
                            MkAPI readA allowedA pushEditA = apiA;

                            readB :: Structure IO (EditReader editb);
                            readB = mapStructure (floatingEditGet state) readA;

                            convertEdit :: editb -> IO (Maybe edita);
                            convertEdit editB = do
                            {
                                feditA :: f editA <- unReadable (floatingEditLensPutEdit state editB) readA;
                                return $ getMaybeOne feditA;
                            };

                            allowedB :: editb -> IO Bool;
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

                            pushEditB :: editb -> IO token;
                            pushEditB editB = do
                            {
                                meditA <- convertEdit editB;
                                case meditA of
                                {
                                    Nothing -> return token; -- is this correct?
                                    Just editA -> pushEditA editA;
                                };
                            };

                            apiB :: API IO editb token;
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

    instance (FunctorOne f,Edit edita) => ObjectLens (EditLens' f edita editb) where
    {
        type LensDomain (EditLens' f edita editb) = edita;
        type LensRange (EditLens' f edita editb) = editb;

        lensObject lens@MkEditLens{..} sub (initialB :: LockAPI editb token -> IO (editor,token)) updateB = do
        {
            let
            {
                MkEditFunction{..} = editLensFunction;

                initialA :: LockAPI edita token -> IO (editor,token);
                initialA lapiA = let
                {
                    lapiB :: LockAPI editb token;
                    lapiB (callB :: token -> API IO editb token -> IO r) = let
                    {
                        callA :: token -> API IO edita token -> IO r;
                        callA token (apiA :: API IO edita token) = let
                        {
                            readA :: Structure IO (EditReader edita);
                            allowedA :: edita -> IO Bool;
                            pushEditA :: edita -> IO token;
                            MkAPI readA allowedA pushEditA = apiA;

                            readB :: Structure IO (EditReader editb);
                            readB = mapStructure editGet readA;

                            convertEdit :: editb -> IO (Maybe edita);
                            convertEdit editB = do
                            {
                                feditA :: f editA <- unReadable (editLensPutEdit editB) readA;
                                return $ getMaybeOne feditA;
                            };

                            allowedB :: editb -> IO Bool;
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

                            pushEditB :: editb -> IO token;
                            pushEditB editB = do
                            {
                                meditA <- convertEdit editB;
                                case meditA of
                                {
                                    Nothing -> return token; -- is this correct?
                                    Just editA -> pushEditA editA;
                                };
                            };

                            apiB :: API IO editb token;
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
    instance (FunctorOne m,Edit edita) => ObjectLens (CleanEditLens' m edita editb) where
    {
        type LensDomain (CleanEditLens' m edita editb) = edita;
        type LensRange (CleanEditLens' m edita editb) = editb;

        lensObject celens = lensObject (cleanEditLens celens);
    };

    instance (FunctorOne f) => ObjectLens (Lens' f a b) where
    {
        type LensDomain (Lens' f a b) = WholeEdit (WholeReader a);
        type LensRange (Lens' f a b) = WholeEdit (WholeReader b);

        lensObject MkLens{..} sub (initialB :: LockAPI (WholeEdit (WholeReader b)) token -> IO (editor,token)) updateB = do
        {
            let
            {
                initialA :: LockAPI (WholeEdit (WholeReader a)) token -> IO (editor,token);
                initialA lapiA = let
                {
                    lapiB :: LockAPI (WholeEdit (WholeReader b)) token;
                    lapiB (callB :: token -> API IO (WholeEdit (WholeReader b)) token -> IO r) = let
                    {
                        callA :: token -> API IO (WholeEdit (WholeReader a)) token -> IO r;
                        callA token (apiA :: API IO (WholeEdit (WholeReader a)) token) = let
                        {
                            readA :: Structure IO (WholeReader a);
                            allowedA :: WholeEdit (WholeReader a) -> IO Bool;
                            pushEditA :: WholeEdit (WholeReader a) -> IO token;
                            MkAPI readA allowedA pushEditA = apiA;

                            readB :: Structure IO (WholeReader b);
                            readB ReadWhole = fmap lensGet $ readA ReadWhole;

                            convertEdit :: (WholeEdit (WholeReader b)) -> IO (Maybe (WholeEdit (WholeReader a)));
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

                            allowedB :: (WholeEdit (WholeReader b)) -> IO Bool;
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

                            pushEditB :: (WholeEdit (WholeReader b)) -> IO token;
                            pushEditB editB = do
                            {
                                meditA <- convertEdit editB;
                                case meditA of
                                {
                                    Nothing -> return token; -- is this correct?
                                    Just editA -> pushEditA editA;
                                };
                            };

                            apiB :: API IO (WholeEdit (WholeReader b)) token;
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

    instance (FunctorOne m) => ObjectLens (Injection' m a b) where
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
