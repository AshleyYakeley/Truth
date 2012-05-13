module Truth.Object.Object where
{
    import Truth.Object.Subscribe;
    import Truth.Edit;
    import Truth.Edit.Import;

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

    freeObjSubscribe :: forall edit. (Edit edit,FullReader (EditReader edit)) => EditSubject edit -> (EditSubject edit -> edit -> Bool) -> Reference edit;
    freeObjSubscribe initial allowed = objSubscribe (\_ -> do
    {
        statevar :: MVar (EditSubject edit) <- newMVar initial;
        let
        {
            objread :: Structure IO (EditReader edit);
            objread reader = withMVar statevar (\s -> readFromM (return s) reader);

            objgetinitial :: forall r. (Allowed edit -> IO r) -> IO r;
            objgetinitial sair = withMVar statevar (\s -> sair (allowed s));

            objsend :: edit -> IO (Maybe (Allowed edit));
            objsend edit = modifyMVar statevar (\olda -> return (
                    if allowed olda edit
                     then let {newa = fromReadFunction (applyEdit edit) olda;} in (newa,Just (allowed newa))
                     else (olda,Nothing)
                    ));

            object :: EditObject edit;
            object = MkObject
            {
                objGetInitial = objgetinitial,
                objRead = MkStructureW objread,
                objSend = objsend,
                objClose = return ()
            };
        };
        return object;
    });

    cacheReferent :: forall edit. (Edit edit,FullReader (EditReader edit)) =>
        Reference edit -> Reference edit;
    cacheReferent ref = objSubscribe (\sendDown -> do
    {
        ((statevar,sendUp),sub) <- ref (\read firstallowed sendUp -> do
        {
            firsta <- readStructureW read fromReader;   -- read the whole thing in
            statevar :: MVar (EditSubject edit,Allowed edit) <- newMVar (firsta,firstallowed);
            return (statevar,sendUp);
        })
        (\(statevar,_sendUp) edit newallowed -> modifyMVar_ statevar (\(olda,oldallowed) -> let
        {
            newa = fromReadFunction (applyEdit edit) olda;  -- read from cache
        } in do
        {
            sendDown edit newallowed;
            return (newa,newallowed);
        }));
        return (MkObject
        {
            objGetInitial = \initialise -> withMVar statevar (\(_a,allowed) -> initialise allowed),
            objRead = MkStructureW (readFromM (withMVar statevar (\(a,_allowed) -> return a))),
            objSend = \edit -> modifyMVar statevar
                 (\olds@(olda,_oldallowed) -> do
                {
                    mv <- sendUp edit;
                    return (case mv of
                    {
                        Just newallowed -> (fromReadFunction (applyEdit edit) olda,newallowed);
                        _ -> olds;
                    },mv);
                }),
            objClose = subClose sub
        });
    });

    class IsEditLens lens where
    {
        type LensDomain lens;
        type LensRange lens;

        lensSubscribe :: lens -> Reference (LensDomain lens) -> Reference (LensRange lens);
    };

    instance (FunctorOne m,Edit edita,Eq state) => IsEditLens (FloatingEditLens' m state edita editb) where
    {
        type LensDomain (FloatingEditLens' m state edita editb) = edita;
        type LensRange (FloatingEditLens' m state edita editb) = editb;

        lensSubscribe lens ref = objSubscribe (\sendDown -> do
        {
            ((statevar,read,sendUp :: Send edita),sub) <- ref (\read allowed sendUp -> do
            {
                statevar :: MVar state <- newEmptyMVar;
                return (statevar,read,sendUp);
            })
             (\(statevar,read,_sendUp) edita newalloweda -> modifyMVar_ statevar (\oldstate -> do
            {
                (newstate,meditb) <- readStructureW read (floatingEditUpdate (floatingEditLensFunction lens) edita oldstate);
                let {allowedb = \editb -> readStructureW read (floatingEditLensAllowed lens newstate editb);};
                case meditb of
                {
                    Just editb -> sendDown editb (\_ -> True); -- allowedb;
                    _ -> return ();
                };
                return newstate;
            }));
            putMVar statevar (floatingEditInitial (floatingEditLensFunction lens));
            return (MkObject
            {
                objGetInitial = \initialise -> withMVar statevar
                 (\(state,alloweda) -> initialise alloweda),
                objRead = read,
                objSend = \editb -> modifyMVar statevar
                 (\oldstate -> do
                 {
                    mstateedita <- readStructureW read (do
                    {
                        medita <- floatingEditLensPutEdit lens oldstate editb;
                        case getMaybeOne medita of
                        {
                            Just edita -> do
                            {
                                (newstate,_meditb) <- floatingEditUpdate (floatingEditLensFunction lens) edita oldstate;
                                return (Just (newstate,edita));
                            };
                            _ -> return Nothing;
                        };
                    });
                    case mstateedita of
                    {
                        Just (newstate,edita) -> do
                        {
                            mv <- sendUp edita;
                            return (case mv of
                            {
                                Just alloweda -> (newstate,Just (\_ -> True){- allowedb -});
                                _ -> (oldstate,Nothing);
                            });
                        };
                        _ -> return (oldstate,Nothing);
                    };
                 }),
                objClose = subClose sub
            } :: EditObject editb);
        });
    };

    instance (FunctorOne m,Edit edita) => IsEditLens (EditLens' m edita editb) where
    {
        type LensDomain (EditLens' m edita editb) = edita;
        type LensRange (EditLens' m edita editb) = editb;

        lensSubscribe lens subscribe = objSubscribe (\pushOut -> do
        {
            ((statevar,firsta,push),sub) <- subscribe
             (\a push -> do
            {
                statevar <- newEmptyMVar;
                return (statevar,a,push);
            })
             (\(statevar,_,_) edita -> modifyMVar_ statevar (\olda -> let
            {
                meditb = fromReadFunction (editUpdate (editLensFunction lens) edita) olda;
            } in do
            {
                case meditb of
                {
                    Just editb -> pushOut editb;
                    _ -> return ();
                };
                return (fromReadFunction (applyEdit edita) olda);
            }));
            putMVar statevar firsta;
            return (MkObject
            {
                objGetInitial = \initialise -> withMVar statevar (\a -> initialise (editGet (editLensFunction lens) a)),
                objSend = \editb -> modifyMVar statevar
                 (\olda -> case getMaybeOne (fromReadFunction (editLensPutEdit lens editb) olda) of
                {
                    Just edita -> do
                    {
                        mv <- push edita;
                        return (case mv of
                        {
                            Just _ -> fromReadFunction (applyEdit edita) olda;
                            _ -> olda;
                        },mv);
                    };
                    _ -> return (olda,Nothing);
                }),
                objClose = subClose sub
            });
        });
    };

    instance (FunctorOne m,Edit edita) => IsEditLens (CleanEditLens' m edita editb) where
    {
        type LensDomain (CleanEditLens' m edita editb) = edita;
        type LensRange (CleanEditLens' m edita editb) = editb;

        lensSubscribe celens = lensSubscribe (cleanEditLens celens);
    };

    instance (FunctorOne m) => IsEditLens (Lens' m a b) where
    {
        type LensDomain (Lens' m a b) = WholeEdit (WholeReader a);
        type LensRange (Lens' m a b) = WholeEdit (WholeReader b);

        lensSubscribe lens subscribe = objSubscribe (\pushOut -> do
        {
            ((statevar,firsta,push),sub) <- subscribe
             (\a push -> do
            {
                statevar <- newEmptyMVar;
                return (statevar,a,push);
            })
             (\(statevar,_,_) (MkWholeEdit newa) -> modifyMVar_ statevar (\_ -> do
            {
                pushOut (MkWholeEdit (lensGet lens newa));
                return newa;
            }));
            putMVar statevar firsta;
            return (MkObject
            {
                objGetInitial = \initialise -> withMVar statevar (\a -> initialise (lensGet lens a)),
                objSend = \(MkWholeEdit newb) -> modifyMVar statevar (\olda ->
                 case getMaybeOne (fromReadFunction (lensPutback lens newb) olda) of
                {
                    Just newa -> do
                    {
                        mv <- push (MkWholeEdit newa);
                        return (case mv of
                        {
                            Just _ -> newa;
                            _ -> olda;
                        },mv);
                    };
                    _ -> return (olda,Nothing);
                }),
                objClose = subClose sub
            });
        });
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
