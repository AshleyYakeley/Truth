module Truth.Object.Object where
{
    import Truth.Edit;
    import Truth.Edit.Import;

    -- | change the object.
    -- The action argument and subscription updates are mutually excluded.
    -- Returns Nothing if change is not allowed.
    -- Returns Just () if change succeeded, and updates will be received before this action returns.
    ;
    type Push edit = edit -> IO (Maybe ());

    type Allowed edit = edit -> Bool;

    data Subscription edit = MkSubscription
    {
        subCopy :: Subscribe edit,

        -- | close the subscription
        subClose :: IO ()
    };

    -- | blocks if the object is busy
    ;
    type Subscribe edit = forall r.
        (Subject edit -> Allowed edit -> Push edit -> IO r) ->
        (r -> edit -> Allowed edit -> IO ()) ->
        IO (r, Subscription edit);

    data Object edit = MkObject
    {
        objGetInitial :: forall r. (Subject edit -> Allowed edit -> IO r) -> IO r,
        objPush :: edit -> IO (Maybe (Allowed edit)),
        objClose :: IO ()
    };

    objSubscribe :: forall edit.
     ((edit -> Allowed edit -> IO ()) -> IO (Object edit)) -> Subscribe edit;
    objSubscribe getObject initialise' updater' = do
    {
        storevar <- newMVar (emptyStore :: Store (edit -> Allowed edit -> IO ()));
        obj <- getObject (\edit allowed -> withMVar storevar (\store -> forM (allStore store) (\u -> u edit allowed) >> return ()));
        let
        {
            keyPush :: Int -> Push edit;
            keyPush key push = withMVar storevar (\store -> do
            {
                mv <- objPush obj push;
                case mv of
                {
                    Just allowed -> forM (allStoreExcept store key) (\u -> u push allowed) >> return ();
                    _ -> return ();
                };
                return (mv >> (return ()));
            });

            keyClose :: Int -> IO ();
            keyClose key = modifyMVar_ storevar (\store -> let
            {
                newstore = deleteStore key store;
            } in do
            {
                if isEmptyStore newstore
                 then (objClose obj)
                 else return ();
                return newstore;
            });

            keySubscription :: Int -> Subscription edit;
            keySubscription key = MkSubscription
            {
                subCopy = objSub,
                subClose = keyClose key
            };

            objSub :: Subscribe edit;
            objSub initialise updater = mfix (\result -> do
            {
                key <- modifyMVar storevar (\store -> do
                {
                    let {(key,newstore) = addStore (updater (fst result)) store;};
                    return (newstore,key);
                });
                r <- objGetInitial obj (\a allowed -> initialise a allowed (keyPush key));
                return (r,keySubscription key);
            });
        };
        objSub initialise' updater';
    };

    freeObjSubscribe :: forall edit. (Edit edit) => Subject edit -> (Subject edit -> edit -> Bool) -> Subscribe edit;
    freeObjSubscribe initial allowed = objSubscribe (\_ -> do
    {
        statevar <- newMVar initial;
        return (MkObject
        {
            objGetInitial = \sair -> withMVar statevar (\s -> sair s (allowed s)),
            objPush = \edit -> modifyMVar statevar (\olda -> return (
                if allowed olda edit
                 then let {newa = applyConstFunction (applyEdit edit) olda;} in (newa,Just (allowed newa))
                 else (olda,Nothing)
                )),
            objClose = return ()
        });
    });

    class IsEditLens lens where
    {
        type LensDomain lens;
        type LensRange lens;

        lensSubscribe :: lens -> Subscribe (LensDomain lens) -> Subscribe (LensRange lens);
    };

    instance (FunctorOne m,Edit edita,Eq state) => IsEditLens (FloatingEditLens' m state edita editb) where
    {
        type LensDomain (FloatingEditLens' m state edita editb) = edita;
        type LensRange (FloatingEditLens' m state edita editb) = editb;

        lensSubscribe lens subscribe = objSubscribe (\pushOut -> do
        {
            ((statevar,firsta,firstalloweda,push),sub) <- subscribe
             (\a allowed push -> do
            {
                statevar :: MVar (state,Subject edita,Allowed edita) <- newEmptyMVar;
                return (statevar,a,allowed,push);
            })
             (\(statevar,_,_,_) edita newalloweda -> modifyMVar_ statevar (\(oldstate,olda,oldalloweda) -> let
            {
                (newstate,meditb) = applyConstFunction (floatingEditUpdate (floatingEditLensFunction lens) edita oldstate) olda;
                newa = applyConstFunction (applyEdit edita) olda;
                allowedb = \editb -> applyConstFunction (floatingEditLensAllowed lens newstate editb) newa;
            } in do
            {
                case meditb of
                {
                    Just editb -> pushOut editb allowedb;
                    _ -> return ();
                };
                return (newstate,newa,newalloweda);
            }));
            putMVar statevar (floatingEditInitial (floatingEditLensFunction lens),firsta,firstalloweda);
            return (MkObject
            {
                objGetInitial = \initialise -> withMVar statevar (\(state,a,alloweda) -> initialise (floatingEditGet (floatingEditLensFunction lens) state a) alloweda),
                objPush = \editb -> modifyMVar statevar
                 (\olds@(oldstate,olda,oldalloweda) -> case (applyConstFunction (do
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
                 }) olda) of
                {
                    Just (newstate,edita) -> do
                    {
                        mv <- push edita;
                        return (case mv of
                        {
                            Just _ -> (newstate,applyConstFunction (applyEdit edita) olda);
                            _ -> olds;
                        },mv);
                    };
                    _ -> return (olds,Nothing);
                }),
                objClose = subClose sub
            });
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
                meditb = applyConstFunction (editUpdate (editLensFunction lens) edita) olda;
            } in do
            {
                case meditb of
                {
                    Just editb -> pushOut editb;
                    _ -> return ();
                };
                return (applyConstFunction (applyEdit edita) olda);
            }));
            putMVar statevar firsta;
            return (MkObject
            {
                objGetInitial = \initialise -> withMVar statevar (\a -> initialise (editGet (editLensFunction lens) a)),
                objPush = \editb -> modifyMVar statevar
                 (\olda -> case getMaybeOne (applyConstFunction (editLensPutEdit lens editb) olda) of
                {
                    Just edita -> do
                    {
                        mv <- push edita;
                        return (case mv of
                        {
                            Just _ -> applyConstFunction (applyEdit edita) olda;
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
        type LensDomain (Lens' m a b) = WholeEdit a;
        type LensRange (Lens' m a b) = WholeEdit b;

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
                objPush = \(MkWholeEdit newb) -> modifyMVar statevar (\olda ->
                 case getMaybeOne (applyConstFunction (lensPutback lens newb) olda) of
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
        type LensDomain (Injection' m a b) = WholeEdit a;
        type LensRange (Injection' m a b) = WholeEdit b;

        lensSubscribe inj = lensSubscribe (injectionLens inj);
    };

    instance IsEditLens (Bijection a b) where
    {
        type LensDomain (Bijection a b) = WholeEdit a;
        type LensRange (Bijection a b) = WholeEdit b;

        lensSubscribe bi = lensSubscribe (bijectionInjection bi);
    };

    instance IsEditLens (Codec a b) where
    {
        type LensDomain (Codec a b) = WholeEdit a;
        type LensRange (Codec a b) = WholeEdit (Maybe b);

        lensSubscribe codec = lensSubscribe (codecInjection codec);
    };
}
