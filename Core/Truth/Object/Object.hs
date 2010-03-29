module Truth.Object.Object where
{
    import Truth.Edit;
    import Truth.Edit.Import;

    -- | change the object.
    -- The action argument and subscription updates are mutually excluded.
    -- Returns Nothing if change is not allowed.
    -- Returns Just () if change succeeded, and updates will be received before this action returns.
    ;
    type Push push = push -> IO (Maybe ());

    data Subscription initial push update = MkSubscription
    {
        subCopy :: Subscribe' initial push update,

        -- | close the subscription
        subClose :: IO ()
    };

    -- | blocks if the object is busy
    ;
    type Subscribe' initial push update = forall r.
        (initial -> Push push -> IO r) ->
        (r -> update -> IO ()) ->
        IO (r, Subscription initial push update);
    type Subscribe edit = Subscribe' (Subject edit) edit edit;

    data Object' initial push = MkObject
    {
        objGetInitial :: forall r. (initial -> IO r) -> IO r,
        objPush :: Push push,
        objClose :: IO ()
    };
    type Object edit = Object' (Subject edit) edit;

    objSubscribe' :: forall initial push update.
     (push -> update) -> ((update -> IO ()) -> IO (Object' initial push)) -> Subscribe' initial push update;
    objSubscribe' pu getObject initialise' updater' = do
    {
        storevar <- newMVar (emptyStore :: Store (update -> IO ()));
        obj <- getObject (\edit -> withMVar storevar (\store -> forM (allStore store) (\u -> u edit) >> return ()));
        let
        {
            keyPush :: Int -> Push push;
            keyPush key push = withMVar storevar (\store -> do
            {
                mv <- objPush obj push;
                case mv of
                {
                    Just _ -> forM (allStoreExcept store key) (\u -> u (pu push)) >> return ();
                    _ -> return ();
                };
                return mv;
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

            keySubscription :: Int -> Subscription initial push update;
            keySubscription key = MkSubscription
            {
                subCopy = objSub,
                subClose = keyClose key
            };

            objSub :: Subscribe' initial push update;
            objSub initialise updater = mfix (\result -> do
            {
                key <- modifyMVar storevar (\store -> do
                {
                    let {(key,newstore) = addStore (updater (fst result)) store;};
                    return (newstore,key);
                });
                r <- objGetInitial obj (\a -> initialise a (keyPush key));
                return (r,keySubscription key);
            });
        };
        objSub initialise' updater';
    };

    objSubscribe :: forall edit. ((edit -> IO ()) -> IO (Object' (Subject edit) edit)) -> Subscribe' (Subject edit) edit edit;
    objSubscribe = objSubscribe' id;

    freeObjSubscribe :: forall edit. (Edit edit) => Subject edit -> Subscribe edit;
    freeObjSubscribe initial = objSubscribe (\_ -> do
    {
        statevar <- newMVar initial;
        return (MkObject
        {
            objGetInitial = withMVar statevar,
            objPush = \edit -> modifyMVar statevar (\a -> return (applyConstFunction (applyEdit edit) a,Just ())),
            objClose = return ()
        });
    });

    class IsEditLens lens where
    {
        type LensDomain lens;
        type LensRange lens;

        lensSubscribe :: lens -> Subscribe (LensDomain lens) -> Subscribe (LensRange lens);
    };

    instance (Edit edita,Eq state) => IsEditLens (FloatingEditLens state edita editb) where
    {
        type LensDomain (FloatingEditLens state edita editb) = edita;
        type LensRange (FloatingEditLens state edita editb) = editb;

        lensSubscribe lens subscribe = objSubscribe (\pushOut -> do
        {
            ((statevar,firsta,push),sub) <- subscribe
             (\a push -> do
            {
                statevar <- newEmptyMVar;
                return (statevar,a,push);
            })
             (\(statevar,_,_) edita -> modifyMVar_ statevar (\(oldstate,olda) -> let
            {
                (newstate,meditb) = applyConstFunction (floatingEditLensUpdate lens edita oldstate) olda;
            } in do
            {
                case meditb of
                {
                    Just editb -> pushOut editb;
                    _ -> return ();
                };
                return (newstate,applyConstFunction (applyEdit edita) olda);
            }));
            putMVar statevar (floatingEditLensInitial lens,firsta);
            return (MkObject
            {
                objGetInitial = \initialise -> withMVar statevar (\(state,a) -> initialise (floatingEditLensGet lens state a)),
                objPush = \editb -> modifyMVar statevar (\olds@(oldstate,olda) -> case applyConstFunction (floatingEditLensPutEdit lens oldstate editb) olda of
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

    instance (Edit edita) => IsEditLens (EditLens edita editb) where
    {
        type LensDomain (EditLens edita editb) = edita;
        type LensRange (EditLens edita editb) = editb;

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
                meditb = applyConstFunction (editLensUpdate lens edita) olda;
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
                objGetInitial = \initialise -> withMVar statevar (\a -> initialise (lensGet (editLensSimple lens) a)),
                objPush = \editb -> modifyMVar statevar (\olda -> case applyConstFunction (editLensPutEdit lens editb) olda of
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

    instance IsEditLens (Lens a b) where
    {
        type LensDomain (Lens a b) = WholeEdit a;
        type LensRange (Lens a b) = WholeEdit b;

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
                 case applyConstFunction (lensPutback lens newb) olda of
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

    instance IsEditLens (Injection a b) where
    {
        type LensDomain (Injection a b) = WholeEdit a;
        type LensRange (Injection a b) = WholeEdit b;

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
