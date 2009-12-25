module Data.Changes.Object where
{
    import Data.Changes.FixedEditLens;
    import Data.Changes.FloatingEditLens;
    import Data.Changes.WholeEdit;
    import Data.Changes.Edit;
    import Data.Lens;
    import Data.Injection;
    import Data.Codec;
    import Data.Bijection;
    import Data.Store;
    import Control.Concurrent.MVar;
    import Control.Monad.Fix;
    import Data.ConstFunction;
    import Data.Traversable;

    -- | change the object.
    -- The action argument and subscription updates are mutually excluded.
    -- Returns Nothing if change is not allowed.
    -- Returns Just () if change succeeded, and updates will be received before this action returns.
    ;
    type Push edit = edit -> IO (Maybe ());

    data Subscription edit = MkSubscription
    {
        subCopy :: Subscribe edit,

        -- | close the subscription
        subClose :: IO ()
    };

    -- | blocks if the object is busy
    ;
    type Subscribe edit = forall r. (Subject edit -> Push edit -> IO r) -> (r -> edit -> IO ()) -> IO (r, Subscription edit);

    data Object edit = MkObject
    {
        objGetInitial :: forall r. (Subject edit -> IO r) -> IO r,
        objPush :: Push edit,
        objClose :: IO ()
    };

    objSubscribe :: forall edit. ((edit -> IO ()) -> IO (Object edit)) -> Subscribe edit;
    objSubscribe getObject initialise' updater' = do
    {
        storevar <- newMVar emptyStore;
        obj <- getObject (\edit -> withMVar storevar (\store -> forM (allStore store) (\u -> u edit) >> return ()));
        let
        {
            keyPush :: Int -> Push edit;
            keyPush key edita = withMVar storevar (\store -> do
            {
                mv <- objPush obj edita;
                case mv of
                {
                    Just _ -> forM (allStoreExcept store key) (\u -> u edita) >> return ();
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
                r <- objGetInitial obj (\a -> initialise a (keyPush key));
                return (r,keySubscription key);
            });
        };
        objSub initialise' updater';
    };

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

    class EditLens lens where
    {
        type LensDomain lens;
        type LensRange lens;

        lensSubscribe :: lens -> Subscribe (LensDomain lens) -> Subscribe (LensRange lens);
    };

    instance (Edit edita,Eq state) => EditLens (FloatingEditLens state edita editb) where
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
                (newstate,meditb) = applyConstFunction (floatingLensUpdate lens edita oldstate) olda;
            } in do
            {
                case meditb of
                {
                    Just editb -> pushOut editb;
                    _ -> return ();
                };
                return (newstate,applyConstFunction (applyEdit edita) olda);
            }));
            putMVar statevar (floatingLensInitial lens,firsta);
            return (MkObject
            {
                objGetInitial = \initialise -> withMVar statevar (\(state,a) -> initialise (floatingLensGet lens state a)),
                objPush = \editb -> modifyMVar statevar (\olds@(oldstate,olda) -> case applyConstFunction (floatingLensPutEdit lens oldstate editb) olda of
                {
                    Just edita -> do
                    {
                        mv <- push edita;
                        case mv of
                        {
                            Just _ -> do
                            {
                                let {(newstate,_) = applyConstFunction (floatingLensUpdate lens edita oldstate) olda;};
                                return ((newstate,applyConstFunction (applyEdit edita) olda),mv);
                            };
                            _ -> return (olds,mv);
                        };
                    };
                    _ -> return (olds,Nothing);
                }),
                objClose = subClose sub
            });
        });
    };

    instance (Edit edita) => EditLens (FixedEditLens edita editb) where
    {
        type LensDomain (FixedEditLens edita editb) = edita;
        type LensRange (FixedEditLens edita editb) = editb;

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
                meditb = applyConstFunction (fixedLensUpdate lens edita) olda;
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
                objGetInitial = \initialise -> withMVar statevar (\a -> initialise (lensGet (fixedLensSimple lens) a)),
                objPush = \editb -> modifyMVar statevar (\olda -> case applyConstFunction (fixedLensPutEdit lens editb) olda of
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

    instance EditLens (Lens a b) where
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

    instance EditLens (Injection a b) where
    {
        type LensDomain (Injection a b) = WholeEdit a;
        type LensRange (Injection a b) = WholeEdit b;

        lensSubscribe inj = lensSubscribe (injectionLens inj);
    };

    instance EditLens (Bijection a b) where
    {
        type LensDomain (Bijection a b) = WholeEdit a;
        type LensRange (Bijection a b) = WholeEdit b;

        lensSubscribe bi = lensSubscribe (bijectionInjection bi);
    };

    instance EditLens (Codec a b) where
    {
        type LensDomain (Codec a b) = WholeEdit a;
        type LensRange (Codec a b) = WholeEdit (Maybe b);

        lensSubscribe codec = lensSubscribe (codecInjection codec);
    };
}
