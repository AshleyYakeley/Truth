module Data.Changes.Object where
{
    import Data.Changes.FloatingLens;
    import Data.Changes.EditScheme;
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

    lensSubscribe :: forall state edita editb. (Edit edita,Eq state) => FloatingLens state edita editb -> state -> Subscribe edita -> Subscribe editb;
    lensSubscribe lens firststate subscribe = objSubscribe (\pushOut -> do
    {
        ((statevar,firsta,push),sub) <- subscribe 
         (\a push -> do
        {
            statevar <- newEmptyMVar;
            return (statevar,a,push);
        }) 
         (\(statevar,_,_) edita -> modifyMVar_ statevar (\(oldstate,olda) -> let
        {
            (newstate,meditb) = applyConstFunction (lensUpdate lens edita oldstate) olda;
        } in do
        {
            case meditb of
            {
                Just editb -> pushOut editb;
                _ -> return ();
            };
            return (newstate,applyConstFunction (applyEdit edita) olda);
        }));
        putMVar statevar (firststate,firsta);
        return (MkObject
        {
            objGetInitial = \initialise -> withMVar statevar (\(state,a) -> initialise (lensGet lens state a)),
            objPush = \editb -> modifyMVar statevar (\olds@(oldstate,olda) -> case applyConstFunction (lensPutEdit lens oldstate editb) olda of
            {
                Just edita -> do
                {
                    mv <- push edita;
                    case mv of
                    {
                        Just _ -> do
                        {
                            let {(newstate,_) = applyConstFunction (lensUpdate lens edita oldstate) olda;};
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
}
