module Truth.Core.Object.View where
{
    import Truth.Core.Import;
    import Data.IORef;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types;
    import Truth.Core.Object.Object;
    import Truth.Core.Object.Subscriber;
    import Truth.Core.Object.Lens;
    import Truth.Core.Object.Aspect;


    data ViewResult edit selstate w = MkViewResult
    {
        vrWidget :: w,
        vrUpdate :: forall m. IsStateIO m => MutableRead m (EditReader edit) -> [edit] -> m (),
        vrFirstSelState :: Maybe selstate,
        vrGetSelection :: selstate -> IO (Maybe (Aspect edit))
    };

    instance Functor (ViewResult edit selstate) where
    {
        fmap f (MkViewResult w update fss getsel) = MkViewResult (f w) update fss getsel;
    };

    data View edit w = forall selstate. MkView (Object edit -> (selstate -> IO ()) -> IO (ViewResult edit selstate w));

    instance Functor (View edit) where
    {
        fmap f (MkView view) = MkView $ \object setSelect -> do
        {
            vr <- view object setSelect;
            return $ fmap f vr;
        };
    };

    mapIOView :: (a -> IO b) -> View edit a -> View edit b;
    mapIOView amb (MkView view) = MkView $ \object setSelect -> do
    {
        vr <- view object setSelect;
        b <- amb $ vrWidget vr;
        return $ fmap (\_ -> b) vr;
    };

    mapView :: forall f w edita editb. (MonadOne f,Edit edita,Edit editb) => GeneralLens' f edita editb -> View editb w -> View edita w;
    mapView
        lens@(MkCloseFloat (flens :: FloatingEditLens' f lensstate edita editb))
        (MkView (viewB :: Object editb -> (selstate -> IO ()) -> IO (ViewResult editb selstate w)))
        = MkView $ \objectA setSelect -> do
    {
        let
        {
            MkFloatingEditLens{..} = flens;
            MkFloatingEditFunction{..} = floatingEditLensFunction;
        };
        lensvar <- newMVar floatingEditInitial;
        let
        {
            objectB :: Object editb;
            objectB = floatingMapObject (mvarStateAccess lensvar) flens objectA;
        };
        MkViewResult w updateB fss getSelB <- viewB objectB setSelect;
        let
        {
            updateA :: forall m. IsStateIO m => MutableRead m (EditReader edita) -> [edita] -> m ();
            updateA mrA editsA = mvarStateAccess lensvar $ StateT $ \oldls -> do
            {
                (newls,editsB) <- unReadable (floatingEditUpdates floatingEditLensFunction editsA oldls) mrA;
                updateB (mapMutableRead (floatingEditGet oldls) mrA) editsB;
                return ((),newls);
            };
            getSelA ss = do
            {
                mAspectA <- getSelB ss;
                case mAspectA of
                {
                    Nothing -> return Nothing;
                    Just aspectA -> return $ Just $ mapAspect (generalLens lens) aspectA;
                };
            };
        };
        return $ MkViewResult w updateA fss getSelA;
    };

    instance Applicative (View edit) where
    {
        pure vrWidget = MkView $ \_object _setSelect -> return $ let
        {
            vrUpdate _ _ = return ();
            vrFirstSelState = Nothing;
            vrGetSelection (ss :: None) = never ss;
        } in MkViewResult{..};

        (MkView view1) <*> (MkView view2) = MkView $ \object setSelect -> do
        {
            let
            {
                setSelect1 ss = setSelect $ Left ss;
                setSelect2 ss = setSelect $ Right ss;
            };
            (MkViewResult w1 update1 mfss1 getsel1) <- view1 object setSelect1;
            (MkViewResult w2 update2 mfss2 getsel2) <- view2 object setSelect2;
            let
            {
                w = w1 w2;

                update :: forall m. IsStateIO m => MutableRead m (EditReader edit) -> [edit] -> m ();
                update mr  edits = do
                {
                    update1 mr edits;
                    update2 mr edits;
                };

                mfss = case mfss1 of
                {
                    Just fss1 -> Just $ Left fss1;
                    Nothing -> fmap Right mfss2;
                };

                getsel (Left ss) = getsel1 ss;
                getsel (Right ss) = getsel2 ss;
            };
            return $ MkViewResult w update mfss getsel;
        };
    };

    data ViewSubscription edit action w = MkViewSubscription
    {
        srWidget :: w,
        srGetSelection :: IO (Maybe (Aspect edit)),
        srCloser :: IO (),
        srAction :: action
    };

    instance Functor (ViewSubscription edit action) where
    {
        fmap f (MkViewSubscription w gs cl aa) = MkViewSubscription (f w) gs cl aa;
    };

    subscribeView :: forall edit w action. View edit w -> Subscriber edit action -> IO (ViewSubscription edit action w);
    subscribeView (MkView (view :: Object edit -> (selstate -> IO ()) -> IO (ViewResult edit selstate w))) sub = do
    {
        let
        {
            initialise :: Object edit -> IO (ViewResult edit selstate w, IORef (Maybe selstate));
            initialise object = do
            {
                rec
                {
                    let
                    {
                        setSelect ss = writeIORef selref $ Just ss;
                    };
                    vr <- view object setSelect;
                    selref <- newIORef $ vrFirstSelState vr;
                };
                return (vr,selref);
            };
            receive (vr,_) = vrUpdate vr;
        };
        ((MkViewResult{..},selref),srCloser,srAction) <- sub initialise receive;
        let
        {
            srGetSelection = do
            {
                mss <- readIORef selref;
                case mss of
                {
                    Just ss -> vrGetSelection ss;
                    Nothing -> return Nothing;
                }
            };
            srWidget = vrWidget;
        };
        return MkViewSubscription{..};
    };

    tupleView :: (Applicative m,FiniteTupleSelector sel) => (forall edit. sel edit -> m (View edit w)) -> m (View (TupleEdit sel) [w]);
    tupleView pickview = getCompose $ for tupleAllSelectors $ \(MkAnyWitness sel) -> case tupleWitness (Proxy :: Proxy Edit) sel of
    {
        MkConstraintWitness -> MkCompose $ fmap (mapView (toGeneralLens $ tupleCleanEditLens sel)) (pickview sel);
    };
}
