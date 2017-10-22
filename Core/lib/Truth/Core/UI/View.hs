module Truth.Core.UI.View where
{
    import Truth.Core.Import;
    import Data.IORef;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types;
    import Truth.Core.Object.MutableEdit;
    import Truth.Core.Object.Object;
    import Truth.Core.Object.Subscriber;
    import Truth.Core.UI.Specifier;
    import Truth.Core.UI.Lens;


    data ViewResult edit w = MkViewResult
    {
        vrWidget :: w,
        vrUpdate :: forall m. IsStateIO m => MutableRead m (EditReader edit) -> [edit] -> m (),
        vrFirstAspectGetter :: Aspect edit
    };

    instance Functor (ViewResult edit) where
    {
        fmap f (MkViewResult w update fss) = MkViewResult (f w) update fss;
    };

    instance Applicative (ViewResult edit) where
    {
        pure vrWidget = let
        {
            vrUpdate _ _ = return ();
            vrFirstAspectGetter = return Nothing;
        } in MkViewResult{..};

        (MkViewResult w1 update1 fss1) <*> (MkViewResult w2 update2 fss2) = let
        {
            vrWidget = w1 w2;

            vrUpdate :: forall m. IsStateIO m => MutableRead m (EditReader edit) -> [edit] -> m ();
            vrUpdate mr edits = do
            {
                update1 mr edits;
                update2 mr edits;
            };

            vrFirstAspectGetter = do
            {
                ma1 <- fss1;
                case ma1 of
                {
                    Just a -> return $ Just a;
                    Nothing -> fss2;
                };
            };
        } in MkViewResult{..};
    };

    instance Monad (ViewResult edit) where
    {
        return = pure;
        vr >>= f = vr *> (f $ vrWidget vr);
    };

    instance Foldable (ViewResult edit) where
    {
        foldMap am vr = am $ vrWidget vr;
    };

    instance Traversable (ViewResult edit) where
    {
        sequenceA vrfa = fmap (\w -> MkViewResult w (vrUpdate vrfa) (vrFirstAspectGetter vrfa)) $ vrWidget vrfa;
    };

    newtype View edit w = MkView (Object edit -> (Aspect edit -> IO ()) -> IO (ViewResult edit w));

    instance Functor (View edit) where
    {
        fmap f (MkView view) = MkView $ \object setSelect -> do
        {
            vr <- view object setSelect;
            return $ fmap f vr;
        };
    };

    instance Applicative (View edit) where
    {
        pure w = MkView $ \_object _setSelect -> return $ pure w;

        (MkView view1) <*> (MkView view2) = MkView $ \object setSelect -> do
        {
            vr1 <- view1 object setSelect;
            vr2 <- view2 object setSelect;
            return $ vr1 <*> vr2;
        };
    };

    instance Monad (View edit) where
    {
        return = pure;
        (MkView view1) >>= f = MkView $ \object setSelect -> getCompose $ do
        {
            w <- Compose $ view1 object setSelect;
            let {MkView view2 = f w;};
            Compose $ view2 object setSelect;
        };
    };

    instance MonadIO (View edit) where
    {
        liftIO iow = MkView $ \_object _setSelect -> do
        {
            w <- iow;
            return $ pure w;
        };
    };

    liftIOView :: forall edit a. ((forall r. View edit r -> IO r) -> IO a) -> View edit a;
    liftIOView call = MkView $ \object setSelect -> do
    {
        a <- call $ \(MkView view) -> fmap vrWidget $ view object setSelect;
        return $ pure a;
    };

    viewMutableEdit :: (forall m. IsStateIO m => MutableEdit m edit -> m r) -> View edit r;
    viewMutableEdit call = MkView $ \object _setSelect -> getCompose $ liftIO $ runObject object call;

    viewMutableRead :: (forall m. IsStateIO m => MutableRead m (EditReader edit) -> m r) -> View edit r;
    viewMutableRead call = viewMutableEdit $ \muted -> call $ mutableRead muted;

    viewReceiveUpdates :: (forall m. IsStateIO m => MutableRead m (EditReader edit) -> [edit] -> m ()) -> View edit ();
    viewReceiveUpdates recv = MkView $ \_ _ -> return $ (pure ()) {vrUpdate = recv};

    viewReceiveUpdate :: (forall m. IsStateIO m => MutableRead m (EditReader edit) -> edit -> m ()) -> View edit ();
    viewReceiveUpdate recv = viewReceiveUpdates $ \mr edits -> for_ edits (recv mr);

    viewAddAspect :: Aspect edit -> View edit ();
    viewAddAspect aspect = MkView $ \_ _ -> return $ (pure ()) {vrFirstAspectGetter = aspect};

    mapView :: forall w edita editb. (Edit edita,Edit editb) => GeneralLens edita editb -> View editb w -> View edita w;
    mapView
        lens@(MkCloseState (flens :: EditLens lensstate edita editb))
        (MkView (viewB :: Object editb -> (Aspect editb -> IO ()) -> IO (ViewResult editb w)))
        = MkView $ \objectA setSelectA -> do
    {
        let
        {
            MkEditLens{..} = flens;
            MkEditFunction{..} = editLensFunction;

            objectB :: Object editb;
            objectB = mapObject lens objectA;

            setSelectB selB = setSelectA $ mapAspect lens selB;
        };
        MkViewResult w updateB fssB <- viewB objectB setSelectB;
        let
        {
            updateA :: forall m. IsStateIO m => MutableRead m (EditReader edita) -> [edita] -> m ();
            updateA mrA editsA = editAccess $ StateT $ \oldls -> do
            {
                (newls,editsB) <- unReadable (editUpdates editLensFunction editsA oldls) mrA;
                updateB (mapMutableRead (editGet oldls) mrA) editsB;
                return ((),newls);
            };

            fssA = mapAspect lens fssB;
        };
        return $ MkViewResult w updateA fssA;
    };

    mapViewAspect :: (Aspect edit -> Aspect edit) -> View edit w -> View edit w;
    mapViewAspect f (MkView view) = MkView $ \object setSelect -> do
    {
        MkViewResult w v ag <- view object setSelect;
        return $ MkViewResult w v $ f ag;
    };

    data ViewSubscription edit action w = MkViewSubscription
    {
        srWidget :: w,
        srGetSelection :: Aspect edit,
        srCloser :: IO (),
        srAction :: action
    };

    instance Functor (ViewSubscription edit action) where
    {
        fmap f (MkViewSubscription w gs cl aa) = MkViewSubscription (f w) gs cl aa;
    };

    subscribeView :: forall edit w action. View edit w -> Subscriber edit action -> IO (ViewSubscription edit action w);
    subscribeView (MkView (view :: Object edit -> (Aspect edit -> IO ()) -> IO (ViewResult edit w))) sub = do
    {
        let
        {
            initialise :: Object edit -> IO (ViewResult edit w, IORef (Aspect edit));
            initialise object = do
            {
                rec
                {
                    let
                    {
                        setSelect ss = writeIORef selref ss;
                    };
                    vr <- view object setSelect;
                    selref <- newIORef $ vrFirstAspectGetter vr;
                };
                return (vr,selref);
            };
            receive (vr,_) = vrUpdate vr;
        };
        ((MkViewResult{..},selref),srCloser,srAction) <- subscribe sub initialise receive;
        let
        {
            srGetSelection = do
            {
                ss <- readIORef selref;
                ss;
            };
            srWidget = vrWidget;
        };
        return MkViewSubscription{..};
    };

    tupleView :: (Applicative m,FiniteTupleSelector sel,TupleWitness Edit sel) => (forall edit. sel edit -> m (View edit w)) -> m (View (TupleEdit sel) [w]);
    tupleView pickview = getCompose $ for tupleAllSelectors $ \(MkAnyWitness sel) -> case tupleWitness (Proxy :: Proxy Edit) sel of
    {
        MkConstraintWitness -> Compose $ fmap (mapView (tupleGeneralLens sel)) (pickview sel);
    };
}
