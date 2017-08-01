module Truth.Core.Object.View where
{
    import Truth.Core.Import;
    import Data.IORef;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types;
    import Truth.Core.Object.Object;
    import Truth.Core.Object.Subscriber;
    import Truth.Core.Object.Aspect;


    type AspectGetter edit = IO (Maybe (KnowM (Aspect edit)));

    mapAspectGetter :: (Edit edita,Edit editb) => GeneralLens edita editb -> AspectGetter editb -> AspectGetter edita;
    mapAspectGetter lens = fmap $ fmap $ fmap $ mapAspect lens;

    data ViewResult edit w = MkViewResult
    {
        vrWidget :: w,
        vrUpdate :: forall m. IsStateIO m => MutableRead m (EditReader edit) -> [edit] -> m (),
        vrFirstAspectGetter :: AspectGetter edit
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
            vrUpdate mr  edits = do
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

    data View edit w = forall. MkView (Object edit -> (AspectGetter edit -> IO ()) -> IO (ViewResult edit w));

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

    mapIOView :: (a -> IO b) -> View edit a -> View edit b;
    mapIOView amb (MkView view) = MkView $ \object setSelect -> do
    {
        vr <- view object setSelect;
        b <- amb $ vrWidget vr;
        return $ fmap (\_ -> b) vr;
    };

    mapView :: forall f w edita editb. (MonadOne f,Edit edita,Edit editb) => GeneralLens' f edita editb -> View editb w -> View edita w;
    mapView
        lens@(MkCloseState (flens :: IOEditLens' f lensstate edita editb))
        (MkView (viewB :: Object editb -> (AspectGetter editb -> IO ()) -> IO (ViewResult editb w)))
        = MkView $ \objectA setSelectA -> do
    {
        let
        {
            MkEditLens{..} = flens;
            MkEditFunction{..} = editLensFunction;
        };
        lensvar <- newMVar editInitial;
        let
        {
            objectB :: Object editb;
            objectB = mapObject (mvarStateAccess lensvar) flens objectA;

            setSelectB selB = setSelectA $ mapAspectGetter (generalLens lens) selB;
        };
        MkViewResult w updateB fssB <- viewB objectB setSelectB;
        let
        {
            updateA :: forall m. IsStateIO m => MutableRead m (EditReader edita) -> [edita] -> m ();
            updateA mrA editsA = mvarStateAccess lensvar $ StateT $ \oldls -> do
            {
                (newls,editsB) <- unReadable (editUpdates editLensFunction editsA oldls) mrA;
                updateB (mapMutableRead (editGet oldls) mrA) editsB;
                return ((),newls);
            };

            fssA = mapAspectGetter (generalLens lens) fssB;
        };
        return $ MkViewResult w updateA fssA;
    };

    data ViewSubscription edit action w = MkViewSubscription
    {
        srWidget :: w,
        srGetSelection :: AspectGetter edit,
        srCloser :: IO (),
        srAction :: action
    };

    instance Functor (ViewSubscription edit action) where
    {
        fmap f (MkViewSubscription w gs cl aa) = MkViewSubscription (f w) gs cl aa;
    };

    subscribeView :: forall edit w action. View edit w -> Subscriber edit action -> IO (ViewSubscription edit action w);
    subscribeView (MkView (view :: Object edit -> (AspectGetter edit -> IO ()) -> IO (ViewResult edit w))) sub = do
    {
        let
        {
            initialise :: Object edit -> IO (ViewResult edit w, IORef (AspectGetter edit));
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

    tupleView :: (Applicative m,FiniteTupleSelector sel) => (forall edit. sel edit -> m (View edit w)) -> m (View (TupleEdit sel) [w]);
    tupleView pickview = getCompose $ for tupleAllSelectors $ \(MkAnyWitness sel) -> case tupleWitness (Proxy :: Proxy Edit) sel of
    {
        MkConstraintWitness -> MkCompose $ fmap (mapView (toGeneralLens $ tupleEditLens @MonadIO @Maybe sel)) (pickview sel);
    };
}
