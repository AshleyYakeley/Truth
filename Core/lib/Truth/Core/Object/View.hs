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


    data ViewResult edit updatestate selstate w = MkViewResult
    {
        vrWidget :: w,
        vrFirstUpdateState :: updatestate,
        vrUpdate :: forall m. IsStateIO m => MutableRead m (EditReader edit) -> [edit] -> StateT updatestate m (),
        vrFirstSelState :: Maybe selstate,
        vrGetSelection :: selstate -> IO (Maybe (Aspect edit))
    };

    instance Functor (ViewResult edit updatestate selstate) where
    {
        fmap f (MkViewResult w fus update fss getsel) = MkViewResult (f w) fus update fss getsel;
    };

    data View edit w = forall updatestate selstate. MkView (Object edit updatestate -> (selstate -> IO ()) -> IO (ViewResult edit updatestate selstate w));

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

    mapView :: forall f w edita editb. MonadOne f => GeneralLens' f edita editb -> View editb w -> View edita w;
    mapView
        lens@(MkCloseFloat (flens :: FloatingEditLens' f lensstate edita editb))
        (MkView (viewB :: Object editb updatestateb -> (selstate -> IO ()) -> IO (ViewResult editb updatestateb selstate w)))
        = MkView $ \objectA setSelect -> do
    {
        let
        {
            MkFloatingEditLens{..} = flens;
            MkFloatingEditFunction{..} = floatingEditLensFunction;

            objectB :: Object editb updatestateb;
            objectB = floatingMapObject flens objectA;
        };
        MkViewResult w fusB updateB fss getSelB <- viewB objectB setSelect;
        let
        {
            fusA = (fusB,floatingEditInitial);
            updateA :: forall m. IsStateIO m => MutableRead m (EditReader edita) -> [edita] -> StateT (updatestateb,lensstate) m ();
            updateA mrA editsA = joinStateT $ swapStateT $ StateT $ \oldls -> do
            {
                (newls,editsB) <- lift $ unReadable (floatingEditUpdates floatingEditLensFunction editsA oldls) mrA;
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
        return $ MkViewResult w fusA updateA fss getSelA;
    };

    instance Applicative (View edit) where
    {
        pure vrWidget = MkView $ \_object _setSelect -> return $ let
        {
            vrFirstUpdateState = ();
            vrUpdate _ _ = return ();
            vrFirstSelState = Nothing;
            vrGetSelection (ss :: None) = never ss;
        } in MkViewResult{..};

        (MkView view1) <*> (MkView view2) = MkView $ \object setSelect -> do
        {
            let
            {
                object1 = lensObject fstLens object;
                object2 = lensObject sndLens object;
                setSelect1 ss = setSelect $ Left ss;
                setSelect2 ss = setSelect $ Right ss;
            };
            (MkViewResult w1 (fus1 :: updatestate1) update1 mfss1 getsel1) <- view1 object1 setSelect1;
            (MkViewResult w2 (fus2 :: updatestate2) update2 mfss2 getsel2) <- view2 object2 setSelect2;
            let
            {
                w = w1 w2;

                fus = (fus1,fus2);

                update :: forall m. IsStateIO m => MutableRead m (EditReader edit) -> [edit] -> StateT (updatestate1,updatestate2) m ();
                update mr  edits = do
                {
                    stateFst $ update1 mr edits;
                    stateSnd $ update2 mr edits;
                };

                mfss = case mfss1 of
                {
                    Just fss1 -> Just $ Left fss1;
                    Nothing -> fmap Right mfss2;
                };

                getsel (Left ss) = getsel1 ss;
                getsel (Right ss) = getsel2 ss;
            };
            return $ MkViewResult w fus update mfss getsel;
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
    subscribeView (MkView (view :: Object edit updatestate -> (selstate -> IO ()) -> IO (ViewResult edit updatestate selstate w))) sub = do
    {
        let
        {
            initialise :: Object edit updatestate -> IO (ViewResult edit updatestate selstate w, IORef (Maybe selstate));
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
                runObject object $ \_ acc -> acc $ put $ vrFirstUpdateState vr;
                return (vr,selref);
            };
            receive (vr,_) = vrUpdate vr;
        };
        ((MkViewResult{..},selref),srCloser,srAction) <- sub (error "uninitialised object (subscribeView)") initialise receive;
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
    tupleView pickview = getCompose $ for tupleAllSelectors $ \(MkAnyWitness sel) -> MkCompose $ fmap (mapView (toGeneralLens $ tupleCleanEditLens sel)) (pickview sel);
}
