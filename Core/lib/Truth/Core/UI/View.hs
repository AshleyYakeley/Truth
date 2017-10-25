module Truth.Core.UI.View
(
    View,
    liftIOView,
    viewObject,
    viewMutableEdit,
    viewMutableRead,
    viewSetSelectedAspect,
    mapViewEdit,

    ViewResult,
    vrWidget,
    vrUpdate,
    vrFirstAspect,
    mapViewResultEdit,

    CreateView,
    createViewReceiveUpdates,
    createViewReceiveUpdate,
    createViewAddAspect,
    mapCreateViewEdit,
    mapCreateViewAspect,

    ViewSubscription(..),
    subscribeView,
    tupleCreateView
)
where
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
        vrFirstAspect :: Aspect edit
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
            vrFirstAspect = return Nothing;
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

            vrFirstAspect = do
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
        sequenceA vrfa = fmap (\w -> MkViewResult w (vrUpdate vrfa) (vrFirstAspect vrfa)) $ vrWidget vrfa;
    };

    mapViewResultEdit :: forall edita editb w. Edit editb => GeneralLens edita editb -> ViewResult editb w -> ViewResult edita w;
    mapViewResultEdit lens@(MkCloseState flens) (MkViewResult w updateB a) = let
    {
        MkEditLens{..} = flens;
        MkEditFunction{..} = editLensFunction;

        updateA :: forall m. IsStateIO m => MutableRead m (EditReader edita) -> [edita] -> m ();
        updateA mrA editsA = editAccess $ StateT $ \oldls -> do
        {
            (newls,editsB) <- unReadable (editUpdates editLensFunction editsA oldls) mrA;
            updateB (mapMutableRead (editGet oldls) mrA) editsB;
            return ((),newls);
        };
        a' = mapAspect lens a;
    } in MkViewResult w updateA a';

    newtype View edit a = MkView (Object edit -> (Aspect edit -> IO ()) -> IO a);

    instance Functor (View edit) where
    {
        fmap f (MkView ma) = MkView $ \object setSelect -> do
        {
            a <- ma object setSelect;
            return $ f a;
        };
    };

    instance Applicative (View edit) where
    {
        pure a = MkView $ \_object _setSelect -> pure a;

        (MkView mab) <*> (MkView ma) = MkView $ \object setSelect -> do
        {
            ab <- mab object setSelect;
            a <- ma object setSelect;
            return $ ab a;
        };
    };

    instance Monad (View edit) where
    {
        return = pure;
        (MkView ma) >>= f = MkView $ \object setSelect -> do
        {
            a <- ma object setSelect;
            let {MkView mb = f a;};
            mb object setSelect;
        };
    };

    instance MonadIO (View edit) where
    {
        liftIO ioa = MkView $ \_object _setSelect -> ioa;
    };

    liftIOView :: forall edit a. ((forall r. View edit r -> IO r) -> IO a) -> View edit a;
    liftIOView call = MkView $ \object setSelect -> call $ \(MkView view) -> view object setSelect;

    viewObject :: View edit (Object edit);
    viewObject = MkView $ \object _ -> return object;

    viewMutableEdit :: (forall m. IsStateIO m => MutableEdit m edit -> m r) -> View edit r;
    viewMutableEdit call = do
    {
        object <- viewObject;
        liftIO $ runObject object call;
    };

    viewMutableRead :: (forall m. IsStateIO m => MutableRead m (EditReader edit) -> m r) -> View edit r;
    viewMutableRead call = viewMutableEdit $ \muted -> call $ mutableRead muted;

    viewSetSelectedAspect :: Aspect edit -> View edit ();
    viewSetSelectedAspect aspect = MkView $ \_ setSelect -> setSelect aspect;

    mapViewEdit :: forall edita editb a. (Edit edita,Edit editb) => GeneralLens edita editb -> View editb a -> View edita a;
    mapViewEdit lens@(MkCloseState (flens :: EditLens lensstate edita editb)) (MkView viewB) = MkView $ \objectA setSelectA -> do
    {
        let
        {
            MkEditLens{..} = flens;
            MkEditFunction{..} = editLensFunction;

            objectB :: Object editb;
            objectB = mapObject lens objectA;

            setSelectB selB = setSelectA $ mapAspect lens selB;
        };
        t <- viewB objectB setSelectB;
        return t;
    };

    type CreateView edit = Compose (View edit) (ViewResult edit);

    createViewReceiveUpdates :: (forall m. IsStateIO m => MutableRead m (EditReader edit) -> [edit] -> m ()) -> CreateView edit ();
    createViewReceiveUpdates recv = liftInner $ (pure ()) {vrUpdate = recv};

    createViewReceiveUpdate :: (forall m. IsStateIO m => MutableRead m (EditReader edit) -> edit -> m ()) -> CreateView edit ();
    createViewReceiveUpdate recv = createViewReceiveUpdates $ \mr edits -> for_ edits (recv mr);

    createViewAddAspect :: Aspect edit -> CreateView edit ();
    createViewAddAspect aspect = liftInner $ (pure ()) {vrFirstAspect = aspect};

    mapCreateViewEdit :: forall edita editb a. (Edit edita,Edit editb) => GeneralLens edita editb -> CreateView editb a -> CreateView edita a;
    mapCreateViewEdit lens (Compose wv) = Compose $ mapViewEdit lens $ fmap (mapViewResultEdit lens) wv;

    mapCreateViewAspect :: (Aspect edit -> Aspect edit) -> CreateView edit w -> CreateView edit w;
    mapCreateViewAspect f (Compose vw) = Compose $ do
    {
        MkViewResult w v ag <- vw;
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

    subscribeView :: forall edit w action. CreateView edit w -> Subscriber edit action -> IO (ViewSubscription edit action w);
    subscribeView (Compose (MkView (view :: Object edit -> (Aspect edit -> IO ()) -> IO (ViewResult edit w)))) sub = do
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
                    selref <- newIORef $ vrFirstAspect vr;
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

    tupleCreateView :: (Applicative m,FiniteTupleSelector sel,TupleWitness Edit sel) => (forall edit. sel edit -> m (CreateView edit w)) -> m (CreateView (TupleEdit sel) [w]);
    tupleCreateView pickview = getCompose $ for tupleAllSelectors $ \(MkAnyWitness sel) -> case tupleWitness (Proxy :: Proxy Edit) sel of
    {
        MkConstraintWitness -> Compose $ fmap (mapCreateViewEdit (tupleGeneralLens sel)) (pickview sel);
    };
}
