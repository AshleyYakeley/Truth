module Truth.Core.Object.Lens where
{
    import Truth.Core.Import;
    import Truth.Core.Edit;
    import Truth.Core.Types;
    import Truth.Core.Read;
    import Truth.Core.Object.Object;
    import Truth.Core.Object.Subscription;


    mapSubscription :: forall f edita editb action. (MonadOne f,Edit edita) => GeneralLens' f edita editb -> Subscription edita action -> Subscription editb action;
    mapSubscription (MkCloseFloat (lens@MkFloatingEditLens{..} :: FloatingEditLens' f lensstate edita editb)) sub firstB (initialB :: Object editb userstate -> IO (editor,userstate)) updateB = let
    {
        MkFloatingEditFunction{..} = floatingEditLensFunction;

        firstA :: (userstate,lensstate);
        firstA = (firstB,floatingEditInitial);

        initialA :: Object edita (userstate,lensstate) -> IO ((Object edita (userstate,lensstate),editor),(userstate,lensstate));
        initialA objectA = do
        {
            (ed,us) <- initialB $ floatingMapObject lens objectA;
            return ((objectA,ed),(us,floatingEditInitial));
        };

        updateA :: forall m. IsStateIO m => (Object edita (userstate,lensstate),editor) -> MutableRead m (EditReader edita) -> [edita] -> StateT (userstate,lensstate) m ();
        updateA (_objectA,editor) mr editAs = joinStateT $ swapStateT $ StateT $ \oldls -> do
        {
            (newls,editBs) <- lift $ unReadable (floatingEditUpdates floatingEditLensFunction editAs oldls) mr;
            updateB editor (mapMutableRead (floatingEditGet oldls) mr) editBs;
            return ((),newls);
        };
    } in do
    {
        ((_,editor),action) <- sub firstA initialA updateA;
        return (editor,action);
    };


    class IsGeneralLens lens where
    {
        type LensMonad lens :: * -> *;
        type LensDomain lens :: *;
        type LensRange lens :: *;

        toGeneralLens' :: lens -> GeneralLens' (LensMonad lens) (LensDomain lens) (LensRange lens);
    };

    toGeneralLens :: (IsGeneralLens lens,MonadOne (LensMonad lens)) => lens -> GeneralLens (LensDomain lens) (LensRange lens);
    toGeneralLens = generalLens . toGeneralLens';

    instance IsGeneralLens (GeneralLens' m edita editb) where
    {
        type LensMonad (GeneralLens' m edita editb) = m;
        type LensDomain (GeneralLens' m edita editb) = edita;
        type LensRange (GeneralLens' m edita editb) = editb;

        toGeneralLens' = id;
    };

    instance Eq state => IsGeneralLens (FloatingEditLens' m state edita editb) where
    {
        type LensMonad (FloatingEditLens' m state edita editb) = m;
        type LensDomain (FloatingEditLens' m state edita editb) = edita;
        type LensRange (FloatingEditLens' m state edita editb) = editb;

        toGeneralLens' = MkCloseFloat;
    };

    instance Functor m => IsGeneralLens (EditLens' m edita editb) where
    {
        type LensMonad (EditLens' m edita editb) = m;
        type LensDomain (EditLens' m edita editb) = edita;
        type LensRange (EditLens' m edita editb) = editb;

        toGeneralLens' = toGeneralLens' . fixedFloatingEditLens;
    };

    instance Functor m => IsGeneralLens (CleanEditLens' m edita editb) where
    {
        type LensMonad (CleanEditLens' m edita editb) = m;
        type LensDomain (CleanEditLens' m edita editb) = edita;
        type LensRange (CleanEditLens' m edita editb) = editb;

        toGeneralLens' = toGeneralLens' . cleanEditLens;
    };

    instance (MonadOne f) => IsGeneralLens (Lens' f a b) where
    {
        type LensMonad (Lens' f a b) = f;
        type LensDomain (Lens' f a b) = WholeEdit a;
        type LensRange (Lens' f a b) = WholeEdit b;

        toGeneralLens' = toGeneralLens' . wholeEditLens;
    };

    instance (MonadOne m) => IsGeneralLens (Injection' m a b) where
    {
        type LensMonad (Injection' m a b) = m;
        type LensDomain (Injection' m a b) = WholeEdit a;
        type LensRange (Injection' m a b) = WholeEdit b;

        toGeneralLens' = toGeneralLens' . injectionLens;
    };

    instance IsGeneralLens (Bijection a b) where
    {
        type LensMonad (Bijection a b) = Identity;
        type LensDomain (Bijection a b) = WholeEdit a;
        type LensRange (Bijection a b) = WholeEdit b;

        toGeneralLens' = toGeneralLens' . bijectionInjection;
    };

    instance IsGeneralLens (Codec a b) where
    {
        type LensMonad (Codec a b) = Maybe;
        type LensDomain (Codec a b) = WholeEdit a;
        type LensRange (Codec a b) = WholeEdit (Maybe b);

        toGeneralLens' = toGeneralLens' . codecInjection;
    };

    convertSubscription :: forall edita editb actions. (EditSubject edita ~ EditSubject editb,FullEdit edita,FullEdit editb) => Subscription edita actions -> Subscription editb actions;
    convertSubscription = mapSubscription $ toGeneralLens' (convertEditLens :: EditLens' Identity edita editb);

{-

    cacheReferent :: forall edit. (Edit edit,FullReader (EditReader edit)) =>
        Reference edit -> Reference edit;
    cacheReferent ref = objSubscribe (\sendDown -> do
    {
        let
        {
            initref read firstallowed sendUp = do
            {
                firsta <- readStructureW read fromReader;   -- read the whole thing in
                statevar :: MVar (EditSubject edit,Allowed edit) <- newMVar (firsta,firstallowed);
                return (statevar,sendUp);
            };

            receive (statevar,_sendUp) edit newallowed = modifyMVar_ statevar $ \(olda,oldallowed) -> let
            {
                newa = fromReadFunction (applyEdit edit) olda;  -- read from cache
            } in do
            {
                sendDown edit newallowed;
                return (newa,newallowed);
            };
        };
        ((statevar,sendUp),sub) <- ref initref receive;
        let
        {
            objGetInitial :: forall r. (Allowed edit -> IO r) -> IO r;
            objGetInitial initialise = withMVar statevar $ \(_a,allowed) -> initialise allowed;

            objRead :: StructureW IO (EditReader edit);
            objRead = MkStructureW $ readFromM $ withMVar statevar $ \(a,_allowed) -> return a;

            objSend :: edit -> IO (Maybe (Allowed edit));
            objSend edit = modifyMVar statevar $ \olds@(olda,_oldallowed) -> do
            {
                mv <- sendUp edit;
                return (case mv of
                {
                    Just newallowed -> (fromReadFunction (applyEdit edit) olda,newallowed);
                    _ -> olds;
                },mv);
            };

            objClose :: IO ();
            objClose = subClose sub;
        };
        return MkObject{..};
    });
-}

{-
    -- | Not sure if this should be used.
    pairSubscription :: forall ea eb. Subscription ea -> Subscription eb -> Subscription (PairEdit ea eb);
    pairSubscription objA objB (initialise :: Object (PairEdit ea eb) userstate -> IO (editor,userstate)) receive = do
    {
        let
        {
            initialiseA objectA = do
            {
                let
                {
                    initialiseB lapiB = do
                    {
                        (ed,ustate) <- initialise (fmap snd $ pairObject objectA lapiB); -- "snd" here is very dubious
                        return ((ed,ustate),ustate);
                    };
                    receiveB :: forall m. IsStateIO m => (editor,userstate) -> MutableRead m (EditReader eb) -> userstate -> [eb] -> m userstate;
                    receiveB (ed,_) mr oldstate ebs = receive ed (pairMutableRead f1 mr) oldstate $ fmap (MkTupleEdit EditSecond) ebs;
                };
                ((ed,ustate),closeB) <- objB initialiseB receiveB;
                return ((ed,closeB),ustate);
            };
            receiveA :: forall m. IsStateIO m => (editor,IO ()) -> MutableRead m (EditReader ea) -> userstate -> [ea] -> m userstate;
            receiveA (ed,_) mr oldstate eas = receive ed (pairMutableRead mr f2) oldstate $ fmap (MkTupleEdit EditFirst) eas;
        };
        ((ed,closeB),closeA) <- objA initialiseA receiveA;
        return $ (ed,closeB >> closeA);
    };
-}
}
