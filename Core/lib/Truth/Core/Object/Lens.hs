module Truth.Core.Object.Lens where
{
    import Truth.Core.Import;
    import Truth.Core.Edit;
    import Truth.Core.Types;
    import Truth.Core.Read;
    import Truth.Core.Object.LockAPI;
    import Truth.Core.Object.Object;


    lensObject :: forall f edita editb. (MonadOne f,Edit edita) => GeneralLens' f edita editb -> Object edita -> Object editb;
    lensObject (MkCloseFloat (lens@MkFloatingEditLens{..} :: FloatingEditLens' f lensstate edita editb)) sub (initialB :: LockAPI editb userstate -> IO (editor,userstate)) updateB = let
    {
        MkFloatingEditFunction{..} = floatingEditLensFunction;

        initialA :: LockAPI edita (userstate,lensstate) -> IO ((LockAPI edita (userstate,lensstate),editor),(userstate,lensstate));
        initialA lapiA = do
        {
            (ed,us) <- initialB $ mapLockAPI lens lapiA;
            return ((lapiA,ed),(us,floatingEditInitial));
        };

        updateA :: forall m. MonadIOInvert m => (LockAPI edita (userstate,lensstate),editor) -> MutableRead m (EditReader edita) -> (userstate,lensstate) -> [edita] -> m (userstate,lensstate);
        updateA (_lapiA,editor) mr (oldus,oldls) editAs = do
        {
            (newls,editBs) <- unReadable (floatingEditUpdates floatingEditLensFunction editAs oldls) mr;
            newus <- updateB editor (mapMutableRead (floatingEditGet oldls) mr) oldus editBs;
            return (newus,newls);
        };
    } in do
    {
        ((_,editor),closer) <- sub initialA updateA;
        return (editor,closer);
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
        type LensDomain (Lens' f a b) = WholeEdit (WholeReader a);
        type LensRange (Lens' f a b) = WholeEdit (WholeReader b);

        toGeneralLens' = toGeneralLens' . wholeEditLens;
    };

    instance (MonadOne m) => IsGeneralLens (Injection' m a b) where
    {
        type LensMonad (Injection' m a b) = m;
        type LensDomain (Injection' m a b) = WholeEdit (WholeReader a);
        type LensRange (Injection' m a b) = WholeEdit (WholeReader b);

        toGeneralLens' = toGeneralLens' . injectionLens;
    };

    instance IsGeneralLens (Bijection a b) where
    {
        type LensMonad (Bijection a b) = Identity;
        type LensDomain (Bijection a b) = WholeEdit (WholeReader a);
        type LensRange (Bijection a b) = WholeEdit (WholeReader b);

        toGeneralLens' = toGeneralLens' . bijectionInjection;
    };

    instance IsGeneralLens (Codec a b) where
    {
        type LensMonad (Codec a b) = Maybe;
        type LensDomain (Codec a b) = WholeEdit (WholeReader a);
        type LensRange (Codec a b) = WholeEdit (WholeReader (Maybe b));

        toGeneralLens' = toGeneralLens' . codecInjection;
    };

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
    pairObject :: forall ea eb. Object ea -> Object eb -> Object (PairEdit ea eb);
    pairObject objA objB (initialise :: LockAPI (PairEdit ea eb) userstate -> IO (editor,userstate)) receive = do
    {
        let
        {
            initialiseA lapiA = do
            {
                let
                {
                    initialiseB lapiB = do
                    {
                        (ed,ustate) <- initialise (fmap snd $ pairLockAPI lapiA lapiB); -- "snd" here is very dubious
                        return ((ed,ustate),ustate);
                    };
                    receiveB :: forall m. MonadIOInvert m => (editor,userstate) -> MutableRead m (EditReader eb) -> userstate -> [eb] -> m userstate;
                    receiveB (ed,_) mr oldstate ebs = receive ed (pairMutableRead f1 mr) oldstate $ fmap (MkTupleEdit EditSecond) ebs;
                };
                ((ed,ustate),closeB) <- objB initialiseB receiveB;
                return ((ed,closeB),ustate);
            };
            receiveA :: forall m. MonadIOInvert m => (editor,IO ()) -> MutableRead m (EditReader ea) -> userstate -> [ea] -> m userstate;
            receiveA (ed,_) mr oldstate eas = receive ed (pairMutableRead mr f2) oldstate $ fmap (MkTupleEdit EditFirst) eas;
        };
        ((ed,closeB),closeA) <- objA initialiseA receiveA;
        return $ (ed,closeB >> closeA);
    };
-}
}
