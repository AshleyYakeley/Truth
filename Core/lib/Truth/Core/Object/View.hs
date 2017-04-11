module Truth.Core.Object.View where
{
    import Truth.Core.Import;
    import Data.IORef;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types;
    import Truth.Core.Object.API;
    import Truth.Core.Object.Object;


    data Aspect edit where
    {
        MkAspect ::
         forall edita editb. (FullEdit editb) =>
          Info editb -> Info (EditSubject editb) -> GeneralLens edita editb -> Aspect edita;
    };

    mapAspect :: GeneralLens edita editb -> Aspect editb -> Aspect edita;
    mapAspect lens (MkAspect ie is lens') = MkAspect ie is $ lens' . lens;

    data ViewResult edit updatestate selstate w = MkViewResult
    {
        vrWidget :: w,
        vrFirstUpdateState :: updatestate,
        vrUpdate :: updatestate -> [edit] -> IO updatestate,
        vrFirstSelState :: Maybe selstate,
        vrGetSelection :: selstate -> IO (Maybe (Aspect edit))
    };

    instance Functor (ViewResult edit updatestate selstate) where
    {
        fmap f (MkViewResult w fus update fss getsel) = MkViewResult (f w) fus update fss getsel;
    };

    data View edit w = forall updatestate selstate. MkView (LockAPI edit updatestate -> (selstate -> IO ()) -> IO (ViewResult edit updatestate selstate w));

    instance Functor (View edit) where
    {
        fmap f (MkView view) = MkView $ \lapi setSelect -> do
        {
            vr <- view lapi setSelect;
            return $ fmap f vr;
        };
    };

    mapIOView :: (a -> IO b) -> View edit a -> View edit b;
    mapIOView amb (MkView view) = MkView $ \lapi setSelect -> do
    {
        vr <- view lapi setSelect;
        b <- amb $ vrWidget vr;
        return $ fmap (\_ -> b) vr;
    };

    mapView :: forall f w edita editb. MonadOne f => GeneralLens' f edita editb -> View editb w -> View edita w;
    mapView
        lens@(MkCloseFloat (flens :: FloatingEditLens' f lensstate edita editb))
        (MkView (viewB :: LockAPI editb updatestateb -> (selstate -> IO ()) -> IO (ViewResult editb updatestateb selstate w)))
        = MkView $ \lapiA setSelect -> do
    {
        let
        {
            MkFloatingEditLens{..} = flens;
            MkFloatingEditFunction{..} = floatingEditLensFunction;

            lapiB :: LockAPI editb updatestateb;
            lapiB = mapLockAPI flens lapiA;
        };
        MkViewResult w fusB updateB fss getSelB <- viewB lapiB setSelect;
        let
        {
            fusA = (fusB,floatingEditInitial);
            updateA (oldusB,oldls) editsA = do
            {
                (newls,editsB) <- return $ floatingEditUpdates floatingEditLensFunction editsA oldls;
                newusB <- updateB oldusB editsB;
                return (newusB,newls);
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
        pure vrWidget = MkView $ \_lapi _setSelect -> return $ let
        {
            vrFirstUpdateState = ();
            vrUpdate () _ = return ();
            vrFirstSelState = Nothing;
            vrGetSelection (ss :: None) = never ss;
        } in MkViewResult{..};

        (MkView view1) <*> (MkView view2) = MkView $ \lapi setSelect -> do
        {
            let
            {
                lapi1 = fmap fst lapi;
                lapi2 = fmap snd lapi;
                setSelect1 ss = setSelect $ Left ss;
                setSelect2 ss = setSelect $ Right ss;
            };
            (MkViewResult w1 fus1 update1 mfss1 getsel1) <- view1 lapi1 setSelect1;
            (MkViewResult w2 fus2 update2 mfss2 getsel2) <- view2 lapi2 setSelect2;
            let
            {
                w = w1 w2;

                fus = (fus1,fus2);
                update (olds1,olds2) edits = do
                {
                    news1 <- update1 olds1 edits;
                    news2 <- update2 olds2 edits;
                    return (news1,news2);
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

    data SubscribeResult edit w = MkSubscribeResult
    {
        srWidget :: w,
        srGetSelection :: IO (Maybe (Aspect edit)),
        srClose :: IO ()
    };

    instance Functor (SubscribeResult edit) where
    {
        fmap f (MkSubscribeResult w gs close) = MkSubscribeResult (f w) gs close;
    };

    subscribeView :: View edit w -> Object edit -> IO (SubscribeResult edit w);
    subscribeView (MkView view) object = do
    {
        let
        {
            initialise lapi = do
            {
                rec
                {
                    let
                    {
                        setSelect ss = writeIORef selref $ Just ss;
                    };
                    vr <- view lapi setSelect;
                    selref <- newIORef $ vrFirstSelState vr;
                };
                return ((vr,selref),vrFirstUpdateState vr);
            };
            receive (vr,_) = vrUpdate vr;
        };
        ((MkViewResult{..},selref),srClose) <- object initialise receive;
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
        return MkSubscribeResult{..};
    };

    mapOneWholeEditAspect :: forall f edit. (MonadOne f, Edit edit,FullReader (EditReader edit)) =>
     Info f -> Aspect edit -> Maybe (Aspect (OneWholeEdit f edit));
    mapOneWholeEditAspect infoF (MkAspect infoEditB infoSubj lens) = do
    {
        let
        {
            knowledge = mconcat [infoKnowledge infoF,infoKnowledge infoEditB,infoKnowledge infoSubj];
        };
        ValueFact (MkEditReaderInfo infoReader) <- ask knowledge $ applyInfo (info @EditReaderInfo) infoEditB;
        let
        {
            infoOneEdit = applyInfo (applyInfo (info @OneEdit) infoF) infoEditB;
            infoJustReader = applyInfo (applyInfo (info @OneReader) infoF) infoReader;

            infoEditB' = applyInfo (applyInfo (info @SumEdit) $ applyInfo (info @WholeEdit) infoJustReader) infoOneEdit;
            infoSubj' = applyInfo infoF infoSubj;
            lens' = oneWholeGeneralLens lens;
        };
        return $ MkAspect infoEditB' infoSubj' lens';
    };

    tupleView :: FiniteTupleSelector sel => (forall edit. sel edit -> View edit w) -> View (TupleEdit sel) [w];
    tupleView pickview = for tupleAllSelectors $ \(MkAnyWitness sel) -> mapView (tupleGeneralLens sel) (pickview sel);
}
