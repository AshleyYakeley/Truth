{-# LANGUAGE ViewPatterns, FlexibleContexts #-}
module Truth.UI.GTK.Window where
{
    import Data.IORef;
    import Data.Result;
    import Data.Reity;
    import Truth.Core;
    import Graphics.UI.Gtk;
    import Truth.UI.GTK.GView;
    import Truth.UI.GTK.Maybe;
    import Truth.UI.GTK.CheckButton;
    import Truth.UI.GTK.Text;
    import Truth.UI.GTK.Tuple;
    import Truth.UI.GTK.KeyContainer;


    lastResortView :: Bool -> [FailureReason] -> KnowM (GView edit);
    lastResortView showmsgs frs = return $ MkView $ \_ _ -> do
    {
        w <- labelNew $ Just $ if showmsgs then show $ MkFailureReason "No editor" frs else "Uneditable";
        hadj <- adjustmentNew 0 0 0 0 0 0;
        vadj <- adjustmentNew 0 0 0 0 0 0;
        vp <- viewportNew hadj vadj;
        containerAdd vp w;
        let
        {
            vrWidget = toWidget vp;
            vrUpdate _ _ = return ();
            vrFirstAspectGetter = return Nothing;
        };
        return MkViewResult{..};
    };

    allKnowledge :: TypeKnowledge;
    allKnowledge = mconcat [generalTypeKnowledge,checkButtonTypeKnowledge,textTypeKnowledge,keyContainerTypeKnowledge,maybeTypeKnowledge,tupleTypeKnowledge];

    class WindowButtons actions where
    {
        addButtons :: VBox -> actions -> IO ();
    };

    instance WindowButtons () where
    {
        addButtons _ () = return ();
    };

    instance (WindowButtons a,WindowButtons b) => WindowButtons (a,b) where
    {
        addButtons vbox (a,b) = do
        {
            addButtons vbox a;
            addButtons vbox b;
        }
    };

    instance WindowButtons SaveActions where
    {
        addButtons vbox (MkSaveActions saveactions) = do
        {
            hbox <- hBoxNew False 0;
            saveButton <- makeButton "Save" $ do
            {
                mactions <- saveactions;
                _ <- case mactions of
                {
                    Just (action,_) -> action;
                    _ -> return False;
                };
                return ();
            };
            revertButton <- makeButton "Revert" $ do
            {
                mactions <- saveactions;
                _ <- case mactions of
                {
                    Just (_,action) -> action;
                    _ -> return False;
                };
                return ();
            };
            boxPackStart hbox saveButton PackNatural 0;
            boxPackStart hbox revertButton PackNatural 0;
            boxPackStart vbox hbox PackNatural 0;
        }
    };

    instance WindowButtons UndoActions where
    {
        addButtons vbox MkUndoActions{..} = do
        {
            hbox <- hBoxNew False 0;
            undoButton <- makeButton "Undo" uaUndo;
            redoButton <- makeButton "Redo" uaRedo;
            boxPackStart hbox undoButton PackNatural 0;
            boxPackStart hbox redoButton PackNatural 0;
            boxPackStart vbox hbox PackNatural 0;
        }
    };

    runKnowMAction :: TypeKnowledge -> KnowM (IO ()) -> IO ();
    runKnowMAction kw kio = case runKnowledge kw $ kio of
    {
        SuccessResult make -> make;
        FailureResult _ -> return ();
    };

    makeViewWindow :: (Edit edit,WindowButtons actions) => TypeKnowledge -> GView edit -> IORef Int -> IO () -> SubscriberW edit actions -> IO ();
    makeViewWindow kw view ref tellclose (MkSubscriberW sub) = do
    {
        MkViewSubscription{..} <- subscribeView view sub;
        window <- windowNew;
        box <- vBoxNew False 0;

        addButtons box srAction;

        selectionButton <- makeButton "Selection" $ do
        {
            msel <- srGetSelection;
            case msel of
            {
                Just kaspect -> runKnowMAction kw $ kmCatch (do
                {
                    (MkAspect tsel lens) <- kaspect;
                    addKnowledge (typeInfoKnowledge tsel) $ do
                    {
                        gview <- kmCatch (findView tsel) $ lastResortView True;
                        kw' <- getKnowledge;
                        return $ makeViewWindowCountRef kw' gview ref $ MkSubscriberW $ mapSubscriber lens sub;
                    };
                }) $ \frs -> do
                {
                    gview <- lastResortView True frs;
                    return $ makeViewWindowCountRef kw gview ref $ objectSubscriber noneObject;
                };
                Nothing -> return ();
            };
        };

        -- this is only correct if srWidget has native scroll support, such as TextView
        sw <- scrolledWindowNew Nothing Nothing;
        set sw [containerChild := srWidget];

        boxPackStart box selectionButton PackNatural 0;
        boxPackStart box sw PackGrow 0;

        set window [containerChild := box];
        widgetShow srWidget;
        _ <- onDestroy window (do
        {
            srCloser;
            tellclose;
        });
        widgetShowAll window;
    };

    makeViewWindowCountRef :: (Edit edit,WindowButtons actions) => TypeKnowledge -> GView edit -> IORef Int -> SubscriberW edit actions -> IO ();
    makeViewWindowCountRef kw view windowCount sub = do
    {
        makeViewWindow kw view windowCount (do
        {
            i <- readIORef windowCount;
            writeIORef windowCount (i - 1);
            if i == 1
             then mainQuit
             else return ();
        }) sub;
        i <- readIORef windowCount;
        writeIORef windowCount (i + 1);
    };

    makeWindowCountRef :: forall edit actions. (HasTypeInfo edit,Edit edit,WindowButtons actions) => IORef Int -> SubscriberW edit actions -> IO ();
    makeWindowCountRef windowCount sub = let
    {
        te :: TypeInfo edit;
        te = typeInfo;
    } in runKnowMAction allKnowledge $ addKnowledge (typeInfoKnowledge te) $ do
    {
        view <- kmCatch (findView te) $ lastResortView True;
        kw' <- getKnowledge;
        return $ makeViewWindowCountRef kw' view windowCount sub;
    };
}
