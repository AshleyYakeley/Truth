{-# LANGUAGE ViewPatterns, FlexibleContexts #-}
module Truth.UI.GTK.Window where
{
    import Data.IORef;
    import Data.Reity;
    import Truth.Core;
    import Graphics.UI.Gtk;
    import Truth.UI.GTK.GView;
    import Truth.UI.GTK.Maybe;
    import Truth.UI.GTK.CheckButton;
    import Truth.UI.GTK.Text;
    import Truth.UI.GTK.KeyContainer;


    lastResortView :: Bool -> FailureReason -> GetView Widget;
    lastResortView showmsgs fr _ = MkView $ \_ _ -> do
    {
        w <- labelNew $ Just $ if showmsgs then show fr else "Uneditable";
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

    matchViews :: TypeKnowledge;
    matchViews = mconcat [checkButtonTypeKnowledge,textTypeKnowledge,keyContainerTypeKnowledge,maybeTypeKnowledge];

    getView :: GetView Widget;
    getView = finalGetView matchViews $ lastResortView True;

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

    makeViewWindow :: (Edit edit,WindowButtons actions) => GView edit -> IORef Int -> IO () -> Subscriber edit actions -> IO ();
    makeViewWindow view ref tellclose sub = do
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
                Just (MkAspect tsel _ta lens) -> do
                {
                    makeLookupWindowCountRef tsel ref (mapSubscriber lens sub);
                };
                _ -> return ();
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

    makeViewWindowCountRef :: (Edit edit,WindowButtons actions) => GView edit -> IORef Int -> Subscriber edit actions -> IO ();
    makeViewWindowCountRef view windowCount sub = do
    {
        makeViewWindow view windowCount (do
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

    makeKnownWindowCountRef :: (Edit edit,HasView Widget edit,WindowButtons actions) => IORef Int -> Subscriber edit actions -> IO ();
    makeKnownWindowCountRef = makeViewWindowCountRef theView;

    makeLookupWindowCountRef :: (Edit edit,WindowButtons actions) => Info edit -> IORef Int -> Subscriber edit actions -> IO ();
    makeLookupWindowCountRef te = makeViewWindowCountRef (getView te);
}
