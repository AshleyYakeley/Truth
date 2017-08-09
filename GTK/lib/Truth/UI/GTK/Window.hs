{-# LANGUAGE ViewPatterns, FlexibleContexts #-}
module Truth.UI.GTK.Window where
{
    import Prelude;
    import Data.IORef;
    import Data.Result;
    import Data.Reity;
    import Truth.Core;
    import Graphics.UI.Gtk;
    import Truth.UI.GTK.GView;
    import Truth.UI.GTK.Maybe;
    import Truth.UI.GTK.CheckButton;
    import Truth.UI.GTK.Entry;
    import Truth.UI.GTK.Text;
    import Truth.UI.GTK.Tuple;
    import Truth.UI.GTK.KeyContainer;


    lastResortView :: Bool -> FailureReasons -> KnowM (GView edit);
    lastResortView showmsgs frs = return $ MkView $ \_ _ -> do
    {
        w <- labelNew $ Just $ if showmsgs then show $ MkFailureReason "No editor" frs else "Uneditable";
        let
        {
            vrWidget = toWidget w;
            vrUpdate _ _ = return ();
            vrFirstAspectGetter = return Nothing;
        };
        return MkViewResult{..};
    };

    allKnowledge :: TypeKnowledge;
    allKnowledge = mconcat [generalTypeKnowledge,checkButtonTypeKnowledge,entryTypeKnowledge,textTypeKnowledge,keyContainerTypeKnowledge,maybeTypeKnowledge,tupleTypeKnowledge];

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

    attachMenuItem :: MenuShellClass menushell => menushell -> String -> IO MenuItem;
    attachMenuItem menu name = do
    {
        item <- menuItemNewWithLabel name;
        menuShellAppend menu item;
        return item;
    };

    attachSubmenu :: MenuItem -> IO Menu;
    attachSubmenu item = do
    {
        menu <- menuNew;
        menuItemSetSubmenu item menu;
        return menu;
    };

    menuItemAction :: MenuItem -> IO () -> IO ();
    menuItemAction item action = do
    {
        _ <- on item menuItemActivated action;
        return ();
    };

    makeViewWindow :: (Edit edit,WindowButtons actions) => TypeKnowledge -> GView edit -> IORef Int -> IO () -> String -> Subscriber edit actions -> IO ();
    makeViewWindow kw view ref tellclose title sub = do
    {
        MkViewSubscription{..} <- subscribeView view sub;
        window <- windowNew;
        set window [windowTitle := title];
        windowSetPosition window WinPosCenter;
        windowSetDefaultSize window 300 400;

        let
        {
            closeRequest :: IO Bool;
            closeRequest = do
            {
                srCloser;
                tellclose;
                return False; -- run existing handler that closes the window
            };
        };

        _ <- on window deleteEvent $ liftIO closeRequest;

        menubar <- menuBarNew;
        fileMI <- attachMenuItem menubar "File";
        fileMenu <- attachSubmenu fileMI;
        closeMI <- attachMenuItem fileMenu "Close";
        menuItemAction closeMI $ do
        {
            ok <- closeRequest;
            if ok then return () else widgetDestroy window;
        };

        box <- vBoxNew False 0;
        boxPackStart box menubar PackNatural 0;

        addButtons box srAction;

        selectionButton <- makeButton "Selection" $ do
        {
            msel <- srGetSelection;
            case msel of
            {
                Just kaspect -> runKnowMAction kw $ kmCatch (do
                {
                    (MkAspect aspname tsel lens) <- kaspect;
                    addKnowledge (typeInfoKnowledge tsel) $ do
                    {
                        gview <- kmCatch (findView tsel) $ lastResortView True;
                        kw' <- getKnowledge;
                        return $ makeViewWindowCountRef kw' gview ref (aspname ++ " of " ++ title) $ mapSubscriber lens sub;
                    };
                }) $ \frs -> do
                {
                    gview <- lastResortView True frs;
                    return $ makeViewWindowCountRef kw gview ref "unknown" $ objectSubscriber noneObject;
                };
                Nothing -> return ();
            };
        };

        -- this is only correct if srWidget has native scroll support, such as TextView
        sw <- scrolledWindowNew Nothing Nothing;
        if any (isA srWidget) [gTypeViewport,gTypeTextView] then set sw [containerChild := srWidget] else do
        {
            hadj <- adjustmentNew 0 0 0 0 0 0;
            vadj <- adjustmentNew 0 0 0 0 0 0;
            viewport <- viewportNew hadj vadj;
            containerAdd viewport srWidget;
            set sw [containerChild := viewport];
        };

        boxPackStart box selectionButton PackNatural 0;
        boxPackStart box sw PackGrow 0;

        set window [containerChild := box];
        widgetShow srWidget;
        widgetShowAll window;
    };

    makeViewWindowCountRef :: (Edit edit,WindowButtons actions) => TypeKnowledge -> GView edit -> IORef Int -> String -> Subscriber edit actions -> IO ();
    makeViewWindowCountRef kw view windowCount title sub = do
    {
        makeViewWindow kw view windowCount (do
        {
            i <- readIORef windowCount;
            writeIORef windowCount (i - 1);
            if i == 1
             then mainQuit
             else return ();
        }) title sub;
        i <- readIORef windowCount;
        writeIORef windowCount (i + 1);
    };

    makeWindowCountRef :: forall edit actions. (HasTypeInfo edit,Edit edit,WindowButtons actions) => IORef Int -> String -> Subscriber edit actions -> IO ();
    makeWindowCountRef windowCount title sub = let
    {
        te :: TypeInfo edit;
        te = typeInfo;
    } in runKnowMAction allKnowledge $ addKnowledge (typeInfoKnowledge te) $ do
    {
        view <- kmCatch (findView te) $ lastResortView True;
        kw' <- getKnowledge;
        return $ makeViewWindowCountRef kw' view windowCount title sub;
    };
}
