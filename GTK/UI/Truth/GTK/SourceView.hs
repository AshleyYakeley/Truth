module UI.Truth.GTK.SourceView where
{
	import Graphics.UI.Gtk hiding (Object);
	import Data.Changes;
	import Data.IORef;
	
	data View = forall w. (WidgetClass w) => MkView
	{
		viewWidget :: w,
		--viewSelection :: IO Selection,
		viewRequestClose :: IO Bool
	};

	--type ViewFactory a = Object a -> (Selection -> IO ()) -> IO View;
	type ViewFactory a = Object a -> IO View;

	data InternalView a = forall w. (WidgetClass w) => MkInternalView
	{
		ivWidget :: w,
		ivUpdate :: Edit a -> IO ()
	};

	type InternalViewFactory a = a -> Push a -> IO (InternalView a);

	ivfViewFactory :: InternalViewFactory a -> ViewFactory a;
	ivfViewFactory ivf obj = do
	{
		(view,sub) <- objSubscribe obj ivf ivUpdate;
		case view of
		{
			(MkInternalView widget _) -> do
			{
				return (MkView
				{
					viewWidget = widget,
					viewRequestClose = do
					{
						subClose sub;
						return True;
					}
				});
			};
		};
	};

	checkButtonIVF :: String -> InternalViewFactory Bool;
	checkButtonIVF name initial push = do
	{
		widget <- checkButtonNew;
		set widget [buttonLabel := name,toggleButtonActive := initial];
		onClicked widget (do
		{
			_ <- push (do
			{
				s <- get widget toggleButtonActive;
				return (ReplaceEdit s);
			});
			return ();
		});
		return (MkInternalView
		{
			ivWidget = widget,
			ivUpdate = \edit -> do
			{
				s <- get widget toggleButtonActive;
				set widget [toggleButtonActive := (applyEdit edit s)];
			}
		});
	};

	maybeIVF :: forall a. a -> InternalViewFactory a -> InternalViewFactory (Maybe a);
	maybeIVF (emptyval :: a) (factory :: InternalViewFactory a) (initial :: Maybe a) (push :: Push (Maybe a)) = 
	let
	{
		mpush :: Push a;
		mpush ioea = push (do
		{
			ea <- ioea;
			return (FunctorOneEdit ea);
		});
	} in do
	{
		frame <- frameNew;
		createButton <- buttonNew;
		set createButton [buttonLabel := "Create"];
		onClicked createButton (do
		{
			result <- push (return (ReplaceEdit (Just emptyval)));
			case result of
			{
				Just _ -> return ();	-- succeeded
				_ -> return ();	-- failed. Could possibly do something here.
			};
		});
		initialmiv :: Maybe (InternalView a) <- case initial of
		{
			Just a -> do
			{
				iv@(MkInternalView widget _) <- factory a mpush;				
				set frame [containerChild := widget];
				return (Just iv);
			};
			_ -> do
			{
				set frame [containerChild := createButton];
				return Nothing;
			};
		};
		stateRef :: IORef (Maybe (InternalView a)) <- newIORef initialmiv;
		return (MkInternalView
		{
			ivWidget = frame,
			ivUpdate = \edit -> do
			{
				miv :: Maybe (InternalView a) <- readIORef stateRef;
				newma <- applyConstFunctionA (applyEditCF edit) (do
				{
					
				}) :: IO (Maybe a) -> IO (Maybe a);
				case newma of
				{
					Nothing -> case miv of
					{
						Nothing -> return ();
						Just (MkInternalView widget _) -> do
						{
							set frame [containerChild := createButton];
							widgetDestroy widget;
							writeIORef stateRef Nothing;
						};
					};
					Just a -> case miv of
					{
						Nothing -> do
						{
							iv@(MkInternalView widget _) <- factory a mpush;				
							set frame [containerChild := widget];
							writeIORef stateRef (Just iv);
						};
						Just (MkInternalView widget _) -> do
						{
							set frame [containerChild := createButton];
							widgetDestroy widget;
						};
					};
				};
			}
		} :: InternalView (Maybe a));
	};

{-
	maybeViewFactory :: a -> ViewFactory context a -> ViewFactory context (Maybe a);
	maybeViewFactory emptyval factory objma = do
	{
		(widget,sub) <- objSubscribe objma (\a -> do
		{
			frame <- frameNew;
			createButton <- buttonNew;
			case a of
			{
				Just _ -> set frame [containerChild := w]
		})
		(\widget edit -> do
		{
		})
		;
		onClicked createButton (subPush (return (ReplaceEdit emptyval)));
		return (MkView
		{
			viewWidget = widget,
			viewRequestClose = do
			{
				subClose sub;
				return True;
			}
		});
	};
-}
	sourceViewBrowser :: ViewFactory a;
	sourceViewBrowser _ = do
	{
		button <- buttonNew;
		set button [ buttonLabel := "Push Me" ];
		onClicked button (putStrLn "Button!");
		return (MkView
		{
			viewWidget = button,
			viewRequestClose = return True
		});
	};
}
