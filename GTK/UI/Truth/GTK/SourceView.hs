module UI.Truth.GTK.SourceView where
{
	import Graphics.UI.Gtk hiding (Object);
	import Data.Changes;
	import Data.ConstFunction;
	import Data.IORef;
	
	data View = forall w. (WidgetClass w) => MkView
	{
		viewWidget :: w,
		--viewSelection :: IO Selection,
		viewRequestClose :: IO Bool
	};

	--type ViewFactory a = Subscribe a -> (Selection -> IO ()) -> IO View;
	type ViewFactory a = Subscribe a -> IO View;

	data InternalView a = forall w. (WidgetClass w) => MkInternalView
	{
		ivWidget :: w,
		ivUpdate :: Edit a -> IO ()
	};

	type InternalViewFactory a = a -> Push a -> IO (InternalView a);

	ivfViewFactory :: InternalViewFactory a -> ViewFactory a;
	ivfViewFactory ivf subscribe = do
	{
		(view,sub) <- subscribe ivf ivUpdate;
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
				return (Just (ReplaceEdit s));
			});
			return ();
		});
		return (MkInternalView
		{
			ivWidget = widget,
			ivUpdate = \edit -> do
			{
				newstate <- applyConstFunctionA (applyEdit edit) (get widget toggleButtonActive);
				set widget [toggleButtonActive := newstate];
			}
		});
	};

	maybeIVF :: forall a. (Data.Changes.Editable a) => a -> InternalViewFactory a -> InternalViewFactory (Maybe a);
	maybeIVF (emptyval :: a) (factory :: InternalViewFactory a) (initial :: Maybe a) (push :: Push (Maybe a)) = 
	let
	{
		mpush :: Push a;
		mpush ioea = push (do
		{
			mea <- ioea;
			return (do
			{
				ea <- mea;
				return (PartEdit (JustEdit ea));
			});
		});
	} in do
	{
		frame <- frameNew;
		createButton <- buttonNew;
		set createButton [buttonLabel := "Create"];
		onClicked createButton (do
		{
			result <- push (return (Just (ReplaceEdit (Just emptyval))));
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
				case miv of
				{
					Just (MkInternalView widget update) -> case extractJustEdit edit of
					{
						Just edita -> update edita;
						Nothing -> do
						{
							set frame [containerChild := createButton];
							widgetDestroy widget;
							writeIORef stateRef Nothing;
						};
					};
					Nothing -> case edit of
					{
						ReplaceEdit (Just a) -> do
						{
							iv@(MkInternalView widget _) <- factory a mpush;				
							set frame [containerChild := widget];
							writeIORef stateRef (Just iv);
						};
						_ -> return ();
					};
				};
			}
		} :: InternalView (Maybe a));
	};

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
