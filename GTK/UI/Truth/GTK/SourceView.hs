module UI.Truth.GTK.SourceView where
{
	import Graphics.UI.Gtk hiding (Object);
	import Data.Changes;
	
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
{-
	maybeIVF :: a -> InternalViewFactory a -> InternalViewFactory (Maybe a);
	maybeIVF emptyval factory initial = do
	{
		frame <- frameNew;
		createButton <- buttonNew;
		set createButton [buttonLabel := "Create"];
		miv <- case initial of
		{
			Just a -> do
			{
				iv <- factory a;				
				set frame [containerChild := (viewWidget iv)];
				return (Just iv);
			};
			_ -> do
			{
				set frame [containerChild := createButton];
				return Nothing;
			};
		};
		stateRef <- newIORef miv;
		return (MkInternalView
		{
			ivWidget = frame,
			ivUpdate = \edit -> do
			{
				case edit of
				{
					ReplaceEdit Nothing -> do
					{
						miv <- readIORef stateRef;
						case miv of
						{
							Nothing -> return ();
							Just iv -> do
							{
								set frame [containerChild := createButton];
								widgetDestroy (ivWidget iv);
								writeIORef stateRef Nothing;
							};
						};
					};
					ReplaceEdit (Just a) -> do
					{
						miv <- readIORef stateRef;
						case miv of
						{
							Nothing -> do
							{
								iv <- factory a;				
								set frame [containerChild := (viewWidget iv)];
								writeIORef stateRef (Just iv);
								ivSetPush iv (\ioea -> push (do
								{
									ea <- ioea;
									return (FunctorOneEdit ea);
								}))
							};
							Just iv -> do
							{
								set frame [containerChild := createButton];
								widgetDestroy (ivWidget iv);
							};
						};
					};
				};
			},
			ivSetPush = \push -> do
			{
				onClicked createButton (push (return (ReplaceEdit emptyval)));
				case miv of
				{
					Just iv -> ivSetPush iv (\ioea -> push (do
					{
						ea <- ioea;
						return (FunctorOneEdit ea);
					}));
					_ -> return ();
				};
				return ();
			}
		});
	};
-}
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
