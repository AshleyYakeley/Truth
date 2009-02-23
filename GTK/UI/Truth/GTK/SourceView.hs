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

	--type ViewFactory context a = Object context a -> (Selection -> IO ()) -> IO View;
	type ViewFactory context a = Object context a -> IO View;

	data InternalViewFactory a = forall w. (WidgetClass w) => MkInternalViewFactory
	{
		ivNew :: a -> IO w,
		ivUpdate :: w -> Edit a -> IO (),
		ivSetPush :: w -> (IO (Edit a) -> IO (Maybe ())) -> IO ()
	};

	ivfViewFactory :: InternalViewFactory a -> ViewFactory context a;
	ivfViewFactory (MkInternalViewFactory new update setpush) obj = do
	{
		(widget,sub) <- objSubscribe obj new update;
		setpush widget (subPush sub);
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

	checkButtonIVF :: String -> InternalViewFactory Bool;
	checkButtonIVF name = MkInternalViewFactory
	{
		ivNew = \s -> do
		{
			widget <- checkButtonNew;
			set widget [buttonLabel := name,toggleButtonActive := s];
			return widget;
		},
		ivUpdate = \widget edit -> do
		{
			s <- get widget toggleButtonActive;
			set widget [toggleButtonActive := (applyEdit edit s)];
		},
		ivSetPush = \widget push -> do
		{
			_ <- push (do
			{
				s <- get widget toggleButtonActive;
				return (ReplaceEdit s);
			});
			return ();
		}
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
	sourceViewBrowser :: ViewFactory context a;
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
