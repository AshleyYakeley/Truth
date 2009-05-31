module UI.Truth.GTK.Maybe where
{
	import UI.Truth.GTK.View;
	import Graphics.UI.Gtk;
	import Data.Changes;
	import Data.IORef;

	pushButton :: Push a -> Edit a -> String -> IO Button;
	pushButton push edit name = do
	{
		button <- buttonNew;
		set button [buttonLabel := name];
		onClicked button (do
		{
			result <- push (return (Just edit));
			case result of
			{
				Just _ -> return ();	-- succeeded
				_ -> return ();	-- failed. Could possibly do something here.
			};
		});
		return button;
	};

	containerAddShow :: (ContainerClass w1, WidgetClass w2) => w1 -> w2 -> IO ();
	containerAddShow w1 w2 = do
	{
		containerAdd w1 w2;
		widgetShow w2;
	};

	containerRemoveDestroy :: (ContainerClass w1, WidgetClass w2) => w1 -> w2 -> IO ();
	containerRemoveDestroy w1 w2 = do
	{
		containerRemove w1 w2;
		widgetDestroy w2;
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
		box <- vBoxNew True 0;
		createButton <- pushButton push (ReplaceEdit (Just emptyval)) "Create";
		deleteButton <- pushButton push (ReplaceEdit Nothing) "Delete";
		initialmiv :: Maybe (InternalView a) <- case initial of
		{
			Just a -> do
			{
				iv@(MkInternalView widget _) <- factory a mpush;
				containerAddShow box deleteButton;
				containerAddShow box widget;
				return (Just iv);
			};
			_ -> do
			{
				containerAddShow box createButton;
				return Nothing;
			};
		};
		stateRef :: IORef (Maybe (InternalView a)) <- newIORef initialmiv;
		return (MkInternalView
		{
			ivWidget = box,
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
							containerAddShow box createButton;
							containerRemoveDestroy box widget;
							containerRemove box deleteButton;
							writeIORef stateRef Nothing;
						};
					};
					Nothing -> case edit of
					{
						ReplaceEdit (Just a) -> do
						{
							iv@(MkInternalView widget _) <- factory a mpush;				
							containerAddShow box deleteButton;
							containerAddShow box widget;
							containerRemove box createButton;
							writeIORef stateRef (Just iv);
						};
						_ -> return ();
					};
				};
			}
		} :: InternalView (Maybe a));
	};
}
