module UI.Truth.GTK.Maybe where
{
	import UI.Truth.GTK.View;
	import Graphics.UI.Gtk;
	import Data.Changes;
	import Data.IORef;

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
}
