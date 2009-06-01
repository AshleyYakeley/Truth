module UI.Truth.GTK.Maybe where
{
	import UI.Truth.GTK.View;
	import Graphics.UI.Gtk;
	import Data.Changes;
	import Data.FunctorOne;
	import Data.Result;
	import Data.IORef;
	import Control.Applicative;

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

	doIf :: Maybe a -> (a -> IO b) -> IO (Maybe b);
	doIf (Just a) f = fmap Just (f a);
	doIf Nothing _ = return Nothing;

	createButton :: a -> Push a -> IO Button;
	createButton val push = pushButton push (ReplaceEdit val) "Create";

	functorOneIVF :: forall f a w. 
	(
		Applicative f,
		FunctorOne f,
		PartEdit (f a) ~ JustEdit a,
		Data.Changes.Editable a,
		WidgetClass w
	) =>
	  Maybe (forall b. f b) -> (Push (f a) -> IO w) -> InternalViewFactory a -> InternalViewFactory (f a);
	functorOneIVF mDeleteValue makeEmptywidget factory initial push = 
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
		emptyWidget <- makeEmptywidget push;
		mDeleteButton <- doIf mDeleteValue (\deleteValue -> pushButton push (ReplaceEdit deleteValue) "Delete");
		initialmiv :: Maybe (InternalView a) <- case retrieveOne initial of
		{
			SuccessResult a -> do
			{
				iv@(MkInternalView widget _) <- factory a mpush;
				doIf mDeleteButton (containerAddShow box);
				containerAddShow box widget;
				return (Just iv);
			};
			_ -> do
			{
				containerAddShow box emptyWidget;
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
							containerAddShow box emptyWidget;
							containerRemoveDestroy box widget;
							doIf mDeleteButton (containerRemove box);
							writeIORef stateRef Nothing;
						};
					};
					Nothing -> case edit of
					{
						ReplaceEdit fa | SuccessResult a <- retrieveOne fa -> do
						{
							iv@(MkInternalView widget _) <- factory a mpush;				
							doIf mDeleteButton (containerAddShow box);
							containerAddShow box widget;
							containerRemove box emptyWidget;
							writeIORef stateRef (Just iv);
						};
						_ -> return ();
					};
				};
			}
		});
	};

	maybeIVF :: (Data.Changes.Editable a) =>
	  a -> InternalViewFactory a -> InternalViewFactory (Maybe a);
	maybeIVF initialVal = functorOneIVF (Just Nothing) (createButton (Just initialVal));
	
	placeholderLabel :: IO Label;
	placeholderLabel = do
	{
		label <- labelNew (Just "Placeholder");
		return label;
	};
	
	resultIVF :: (Data.Changes.Editable a) => InternalViewFactory a -> InternalViewFactory (Result err a);
	resultIVF = functorOneIVF Nothing (\_ -> placeholderLabel);
}
