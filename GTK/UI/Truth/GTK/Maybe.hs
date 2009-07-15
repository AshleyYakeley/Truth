{-# OPTIONS -fno-warn-orphans #-}
module UI.Truth.GTK.Maybe () where
{
	import UI.Truth.GTK.GView;
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
			result <- push edit;
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

	functorOneIVF :: forall f a wd. 
	(
		Applicative f,
		FunctorOne f,
		PartEdit (f a) ~ JustEdit a,
		Data.Changes.Editable a,
		WidgetClass wd
	) =>
	  Maybe (forall b. f b) -> (Push (f a) -> IO wd) -> GView a -> GView (f a);
	functorOneIVF mDeleteValue makeEmptywidget factory initial push = 
	let
	{
		mpush :: Push a;
		mpush ea = push (PartEdit (JustEdit ea));
	} in do
	{
		box <- vBoxNew True 0;
		emptyWidget <- makeEmptywidget push;
		mDeleteButton <- doIf mDeleteValue (\deleteValue -> pushButton push (ReplaceEdit deleteValue) "Delete");
		initialmiv :: Maybe (GViewResult a) <- case retrieveOne initial of
		{
			SuccessResult a -> do
			{
				iv@(MkViewResult widget _) <- factory a mpush;
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
		stateRef :: IORef (Maybe (GViewResult a)) <- newIORef initialmiv;
		return (MkViewResult
		{
			vrWidget = toWidget box,
			vrUpdate = \edit -> do
			{
				miv :: Maybe (GViewResult a) <- readIORef stateRef;
				case miv of
				{
					Just (MkViewResult widget update) -> case extractJustEdit edit of
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
							iv@(MkViewResult widget _) <- factory a mpush;				
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

	maybeView :: (Data.Changes.Editable a) =>
	  a -> GView a -> GView (Maybe a);
	maybeView initialVal = functorOneIVF (Just Nothing) (createButton (Just initialVal));
	
	placeholderLabel :: IO Label;
	placeholderLabel = do
	{
		label <- labelNew (Just "Placeholder");
		return label;
	};
	
	resultView :: (Data.Changes.Editable a) => GView a -> GView (Result err a);
	resultView = functorOneIVF Nothing (\_ -> placeholderLabel);
	
	instance (HasGView a) => HasGView (Result err a) where
	{
		gView = resultView gView;
	};

	instance (HasNewValue a,HasGView a) => HasGView (Maybe a) where
	{
		gView = maybeView newValue gView;
	};
}
