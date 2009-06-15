module Main where
{
	import UI.Truth.GTK;
	import Graphics.UI.Gtk;
	import Data.Changes;
	import Data.Changes.File.Linux;
	import Data.Result;
	import Data.Chain;
	import Data.IORef;
	import Data.Foldable;
	import Control.Category;
	import Control.Concurrent;
	import Prelude hiding (id,(.));

	makeWindow :: (WidgetClass w) => IORef Int -> InternalViewFactory w a -> Subscribe a -> IO (Subscribe a);
	makeWindow windowCount ivf sub = do
	{
		(sub',w,close) <- makeView ivf sub;
		window <- windowNew;
		set window [containerChild := w];
		widgetShow w;
		onDestroy window (do
		{
			close;
			i <- readIORef windowCount;
			writeIORef windowCount (i - 1);
			if i == 1
			 then mainQuit
			 else return ();
		});
		i <- readIORef windowCount;
		writeIORef windowCount (i + 1);
		widgetShow window;
		return sub';
	};

	initial :: Maybe Bool;
	initial = Just True;

	main :: IO ();
	main = withINotifyB (\inotify -> do
	{
		windowCount <- newIORef 0;
		args <- initGUI;
		timeoutAddFull (yield >> return True) priorityDefaultIdle 50;
		for_ args (\arg -> let
		{
			file = linuxFileObject inotify arg; -- WithContext FilePath (Maybe ByteString)
			content = lensSubscribe (toFloatingLens (fixedFloatingLens (cleanFixedLens contentCleanLens))) () file; -- (Maybe ByteString)
			mrtext = lensSubscribe (fixedFloatingLens (simpleFixedLens (wholeSimpleLens (cfmap (utf8Lens . packBSLens))))) () content; -- Maybe (Result ListError String)
			
			ivf = maybeIVF (SuccessResult "") (resultIVF textIVF);
		} in do
		{
			makeWindow windowCount ivf mrtext;
		});
		{-
		sub <- makeWindow windowCount (maybeIVF False (checkButtonIVF "AAAAAAAAAAAAAAAAAAAAAAAAAA")) (freeObjSubscribe initial);
		makeWindow windowCount (maybeIVF False (checkButtonIVF "BBBBBBBBBBBBBBBBBBBBBBBBBBBBB")) sub;
		makeWindow windowCount (maybeIVF True (checkButtonIVF "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC")) sub;
		-}
		mainGUI;
	});
}
