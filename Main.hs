module Main where
{
	import qualified GnomeVFS;
	import Partial;
	import Browser;
	import Interpret;
	import Distribution.PackageDescription;
	import Graphics.UI.Gtk;
	import Graphics.UI.Gtk.SourceView;
	import qualified Data.ByteString as BS;
	import Data.Word;

	makePane :: IO ScrolledWindow;
	makePane = do
	{
		s <- sourceViewNew;
		w <- scrolledWindowNew Nothing Nothing;
		containerAdd w s;
		scrolledWindowSetPolicy w PolicyAutomatic PolicyAutomatic;
		return w;
	};

	createWindowFromBrowser :: Browser a -> IO Window;
	createWindowFromBrowser browser = do
	{
		window <- windowNew;
		onDestroy window mainQuit;
		split <- hPanedNew;
		w2 <- makePane;
		((\(MkBrowser w _ _ _) -> panedAdd1 split w) browser);
		panedAdd2 split w2;
		set window [containerChild := split];
		return window;
	};

	createBrowserFromInterpreter :: forall b. (forall a. Browser a -> IO b) -> Interpreter -> IO [Word8] -> IO b;
	createBrowserFromInterpreter withBrowser (MkInterpreter objType codec) getter = do
	{
		browser <- (pickBrowser objType) (do
		{
			bytes <- getter;
			case (decode codec bytes) of
			{
				Just a -> return a;
				_ -> fail "decode error";
			};
		}) (\_ -> return ());
		withBrowser browser;
	};

	data MIMEFile = MkMIMEFile MIMEType FilePath;
	
	myFile :: MIMEFile;
	myFile = MkMIMEFile (MkMIMEType "text" "cabal" []) "Ghide.cabal";

	main :: IO ();
	main = do
	{
		GnomeVFS.init;
		initGUI;
		let
		{
			MkMIMEFile mtype fname = myFile;
			interpreter = interpret mtype;
			getter = do
			{
				bs <- BS.readFile "Ghide.cabal";
				return (BS.unpack bs);
			}
		};
		window <- createBrowserFromInterpreter createWindowFromBrowser interpreter getter;
		widgetShowAll window;
		mainGUI;
		GnomeVFS.shutdown;
	};

}
