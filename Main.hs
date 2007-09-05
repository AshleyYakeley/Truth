module Main where
{
--	import Partial;
	import MIME;
	import Browser;
	import Object;
	import Interpret;
	import Data.Witness;
	import System.Gnome.VFS;
	import Graphics.UI.Gtk hiding (Object);
	import qualified Data.ByteString as BS;
	import Data.Maybe;
	import Data.Word;

	createPanedWindow :: IO (Window,HPaned);
	createPanedWindow = do
	{
		window <- windowNew;
		onDestroy window mainQuit;
		split <- hPanedNew;
		set window [containerChild := split];
		return (window,split);
	};

	browserAddToPane1 :: HPaned -> Browser a -> IO ();
	browserAddToPane1 split browser = (\(MkBrowser w _ _) -> panedAdd1 split w) browser;

	showObjects :: [AnyObject] -> IO ();
	showObjects [] = return ();
	showObjects (MkAnyF t obj:objs) = do
	{
		putStrLn ("select "++(uriToString (objContext obj) URIHideNone) ++ " (" ++ (show t)++")");
		showObjects objs;
	};
	
	onSelPane2 :: HPaned -> Selection -> IO ();
	onSelPane2 split sel = do
	{
		showObjects sel;
		case sel of
		{
			[s1] -> pickObjBrowser s1 (\_ -> return ()) (\(MkBrowser w _ _) -> do
			{
				panedAdd2 split w;
				widgetShowAll w;
			});
			_ -> return ();
		};
	};

	data MIMEFile = MkMIMEFile MIME.MIMEType FilePath;

	mySubject :: URI;
	mySubject = (fromMaybe undefined (uriFromString "file:///home/ashley/Projects/Ghide/Ghide.cabal"));
	
	myFile :: MIMEFile;
	myFile = MkMIMEFile (MkMIMEType "text" "cabal" []) "Ghide.cabal";

	myContext :: URI;
	myContext = (fromMaybe undefined (uriFromString "file:///home/ashley/Projects/Ghide/"));

	fileReference :: FilePath -> Reference [Word8];
	fileReference fname = MkReference
	{
		getRef = do
		{
			bs <- BS.readFile fname;
			return (BS.unpack bs);
		},
		setRef = \_ -> return ()	-- NYI
	};

	main :: IO ();
	main = do
	{
		System.Gnome.VFS.init;
		initGUI;
		let
		{
			MkMIMEFile mtype fname = myFile;
			interpreter = mimeInterpreter mtype;
			--obj = uriObject mySubject;
			obj = MkObject myContext (fileReference fname);
		};
		(window,split) <- createPanedWindow;
		pickObjBrowser (interpret interpreter obj) (onSelPane2 split) (browserAddToPane1 split);
		widgetShowAll window;
		mainGUI;
		System.Gnome.VFS.shutdown;
	};
}
