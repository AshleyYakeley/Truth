module Browser where
{
	import Partial;
	import Object;
	import GnomeVFS;
	import Graphics.UI.Gtk hiding (Object);
	import Graphics.UI.Gtk.SourceView;
	import Distribution.PackageDescription;
	import Distribution.Simple.Utils;
	import Data.Maybe;
	import Data.Word;	
	
	data Browser a = forall w. (WidgetClass w) => MkBrowser
	{
		browserWidget :: w,
		browserChanged :: IO (Maybe (IO a)),
		browserSelection :: IO [Object],
		closeBrowser :: IO ()
	};
	
	instance Show GenericValue where
	{
		show (GVstring (Just s)) = show s;
		show (GVstring _) = "<null string>";
		show _ = "<other type>"
	};

	type BrowserFactory a = IO a -> ([Object] -> IO ()) -> IO (Browser a);
	
	pickBrowser :: ObjectType a -> BrowserFactory a;
	pickBrowser TextObjectType = textBrowser;
	pickBrowser PackageDescriptionObjectType = cabalBrowser;
	pickBrowser _ = lastResortBrowser;

	textBrowser :: BrowserFactory String;
	textBrowser getter onSel = do
	{
		text <- getter;
		buffer <- textBufferNew Nothing;
		textBufferSetText buffer text;
		view <- textViewNewWithBuffer buffer;
		onMoveCursor view (\_ _ _ -> do
		{
			sel <- getBufferSelection buffer;
			onSel sel;
		});
		return 
		(MkBrowser
			view
			(return Nothing)
			(getBufferSelection buffer)
			(return ())
		);
	} where
	{
		getBufferSelection :: TextBuffer -> IO [Object];
		getBufferSelection buffer = do
		{
			m1 <- textBufferGetInsert buffer;
			i1 <- textBufferGetIterAtMark buffer m1;
			m2 <- textBufferGetSelectionBound buffer;
			i2 <- textBufferGetIterAtMark buffer m2;
			text <- textBufferGetSlice buffer i1 i2 True;
			return [MkObject TextObjectType text];
		};
	};
	
	lastResortBrowser :: BrowserFactory a;
	lastResortBrowser _ _ = do
	{
		w <- labelNew (Just "No Browser");
		return (MkBrowser w (return Nothing) (return []) (return ()));
	};
	
	cabalBrowser :: BrowserFactory PackageDescription;
	cabalBrowser getter onSel = do
	{
		pd <- getter;
		view <- makeTreeView "file:///home/ashley/Projects/Ghide/Ghide.cabal" pd;
		return
		(MkBrowser
			view
			(return Nothing)
			(return [])
			(return ())
		);
	}
	where
	{
		makeItem :: TreeStore -> Maybe TreeIter -> String -> Maybe String -> IO TreeIter;
		makeItem store mt s0 ms1 = do
		{
			iter <- treeStoreAppend store mt;
			treeStoreSetValue store iter 0 (GVstring (Just s0));
			treeStoreSetValue store iter 1 (GVstring ms1);
			return iter;
		};

		makeModuleItem :: TreeStore -> TreeIter -> BuildInfo -> String -> IO TreeIter;
		makeModuleItem store ti info fname = do
		{
			pathl <- moduleToFilePath (hsSourceDirs info) fname ["chs","hs"];
			mpath <- case pathl of
			{
				path:_ -> return (Just path);
				_ -> return Nothing;
			};
			makeItem store (Just ti) fname mpath;
		};
		
		addBuildInfo :: TreeStore -> TreeIter -> BuildInfo -> IO ();
		addBuildInfo store ti info = do
		{
			itHidden <- makeItem store (Just ti) "Hidden" Nothing;
			mapM_ (makeModuleItem store itHidden info) (otherModules info);
		};
		
		makeLibraryFolder :: TreeStore -> (Maybe TreeIter) -> Library -> IO ();
		makeLibraryFolder store mti lib = do
		{
			itLib <- makeItem store mti "Library" Nothing;
			mapM_ (makeModuleItem store itLib (libBuildInfo lib)) (exposedModules lib);
			addBuildInfo store itLib (libBuildInfo lib);
		};
		
		makeExecutableFolder :: TreeStore -> (Maybe TreeIter) -> Executable -> IO ();
		makeExecutableFolder store mti exe = do
		{
			itExe <- makeItem store mti (exeName exe) Nothing;
			makeItem store (Just itExe) (modulePath exe) (Just (modulePath exe));
			addBuildInfo store itExe (buildInfo exe);
		};

		makeTreeView :: String -> PackageDescription -> IO TreeView;
		makeTreeView baseURI descr = do
		{
			store <- treeStoreNew [TMstring,TMstring];
			case library descr of
			{
				Just lib -> makeLibraryFolder store Nothing lib;
				_ -> return ();
			};
			mapM_ (makeExecutableFolder store Nothing) (executables descr);
			itProps <- makeItem store Nothing "Properties" Nothing;
			tv <- treeViewNewWithModel store;
			rText <- cellRendererTextNew;
			c0 <- treeViewColumnNewWithAttributes "Name" rText [("text",0)];
	 		treeViewAppendColumn tv c0;
			treeViewSetHeadersVisible tv True;
			onCursorChanged tv (do
			{
				sel <- treeViewGetSelection tv;
				(Just ti) <- treeSelectionGetSelected sel;
				(GVstring (Just s)) <- treeModelGetValue store ti 0;
				v1 <- treeModelGetValue store ti 1;
				case v1 of
				{
					GVstring (Just fp) -> do
					{
						uri <- uriMakeFullFromRelative baseURI fp;
						mmime <- getMIMEType uri;
						putStrLn ("select " ++ uri ++ " ("++(fromMaybe "<unknown>" mmime)++")");
					};
					_ -> return ();
				};
			});
			return tv;
		};
	};
}
