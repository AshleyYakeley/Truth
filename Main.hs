
module Main where
{
	import Distribution.PackageDescription;
	import Graphics.UI.Gtk;
	import Graphics.UI.Gtk.SourceView;

	makePane :: IO ScrolledWindow;
	makePane = do
	{
		s <- sourceViewNew;
		w <- scrolledWindowNew Nothing Nothing;
		containerAdd w s;
		scrolledWindowSetPolicy w PolicyAutomatic PolicyAutomatic;
		return w;
	};

	makeItem :: TreeStore -> Maybe TreeIter -> String -> String -> IO TreeIter;
	makeItem store mt s0 s1 = do
	{
		iter <- treeStoreAppend store mt;
		treeStoreSetValue store iter 0 (GVstring (Just s0));
		treeStoreSetValue store iter 1 (GVstring (Just s1));
		return iter;
	};

	makeModuleItem :: TreeStore -> TreeIter -> String -> IO TreeIter;
	makeModuleItem store ti fname = makeItem store (Just ti) fname "hs";
	
	addBuildInfo :: TreeStore -> TreeIter -> BuildInfo -> IO ();
	addBuildInfo store ti info = do
	{
		itHidden <- makeItem store (Just ti) "Hidden" "";
		mapM_ (makeModuleItem store itHidden) (otherModules info);
	};
	
	makeLibraryFolder :: TreeStore -> (Maybe TreeIter) -> Library -> IO ();
	makeLibraryFolder store mti lib = do
	{
		itLib <- makeItem store mti "Library" "";
		mapM_ (makeModuleItem store itLib) (exposedModules lib);
		addBuildInfo store itLib (libBuildInfo lib);
	};
	
	makeExecutableFolder :: TreeStore -> (Maybe TreeIter) -> Executable -> IO ();
	makeExecutableFolder store mti exe = do
	{
		itExe <- makeItem store mti (exeName exe) "";
		makeModuleItem store itExe (modulePath exe);
		addBuildInfo store itExe (buildInfo exe);
	};

	makeTreeView :: PackageDescription -> IO TreeView;
	makeTreeView descr = do
	{
		store <- treeStoreNew [TMstring,TMstring];
		case library descr of
		{
			Just lib -> makeLibraryFolder store Nothing lib;
			_ -> return ();
		};
		mapM_ (makeExecutableFolder store Nothing) (executables descr);
		itProps <- makeItem store Nothing "Properties" "";
		tv <- treeViewNewWithModel store;
		rText <- cellRendererTextNew;
		c0 <- treeViewColumnNewWithAttributes "Name" rText [("text",0)];
 		treeViewAppendColumn tv c0;
		treeViewSetHeadersVisible tv True;
		return tv;
	};

	main :: IO ();
	main = do
	{
		initGUI;
		descr <- readPackageDescription "Ghide.cabal";
		window <- windowNew;
		onDestroy window mainQuit;
		split <- hPanedNew;
		tv <- makeTreeView descr;
		w2 <- makePane;
		onCursorChanged tv (do
		{
			sel <- treeViewGetSelection tv;
			(Just ti) <- treeSelectionGetSelected sel;
			putStrLn "select";
		});
		panedAdd1 split tv;
		panedAdd2 split w2;
		set window [containerChild := split];
		widgetShowAll window;
		mainGUI;
	};

}
