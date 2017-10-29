module Main(main) where
{
    import Shapes;
    import Data.IORef;
    import Pinafore.Window;
    import Graphics.UI.Gtk;
    import Truth.UI.GTK;
    import qualified Options.Applicative as O;


    pinaforeWindow :: FilePath -> IO SomeUIWindow;
    pinaforeWindow sqlitepath = fmap MkSomeUIWindow $ sqlitePinaforeWindow sqlitepath;

    optWMParser :: O.Parser [IO SomeUIWindow];
    optWMParser = O.many $ (pinaforeWindow <$> O.strOption (O.long "db"));

    optParser :: O.Parser ([IO SomeUIWindow],Bool);
    optParser = (,) <$> optWMParser <*> O.switch (O.short '2');

    main :: IO ();
    main = do
    {
        args <- initGUI;
        (wms,double) <- O.handleParseResult $ O.execParserPure O.defaultPrefs (O.info optParser mempty) args;
        _ <- timeoutAddFull (yield >> return True) priorityDefaultIdle 50;
        windowCount <- newIORef 0;
        for_ wms $ \wm -> do
        {
            MkSomeUIWindow uiw <- wm;
            makeWindowCountRef windowCount uiw;
            if double then makeWindowCountRef windowCount uiw else return ();
        };
        c <- readIORef windowCount;
        if c == 0 then return () else mainGUI;
    };
}
