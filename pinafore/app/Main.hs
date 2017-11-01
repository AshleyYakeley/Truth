module Main(main) where
{
    import Shapes;
    import Pinafore.Window;
    import Truth.UI.GTK;
    import qualified Options.Applicative as O;


    optParser :: O.Parser (FilePath,Bool,[FilePath]);
    optParser = (,,) <$> (O.strOption $ O.long "db") <*> O.switch (O.short '2') <*> (O.many $ O.strArgument mempty);

    main :: IO ();
    main = truthMain $ \args -> do
    {
        (dbpath,double,puipaths) <- O.handleParseResult $ O.execParserPure O.defaultPrefs (O.info optParser mempty) args;
        wmss <- for puipaths $ \puipath -> do
        {
            puitext <- readFile puipath;
            wm <- fmap MkSomeUIWindow $ sqlitePinaforeWindow dbpath (puipath,puitext);
            return $ if double then [wm,wm] else [wm];
        };
        return $ mconcat wmss;
    };
}
