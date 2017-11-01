module Main(main) where
{
    import Shapes;
    import Pinafore.Window;
    import Truth.UI.GTK;
    import qualified Options.Applicative as O;


    optParser :: O.Parser (FilePath,[FilePath]);
    optParser = (,) <$> (O.strOption $ O.long "db") <*> (O.many $ O.strArgument mempty);

    main :: IO ();
    main = truthMain $ \args -> do
    {
        (dbpath,puipaths) <- O.handleParseResult $ O.execParserPure O.defaultPrefs (O.info optParser mempty) args;
        wmss <- for puipaths $ \puipath -> do
        {
            puitext <- readFile puipath;
            wms <- sqlitePinaforeWindow dbpath (puipath,puitext);
            return $ fmap MkSomeUIWindow wms;
        };
        return $ mconcat wmss;
    };
}
