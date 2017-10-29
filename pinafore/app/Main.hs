module Main(main) where
{
    import Shapes;
    import Pinafore.Window;
    import Truth.UI.GTK;
    import qualified Options.Applicative as O;


    optParser :: O.Parser ([FilePath],Bool);
    optParser = (,) <$> (O.many $ O.strArgument mempty) <*> O.switch (O.short '2');

    main :: IO ();
    main = truthMain $ \args -> do
    {
        (dbpaths,double) <- O.handleParseResult $ O.execParserPure O.defaultPrefs (O.info optParser mempty) args;
        wmss <- for dbpaths $ \dbpath -> do
        {
            wm <- fmap MkSomeUIWindow $ sqlitePinaforeWindow dbpath;
            return $ if double then [wm,wm] else [wm];
        };
        return $ mconcat wmss;
    };
}
