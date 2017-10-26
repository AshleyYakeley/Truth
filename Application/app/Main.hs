module Main(main) where
{
    import Shapes;
    import Data.IORef;
    import System.FilePath hiding ((<.>));
    import Truth.Core;
    import Truth.World.File;
    import Truth.World.Charset;
    import Truth.World.Soup;
    import Truth.World.Pinafore;
    import Graphics.UI.Gtk;
    import Truth.UI.GTK;
    import qualified Options.Applicative as O;


    newtype WindowMaker = MkWindowMaker ((forall edit actions. (Edit edit,WindowButtons actions) => UISpec edit -> String -> Subscriber edit actions -> IO ()) -> IO ());

    textCodec :: ReasonCodec ByteString String;
    textCodec = utf8Codec . bijectionCodec packBijection;

    textLens :: PureEditLens ByteStringEdit (WholeEdit ((Result String) String));
    textLens = (wholeEditLens $ injectionLens $ toInjection $ codecInjection textCodec) <.> convertEditLens;

    fileTextWindow :: Bool -> FilePath -> WindowMaker;
    fileTextWindow saveOpt path = MkWindowMaker $ \makeWindow -> do
    {
        let
        {
            bsObj :: Object ByteStringEdit;
            bsObj = fileObject path;

            wholeTextObj :: Object (WholeEdit ((Result String) String));
            wholeTextObj = cacheObject $ mapObject (MkCloseState textLens) bsObj;
        };
        if saveOpt then do
        {
            let
            {
                baseSub :: Subscriber (WholeEdit ((Result String) String)) ();
                baseSub = objectSubscriber wholeTextObj;

                bufferSub :: Subscriber (OneWholeEdit (Result String) (StringEdit String)) ((),SaveActions);
                bufferSub = saveBufferSubscriber baseSub;

                undoBufferSub :: Subscriber (OneWholeEdit (Result String) (StringEdit String)) (((),SaveActions),UndoActions);
                undoBufferSub = undoQueueSubscriber bufferSub;
            };
            textSub <- makeSharedSubscriber undoBufferSub;
            makeWindow (MkUISpec $ MkUIOneWhole $ MkUISpec MkStringUIText) (takeFileName path) textSub;
        }
        else do
        {
            let
            {
                textObj :: Object (OneWholeEdit (Result String) (StringEdit String));
                textObj = convertObject wholeTextObj;
            };
            textSub <- makeObjectSubscriber textObj;
            makeWindow (MkUISpec $ MkUIOneWhole $ MkUISpec MkStringUIText) (takeFileName path) textSub;
        };
    };

    soupWindow :: FilePath -> WindowMaker;
    soupWindow dirpath = MkWindowMaker $ \makeWindow -> do
    {
        sub <- makeObjectSubscriber $ soupObject dirpath;
        makeWindow soupEditSpec (takeFileName $ dropTrailingPathSeparator dirpath) sub;
    };

    pinaforeWindow :: FilePath -> WindowMaker;
    pinaforeWindow sqlitepath = MkWindowMaker $ \makeWindow -> do
    {
        sub <- makeObjectSubscriber $ sqlitePinaforeObject sqlitepath;
        makeWindow (pinaforeValueSpec rootValue) "Root" sub;
    };

    testSave :: Bool;
    testSave = True;

    optWMParser :: O.Parser [WindowMaker];
    optWMParser = O.many $ (pinaforeWindow <$> O.strOption (O.long "pinafore")) O.<|> (soupWindow <$> O.strOption (O.long "soup")) O.<|> (fileTextWindow testSave <$> O.strArgument mempty);

    optParser :: O.Parser ([WindowMaker],Bool);
    optParser = (,) <$> optWMParser <*> O.switch (O.short '2');

    main :: IO ();
    main = do
    {
        args <- initGUI;
        (wms,double) <- O.handleParseResult $ O.execParserPure O.defaultPrefs (O.info optParser mempty) args;
        _ <- timeoutAddFull (yield >> return True) priorityDefaultIdle 50;
        windowCount <- newIORef 0;
        for_ wms $ \(MkWindowMaker wm) -> wm $ \spec name sub -> do
        {
            makeWindowCountRef windowCount spec name sub;
            if double then makeWindowCountRef windowCount spec name sub else return ();
        };
        c <- readIORef windowCount;
        if c == 0 then return () else mainGUI;
    };
}
