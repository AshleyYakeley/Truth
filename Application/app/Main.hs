module Main(main) where
{
    import Shapes;
    import Data.IORef;
    import System.FilePath hiding ((<.>));
    import Data.Reity;
    import Truth.Core;
    import Truth.World.File;
    import Truth.World.FileSystem;
    import Truth.World.Charset;
    import Truth.World.JSON;
    import Truth.World.Note;
    import Truth.World.Soup;
    import Truth.World.Pinafore;
    import Graphics.UI.Gtk hiding (Object);
    import Truth.UI.GTK;
    import qualified Options.Applicative as O;


    type WindowMaker = IORef Int -> IO ();

    textCodec :: ReasonCodec ByteString String;
    textCodec = utf8Codec . bijectionCodec packBijection;

    textLens :: PureEditLens () ByteStringEdit (WholeEdit (ReasonM String));
    textLens = (wholeEditLens $ injectionLens $ toInjection $ codecInjection textCodec) <.> convertEditLens;

    fileTextWindow :: Bool -> FilePath -> WindowMaker;
    fileTextWindow saveOpt path windowCount = do
    {
        let
        {
            bsObj :: Object ByteStringEdit;
            bsObj = fileObject path;

            wholeTextObj :: Object (WholeEdit (ReasonM String));
            wholeTextObj = cacheObject $ pureFixedMapObject textLens bsObj;
        };
        if saveOpt then do
        {
            let
            {
                baseSub :: Subscriber (WholeEdit (ReasonM String)) ();
                baseSub = objectSubscriber wholeTextObj;

                bufferSub :: Subscriber (OneWholeEdit ReasonM (StringEdit String)) ((),SaveActions);
                bufferSub = saveBufferSubscriber baseSub;

                undoBufferSub :: Subscriber (OneWholeEdit ReasonM (StringEdit String)) (((),SaveActions),UndoActions);
                undoBufferSub = undoQueueSubscriber bufferSub;
            };
            textSub <- makeSharedSubscriber undoBufferSub;
            makeWindowCountRef windowCount (takeFileName path) textSub;
        }
        else do
        {
            let
            {
                textObj :: Object (OneWholeEdit ReasonM (StringEdit String));
                textObj = convertObject wholeTextObj;
            };
            textSub <- makeObjectSubscriber textObj;
            makeWindowCountRef windowCount (takeFileName path) textSub;
        };
    };

    type SoupContentsEdit = TupleEdit NoteSel;
    type SoupItemEdit = OneWholeEdit ReasonM SoupContentsEdit;

    soupWindow :: FilePath -> WindowMaker;
    soupWindow dirpath windowCount = do
    {
        let
        {
            rawSoupObject :: Object (SoupEdit (MutableIOEdit ByteStringEdit));
            rawSoupObject = directorySoup fileSystemMutableEdit dirpath;

            soupContentsCodec :: ReasonCodec ByteString (EditSubject SoupContentsEdit);
            soupContentsCodec = jsonValueCodec . jsonCodec;

            soupItemInjection :: Injection' ReasonM ByteString (EditSubject SoupItemEdit);
            soupItemInjection = codecInjection soupContentsCodec;

            paste :: forall m. MonadIO m => EditSubject SoupItemEdit -> m (ReasonM ByteString);
            paste s = return $ injBackwards soupItemInjection s;

            soupItemLens :: IOEditLens' ReasonM () ByteStringEdit SoupItemEdit;
            soupItemLens = convertEditLens <.> (wholeEditLens $ injectionLens soupItemInjection) <.> convertEditLens;

            lens :: IOEditLens' ReasonM () (SoupEdit (MutableIOEdit ByteStringEdit)) (SoupEdit SoupItemEdit);
            lens = liftSoupLens paste $ soupItemLens <.> mutableIOEditLens;

            soupObject :: Object (SoupEdit SoupItemEdit);
            soupObject = fixedMapObject lens rawSoupObject;
        };
        soupSub <- makeObjectSubscriber soupObject;
        makeWindowCountRef windowCount (takeFileName $ dropTrailingPathSeparator dirpath) soupSub;
    };

    pinaforeWindow :: FilePath -> WindowMaker;
    pinaforeWindow sqlitepath windowCount = do
    {
        sub <- makeObjectSubscriber $ sqlitePinaforeObject sqlitepath;
        pinaforeValueLens rootValue $ \lens -> makeWindowCountRef windowCount "Root" $ mapSubscriber lens sub;
    };

    testSave :: Bool;
    testSave = True;

    optParser :: O.Parser [WindowMaker];
    optParser = O.many $ (pinaforeWindow <$> O.strOption (O.long "pinafore")) O.<|> (soupWindow <$> O.strOption (O.long "soup")) O.<|> (fileTextWindow testSave <$> O.strArgument mempty);

    main :: IO ();
    main = do
    {
        args <- initGUI;
        wms <- O.handleParseResult $ O.execParserPure O.defaultPrefs (O.info optParser mempty) args;
        _ <- timeoutAddFull (yield >> return True) priorityDefaultIdle 50;
        windowCount <- newIORef 0;
        for_ wms $ \wm -> wm windowCount;
        c <- readIORef windowCount;
        if c == 0 then return () else mainGUI;
    };
}
