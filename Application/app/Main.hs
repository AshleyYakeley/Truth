module Main(main) where
{
    import Shapes;
    import Data.IORef;
    import System.FilePath hiding ((<.>));
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

    textLens :: EditLens () ByteStringEdit (WholeEdit ((Result String) String));
    textLens = (wholeEditLens $ injectionLens $ toInjection $ codecInjection textCodec) <.> convertEditLens;

    fileTextWindow :: Bool -> FilePath -> WindowMaker;
    fileTextWindow saveOpt path windowCount = do
    {
        let
        {
            bsObj :: Object ByteStringEdit;
            bsObj = fileObject path;

            wholeTextObj :: Object (WholeEdit ((Result String) String));
            wholeTextObj = cacheObject $ fixedMapObject textLens bsObj;
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
            makeWindowCountRef windowCount (MkUISpec $ MkUIOneWhole $ MkUISpec MkStringUIText) (takeFileName path) textSub;
        }
        else do
        {
            let
            {
                textObj :: Object (OneWholeEdit (Result String) (StringEdit String));
                textObj = convertObject wholeTextObj;
            };
            textSub <- makeObjectSubscriber textObj;
            makeWindowCountRef windowCount (MkUISpec $ MkUIOneWhole $ MkUISpec MkStringUIText) (takeFileName path) textSub;
        };
    };


    fromResult :: Result String String -> String;
    fromResult (SuccessResult s) = s;
    fromResult (FailureResult s) = "<" ++ s ++ ">";

    type SoupItemEdit = OneWholeEdit (Result String) NoteEdit;
    soupEditSpec :: UISpec (SoupEdit SoupItemEdit);
    soupEditSpec = let
    {
        nameColumn = MkKeyColumn "Name" $ funcEditFunction fromResult <.> oneWholeLiftEditFunction (tupleObjectFunction NoteTitle) <.> tupleObjectFunction EditSecond;
        aspect :: Aspect (OneWholeEdit Maybe (UUIDElementEdit SoupItemEdit));
        aspect = MkAspect "item" (MkUISpec $ MkUIOneWhole $ MkUISpec $ MkUIOneWhole noteEditSpec) $ oneWholeLiftGeneralLens $ tupleEditLens EditSecond;
    } in uiTableToContext [nameColumn] aspect;

    soupWindow :: FilePath -> WindowMaker;
    soupWindow dirpath windowCount = do
    {
        let
        {
            rawSoupObject :: Object (SoupEdit (MutableIOEdit ByteStringEdit));
            rawSoupObject = directorySoup fileSystemMutableEdit dirpath;

            soupContentsCodec :: ReasonCodec ByteString (EditSubject NoteEdit);
            soupContentsCodec = jsonValueCodec . jsonCodec;

            soupItemInjection :: Injection' (Result String) ByteString (EditSubject SoupItemEdit);
            soupItemInjection = codecInjection soupContentsCodec;

            paste :: forall m. MonadIO m => EditSubject SoupItemEdit -> m (Maybe ByteString);
            paste s = return $ getMaybeOne $ injBackwards soupItemInjection s;

            soupItemLens :: EditLens () ByteStringEdit SoupItemEdit;
            soupItemLens = convertEditLens <.> (wholeEditLens $ injectionLens soupItemInjection) <.> convertEditLens;

            lens :: EditLens () (SoupEdit (MutableIOEdit ByteStringEdit)) (SoupEdit SoupItemEdit);
            lens = liftSoupLens paste $ soupItemLens <.> mutableIOEditLens;

            soupObject :: Object (SoupEdit SoupItemEdit);
            soupObject = fixedMapObject lens rawSoupObject;
        };
        soupSub <- makeObjectSubscriber soupObject;
        makeWindowCountRef windowCount soupEditSpec (takeFileName $ dropTrailingPathSeparator dirpath) soupSub;
    };

    pinaforeWindow :: FilePath -> WindowMaker;
    pinaforeWindow sqlitepath windowCount = do
    {
        sub <- makeObjectSubscriber $ sqlitePinaforeObject sqlitepath;
        makeWindowCountRef windowCount (pinaforeValueSpec rootValue) "Root" sub;
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
