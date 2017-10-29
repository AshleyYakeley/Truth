module Main(main) where
{
    import Shapes;
    import Data.IORef;
    import System.FilePath hiding ((<.>));
    import Truth.Core;
    import Truth.World.File;
    import Truth.World.Charset;
    import Truth.World.Soup;
    import Graphics.UI.Gtk;
    import Truth.UI.GTK;
    import qualified Options.Applicative as O;


    textCodec :: ReasonCodec ByteString String;
    textCodec = utf8Codec . bijectionCodec packBijection;

    textLens :: PureEditLens ByteStringEdit (WholeEdit ((Result String) String));
    textLens = (wholeEditLens $ injectionLens $ toInjection $ codecInjection textCodec) <.> convertEditLens;

    fileTextWindow :: Bool -> FilePath -> IO SomeUIWindow;
    fileTextWindow saveOpt path = do
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
            return $ MkSomeUIWindow $ MkUIWindow (takeFileName path) (uiOneWhole uiStringText) textSub;
        }
        else do
        {
            let
            {
                textObj :: Object (OneWholeEdit (Result String) (StringEdit String));
                textObj = convertObject wholeTextObj;
            };
            textSub <- makeObjectSubscriber textObj;
            return $ MkSomeUIWindow $ MkUIWindow (takeFileName path) (uiOneWhole uiStringText) textSub;
        };
    };

    soupWindowMaker :: FilePath -> IO SomeUIWindow;
    soupWindowMaker dirpath = fmap MkSomeUIWindow $ soupWindow dirpath;

    testSave :: Bool;
    testSave = True;

    optWMParser :: O.Parser [IO SomeUIWindow];
    optWMParser = O.many $ (soupWindowMaker <$> O.strOption (O.long "soup")) O.<|> (fileTextWindow testSave <$> O.strArgument mempty);

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
