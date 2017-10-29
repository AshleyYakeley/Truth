module Main(main) where
{
    import Shapes;
    import Data.IORef;
    import System.FilePath hiding ((<.>));
    import Truth.Core;
    import Truth.World.File;
    import Truth.World.Charset;
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

    optParser :: O.Parser ([FilePath],Bool,Bool);
    optParser = (,,) <$> (O.many $ O.strArgument mempty) <*> O.switch (O.short '2') <*> O.switch (O.long "save");

    main :: IO ();
    main = do
    {
        args <- initGUI;
        (paths,double,save) <- O.handleParseResult $ O.execParserPure O.defaultPrefs (O.info optParser mempty) args;
        _ <- timeoutAddFull (yield >> return True) priorityDefaultIdle 50;
        windowCount <- newIORef 0;
        for_ paths $ \path -> do
        {
            MkSomeUIWindow uiw <- fileTextWindow save path;
            makeWindowCountRef windowCount uiw;
            if double then makeWindowCountRef windowCount uiw else return ();
        };
        c <- readIORef windowCount;
        if c == 0 then return () else mainGUI;
    };
}
