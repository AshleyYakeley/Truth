module Main(main) where
{
    import Prelude hiding (id,(.));
    import Control.Concurrent;
    import Control.Category;
    import Data.ByteString.Lazy;
    import Data.Foldable;
    import Data.IORef;
    import Control.Monad.IO.Class;
    import Control.Constrained.Category;
    import Data.Result;
    import Data.Injection;
    import Data.Codec;
    import Data.Lens;
    import Truth.Core;
    import Truth.World.File;
    import Truth.World.FileSystem;
    import Truth.World.Charset;
    import Truth.World.Soup;
    import Graphics.UI.Gtk hiding (Object);
    import Truth.UI.GTK;
    import qualified Options.Applicative as O;


    type WindowMaker = IORef Int -> IO ();

    textCodec :: ReasonCodec ByteString String;
    textCodec = utf8Codec . bijectionCodec packBijection;

    textLens :: PureEditLens () ByteStringEdit (WholeEdit String);
    textLens = let
    {
        errorInjection :: forall m err a. (Show err,Applicative m) => Injection' m (Result err a) a;
        errorInjection = let
        {
            injForwards :: Result err a -> a;
            injForwards (SuccessResult a) = a;
            injForwards (FailureResult err) = error $ show err;

            injBackwards :: a -> m (Result err a);
            injBackwards = pure . SuccessResult;
        } in MkInjection{..};

        injection :: Injection ByteString String;
        injection = errorInjection . (toInjection $ codecInjection textCodec);
    } in (wholeEditLens $ injectionLens injection) <.> convertEditLens;

    fileTextWindow :: Bool -> FilePath -> WindowMaker;
    fileTextWindow saveOpt path windowCount = do
    {
        let
        {
            bsObj :: Object ByteStringEdit;
            bsObj = fileObject path;

            wholeTextObj :: Object (WholeEdit String);
            wholeTextObj = cacheObject $ pureFixedMapObject textLens bsObj;
        };
        if saveOpt then do
        {
            let
            {
                baseSub :: Subscriber (WholeEdit String) ();
                baseSub = objectSubscriber wholeTextObj;

                bufferSub :: Subscriber (StringEdit String) ((),SaveActions);
                bufferSub = saveBufferSubscriber baseSub;

                undoBufferSub :: Subscriber (StringEdit String) (((),SaveActions),UndoActions);
                undoBufferSub = undoQueueSubscriber bufferSub;
            };
            textSub <- makeSharedSubscriber undoBufferSub;
            makeWindowCountRef windowCount textSub;
        }
        else do
        {
            let
            {
                textObj :: Object (StringEdit String);
                textObj = convertObject wholeTextObj;
            };
            textSub <- makeObjectSubscriber textObj;
            makeWindowCountRef windowCount textSub;
        };
    };

    type SoupContents = StringEdit String;

    soupWindow :: FilePath -> WindowMaker;
    soupWindow dirpath windowCount = do
    {
        let
        {
            rawSoupObject :: Object (SoupEdit (MutableIOEdit ByteStringEdit));
            rawSoupObject = directorySoup fileSystemMutableEdit dirpath;

            paste :: forall m. MonadIO m => EditSubject SoupContents -> m (Maybe ByteString);
            paste s = return $ pure $ encode textCodec s;

            soupItemLens :: IOEditLens' Maybe () ByteStringEdit SoupContents;
            soupItemLens = convertEditLens <.> pureToEditLens textLens;

            lens :: IOEditLens' Maybe () (SoupEdit (MutableIOEdit ByteStringEdit)) (SoupEdit SoupContents);
            lens = liftSoupLens paste $ soupItemLens <.> mutableIOEditLens;

            soupObject :: Object (SoupEdit SoupContents);
            soupObject = fixedMapObject lens rawSoupObject;
        };
        soupSub <- makeObjectSubscriber soupObject;
        makeWindowCountRef windowCount soupSub;
    };

    testSave :: Bool;
    testSave = True;

    optParser :: O.Parser [WindowMaker];
    optParser = O.many $ (soupWindow <$> O.strOption (O.long "soup")) O.<|> (fileTextWindow testSave <$> O.strArgument mempty);

    main :: IO ();
    main = do
    {
        args <- initGUI;
        wms <- O.handleParseResult $ O.execParserPure O.defaultPrefs (O.info optParser mempty) args;
        _ <- timeoutAddFull (yield >> return True) priorityDefaultIdle 50;
        windowCount <- newIORef 0;
        for_ wms $ \wm -> wm windowCount;
        mainGUI;
    };
}
