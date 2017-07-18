module Main(main) where
{
    import Prelude hiding (id,(.));
    import Control.Concurrent;
    import Control.Category;
    import Data.ByteString;
    import Data.Foldable;
    import Data.IORef;
    import Data.Result;
    import Data.Injection;
    import Data.Codec;
    import Data.Lens;
    import Data.Reity;
    import Truth.Core;
    import Truth.World.File;
    import Truth.World.FileSystem;
    import Truth.World.Soup;
    import Graphics.UI.Gtk hiding (Object);
    import Truth.UI.GTK;
    import qualified Options.Applicative as O;


    type WindowMaker = IORef Int -> IO ();

    fileTextWindow :: Bool -> FilePath -> WindowMaker;
    fileTextWindow saveOpt path windowCount = do
    {
        let
        {
            bsObj :: Object ByteStringEdit;
            bsObj = fileObject path;

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
            injection = errorInjection . utf8Injection . toBiMapMaybe (bijectionInjection packBijection);

            wholeTextObj :: Object (WholeEdit String);
            wholeTextObj = cacheObject $ fixedMapObject ((wholeEditLens $ injectionLens injection) . convertEditLens) bsObj;
        };
        if saveOpt then do
        {
            let
            {
                baseSub :: Subscriber (WholeEdit String) ();
                MkSubscriberW baseSub = objectSubscriber wholeTextObj;

                bufferSub :: Subscriber (StringEdit String) ((),SaveActions);
                bufferSub = saveBufferSubscriber baseSub;

                undoBufferSub :: Subscriber (StringEdit String) (((),SaveActions),UndoActions);
                undoBufferSub = undoQueueSubscriber bufferSub;
            };
            MkSubscriberW textSub <- makeSharedSubscriber undoBufferSub;
            makeWindowCountRef info windowCount textSub;
        }
        else do
        {
            let
            {
                textObj :: Object (StringEdit String);
                textObj = convertObject wholeTextObj;
            };
            MkSubscriberW textSub <- makeObjectSubscriber textObj;
            makeWindowCountRef info windowCount textSub;
        };
    };

    soupWindow :: FilePath -> WindowMaker;
    soupWindow dirpath windowCount = do
    {
        let
        {
            soupObject :: Object (SoupEdit (ObjectEdit ByteStringEdit));
            soupObject = nonlockingObject $ directorySoup fileSystemMutableEdit dirpath;
        };
        MkSubscriberW soupSub <- makeObjectSubscriber soupObject;
        makeWindowCountRef info windowCount soupSub;
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
