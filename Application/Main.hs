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


    testSave :: Bool;
    testSave = True;

    soupOption :: Maybe FilePath;
    soupOption = Just ".";

    fileTextWindow :: IORef Int -> FilePath -> IO ();
    fileTextWindow windowCount path = do
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
        if testSave then do
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

    soupWindow :: IORef Int -> FilePath -> IO ();
    soupWindow windowCount dirpath = do
    {
        let
        {
            soupObject :: Object (SoupEdit (ObjectEdit ByteStringEdit));
            soupObject = nonlockingObject $ directorySoup fileSystemMutableEdit dirpath;
        };
        MkSubscriberW soupSub <- makeObjectSubscriber soupObject;
        makeWindowCountRef info windowCount soupSub;
    };

    main :: IO ();
    main = do
    {
        windowCount <- newIORef 0;
        args <- initGUI;
        _ <- timeoutAddFull (yield >> return True) priorityDefaultIdle 50;
        case soupOption of
        {
            Nothing -> for_ args $ fileTextWindow windowCount;
            Just dirpath -> soupWindow windowCount dirpath;
        };
        mainGUI;
    };
}
