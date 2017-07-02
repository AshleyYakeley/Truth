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
    import Graphics.UI.Gtk hiding (Object);
    import Truth.UI.GTK;


    testSave :: Bool;
    testSave = True;

    main :: IO ();
    main = do
    {
        windowCount <- newIORef 0;
        args <- initGUI;
        _ <- timeoutAddFull (yield >> return True) priorityDefaultIdle 50;
        for_ args $ \arg -> do
        {
            let
            {
                bsObj :: Object ByteStringEdit;
                bsObj = fileObject arg;

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
                };
                MkSubscriberW textSub <- makeSharedSubscriber bufferSub;
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
        {-
        sub <- makeWindowCountRef windowCount (maybeIVF False (gNamedView "AAAAAAAAAAAAAAAAAAAAAAAAAA")) (freeObjSubscribe initial);
        makeWindowCountRef windowCount (maybeIVF False (gNamedView "BBBBBBBBBBBBBBBBBBBBBBBBBBBBB")) sub;
        makeWindowCountRef windowCount (maybeIVF True (gNamedView "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC")) sub;
        -}
        mainGUI;
    };
}
