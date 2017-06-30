module Main where
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


    initial :: Maybe Bool;
    initial = Just True;

    testSave :: Bool;
    testSave = False;

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
                bsObj :: Object ByteStringEdit ();
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

                wholeTextObj :: Object (WholeEdit String) ();
                wholeTextObj = cacheObject $ fixedMapObject ((wholeEditLens $ injectionLens injection) . convertEditLens) bsObj;
            };
            if testSave then do
            {
                MkSubscriptionW fileSub <- subscribeObject wholeTextObj;
                let
                {
                    bufferSub :: Subscription (WholeEdit String) (IO (),SaveActions);
                    bufferSub = saveBufferSubscription fileSub;

                    editBufferSub :: Subscription (StringEdit String) (IO (),SaveActions);
                    editBufferSub = convertSubscription bufferSub;
                };
                makeWindowCountRef info windowCount addButtonsCloseSave editBufferSub
            }
            else do
            {
                let
                {
                    textObj :: Object (StringEdit String) ();
                    textObj = convertObject wholeTextObj;
                };
                MkSubscriptionW textSub <- subscribeObject textObj;
                makeWindowCountRef info windowCount addButtonsClose textSub;
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
