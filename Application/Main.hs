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

                --injection :: Injection ByteString (Result ListError String);
                --injection = utf8Injection . toBiMapMaybe (bijectionInjection packBijection);
                injection :: Injection ByteString String;
                injection = errorInjection . utf8Injection . toBiMapMaybe (bijectionInjection packBijection);

                editLens :: EditLens ByteStringEdit (StringEdit String);
                editLens = convertEditLens . (wholeEditLens $ injectionLens injection) . convertEditLens;

                --textSub :: Subscription (OneWholeEdit Maybe (OneWholeEdit (Result ListError) (StringEdit String)))
                textObj :: Object (StringEdit String) ();
                textObj = mapObject (fixedFloatingEditLens editLens) $ fmap (\_ -> ((),())) bsObj;
            };
            MkSubscriptionW textSub <- subscribeObject textObj;
            makeWindowCountRef info windowCount textSub;
        };
        {-
        sub <- makeWindowCountRef windowCount (maybeIVF False (gNamedView "AAAAAAAAAAAAAAAAAAAAAAAAAA")) (freeObjSubscribe initial);
        makeWindowCountRef windowCount (maybeIVF False (gNamedView "BBBBBBBBBBBBBBBBBBBBBBBBBBBBB")) sub;
        makeWindowCountRef windowCount (maybeIVF True (gNamedView "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC")) sub;
        -}
        mainGUI;
    };
}
