module Main where
{
    import UI.Truth.GTK;
    import Graphics.UI.Gtk;
    import Data.Changes;
    import Data.Changes.File.Linux;
    import Data.Chain;
    import Data.Result;
    import Data.IORef;
    import Data.Foldable;
    import Control.Category;
    import Control.Concurrent;
    import Prelude hiding (id,(.));

    initial :: Maybe Bool;
    initial = Just True;

    main :: IO ();
    main = withINotifyB (\inotify -> do
    {
        windowCount <- newIORef 0;
        args <- initGUI;
        timeoutAddFull (yield >> return True) priorityDefaultIdle 50;
        for_ args (\arg -> let
        {
            file = linuxFileObject inotify arg; -- WithContext FilePath (Maybe ByteString)
            content = lensSubscribe (toFloatingLens (fixedFloatingLens (cleanFixedLens contentCleanLens))) () file; -- (Maybe ByteString)
            mrtext :: Subscribe (Maybe (Result ListError String)) (WholeEdit (Maybe (Result ListError String))) =
             lensSubscribe (fixedFloatingLens (simpleFixedLens (wholeSimpleLens (cfmap (utf8Lens . packBSLens))))) () content;
        } in do
        {
            makeWindowCountRef windowCount mrtext;
        });
        {-
        sub <- makeWindowCountRef windowCount (maybeIVF False (gNamedView "AAAAAAAAAAAAAAAAAAAAAAAAAA")) (freeObjSubscribe initial);
        makeWindowCountRef windowCount (maybeIVF False (gNamedView "BBBBBBBBBBBBBBBBBBBBBBBBBBBBB")) sub;
        makeWindowCountRef windowCount (maybeIVF True (gNamedView "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC")) sub;
        -}
        mainGUI;
    });
}
