module Main where
{
    import UI.Truth.GTK;
    import Graphics.UI.Gtk;
    import Data.Changes;
    import Data.Changes.File.Linux;
    import Data.Lens;
    import Data.Injection;
    import Data.Chain;
    import Data.Result;
    import Data.IORef;
    import Data.Foldable;
    import Data.ByteString;
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
            content :: Subscribe (JustWholeEdit Maybe (WholeEdit ByteString))
             = lensSubscribe (toFloatingLens (fixedFloatingLens (cleanFixedLens contentCleanLens))) file; -- (Maybe ByteString)
            mrtext :: Subscribe (JustWholeEdit Maybe (JustWholeEdit (Result ListError) (ListEdit (WholeEdit Char))))
             = lensSubscribe (convertFixedEditLens . (simpleFixedLens (cfmap (injectionLens (utf8Injection . (bijectionInjection packBijection))))) . convertFixedEditLens) content;
        } in do
        {
            makeWindowCountRef infoT windowCount mrtext;
        });
        {-
        sub <- makeWindowCountRef windowCount (maybeIVF False (gNamedView "AAAAAAAAAAAAAAAAAAAAAAAAAA")) (freeObjSubscribe initial);
        makeWindowCountRef windowCount (maybeIVF False (gNamedView "BBBBBBBBBBBBBBBBBBBBBBBBBBBBB")) sub;
        makeWindowCountRef windowCount (maybeIVF True (gNamedView "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC")) sub;
        -}
        mainGUI;
    });
}
