module Main where
{
    import Prelude hiding (id,(.));
    import Control.Concurrent;
    import Control.Category;
    import Data.ByteString;
    import Data.Foldable;
    import Data.IORef;
    import Data.Result;
    import Data.Chain;
    import Data.Injection;
    import Data.Codec;
    import Data.Lens;
    import Truth.TypeKT;
    import Truth.Edit;
    import Truth.Object;
    import Truth.Linux.File;
    import Graphics.UI.Gtk;
    import Truth.UI.GTK;


    initial :: Maybe Bool;
    initial = Just True;

    main :: IO ();
    main = withINotifyB (\inotify -> do
    {
        windowCount <- newIORef 0;
        args <- initGUI;
        _ <- timeoutAddFull (yield >> return True) priorityDefaultIdle 50;
        for_ args (\arg -> let
        {
            file = linuxFileObject inotify arg; -- WithContext FilePath (Maybe ByteString)
            content :: Subscribe (JustWholeEdit Maybe (WholeEdit ByteString))
             = lensObject (toBiMapMaybe contentCleanLens) file; -- (Maybe ByteString)
            mrtext :: Subscribe (JustWholeEdit Maybe (JustWholeEdit (Result ListError) (ListEdit (WholeEdit Char))))
             = lensObject (convertEditLens . (simpleEditLens (cfmap (injectionLens (utf8Injection . (toBiMapMaybe (bijectionInjection packBijection)))))) . convertEditLens) content;
        } in do
        {
            makeWindowCountRef info windowCount mrtext;
        });
        {-
        sub <- makeWindowCountRef windowCount (maybeIVF False (gNamedView "AAAAAAAAAAAAAAAAAAAAAAAAAA")) (freeObjSubscribe initial);
        makeWindowCountRef windowCount (maybeIVF False (gNamedView "BBBBBBBBBBBBBBBBBBBBBBBBBBBBB")) sub;
        makeWindowCountRef windowCount (maybeIVF True (gNamedView "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC")) sub;
        -}
        mainGUI;
    });
}
