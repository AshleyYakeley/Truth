module System.INotify.Balanced
(
    Event(..),EventVariety(..),Cookie,
    INotifyB,initINotifyB,killINotifyB,withINotifyB,
    WatchDescriptorB,
    addWatchB,removeWatchB
) where
{
    import System.INotify;
    import Control.Concurrent.MVar;
    import Control.Monad;
    import Control.Exception;
    import Data.Map;
    import Data.List(nub);
    import Data.Unique;
    import Prelude hiding (lookup,null);
    
    type CallbackMap = Map Unique ([EventVariety],Event -> IO ());
    
    matchEventVariety :: Event -> EventVariety -> Bool;
    matchEventVariety (Closed _ _ _) Close = True;
    matchEventVariety (Modified _ _) Modify = True;
    matchEventVariety (MovedSelf _) MoveSelf = True;
    matchEventVariety (DeletedSelf) DeleteSelf = True;
    matchEventVariety _ _ = False;
    
    matchEventVarieties :: Event -> [EventVariety] -> Bool;
    matchEventVarieties event = any (matchEventVariety event);
    
    addMapWatch :: INotify -> FilePath -> CallbackMap -> IO WatchDescriptor;
    addMapWatch inotify path cbmap = addWatch inotify varieties path dispatch where
    {
        callbacks = elems cbmap;
        varieties = nub (concat (fmap fst callbacks));
        dispatch event = forM_ callbacks 
         (\(vs,callback) ->
          if matchEventVarieties event vs
           then callback event
           else return ());
    };
    
    type WatchMap = Map WatchDescriptor (FilePath,CallbackMap);
    
    data INotifyB = MkINotifyB INotify (MVar WatchMap);
    
    initINotifyB :: IO INotifyB;
    initINotifyB = do
    {
        inotify <- initINotify;
        mvar <- newMVar empty;
        return (MkINotifyB inotify mvar);
    };
    
    killINotifyB :: INotifyB -> IO ();
    killINotifyB (MkINotifyB inotify _) = killINotify inotify;

    withINotifyB :: (INotifyB -> IO a) -> IO a;
    withINotifyB = bracket initINotifyB killINotifyB;
    
    data WatchDescriptorB = MkWatchDescriptorB WatchDescriptor Unique;

    instance Show WatchDescriptorB where
    {
        show (MkWatchDescriptorB wd _) = (show wd);
    };

    addWatchB :: INotifyB -> [EventVariety] -> FilePath -> (Event -> IO ()) -> IO WatchDescriptorB;
    addWatchB (MkINotifyB inotify mvar) evs path callback = modifyMVar mvar (\wdmap -> do
    {
        uniq <- newUnique;
        wd <- addWatch inotify [AllEvents] path (\_ -> return ());
        let
        {
            cbmap = case lookup wd wdmap of
            {
                Just (_,oldcbmap) -> insert uniq (evs,callback) oldcbmap;
                _ -> singleton uniq (evs,callback);
            };
        };
        addMapWatch inotify path cbmap;
        return (insert wd (path,cbmap) wdmap,MkWatchDescriptorB wd uniq);
    });
    
    removeWatchB :: INotifyB -> WatchDescriptorB -> IO ();
    removeWatchB (MkINotifyB inotify mvar) (MkWatchDescriptorB wd uniq) = modifyMVar_ mvar (\wdmap -> case lookup wd wdmap of
    {
        Just (path,cbmap) -> do
        {
            let {cbmap' = delete uniq cbmap};
            if null cbmap' then do
            {
                removeWatch inotify wd;
                return (delete wd wdmap);
            }
            else do
            {
                Prelude.catch (addMapWatch inotify path cbmap' >> return ()) (\_ -> return ());    -- don't worry about non-existent inodes
                return (insert wd (path,cbmap') wdmap);
            };
        };
        _ -> fail "WatchDescriptor not found in map";
    });
}
