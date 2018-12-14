module Truth.Core.Object.Cache where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Read
import Truth.Debug

cacheObject ::
       forall edit. CacheableEdit edit
    => Int
    -> Object edit
    -> LifeCycle (Object edit)
cacheObject mus (MkObject unlift read push) = do
    runAction <- listDeferrer mus $ \editsnl -> traceBracket "cache update" $ runTransform (traceThing "cacheObject:back.update" unlift) $ pushOrFail "cached object" $ push $ concat editsnl
    cacheVar <- liftIO $ newMVar $ cacheEmpty @ListCache @(EditCacheKey ListCache edit)
    return $ let
        objRun = traceThing "cacheObject:front" $ mvarUnliftIO cacheVar
        objRead :: MutableRead (StateT (ListCache (EditCacheKey ListCache edit)) IO) (EditReader edit)
        objRead rt = traceBracket "cache read" $ do
            oldcache <- get
            case editCacheLookup @edit rt oldcache of
                Just t -> do
                    traceIOM "cache hit"
                    return t
                Nothing -> traceBracket "cache miss" $ do
                    t <- liftIO $ runTransform (traceThing "cacheObject:back.read" unlift) $ read rt
                    liftIO $ runAction Nothing -- still reading, don't push yet
                    editCacheAdd @edit rt t
                    return t
        objEdit edits =
            return $
            Just $ traceBracket "cache update" $ do
                editCacheUpdates edits
                liftIO $ runAction $ Just edits
        in MkObject {..}
