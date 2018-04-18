module Truth.Core.Object.Cache where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Read
import Truth.Debug

cacheObject ::
       forall edit. CacheableEdit edit
    => Object edit
    -> LifeCycle (Object edit)
cacheObject (MkObject unlift read push) = do
    runAction <- listDeferrer $ \editsnl -> traceBracket "cache update" $ runUnliftIO unlift $ pushOrFail "cached object" $ push $ concat editsnl
    cacheVar <- liftIO $ newMVar $ cacheEmpty @ListCache @(EditCacheKey ListCache edit)
    return $ let
        objRun = mvarUnliftIO cacheVar
        objRead :: MutableRead (StateT (ListCache (EditCacheKey ListCache edit)) IO) (EditReader edit)
        objRead rt = do
            oldcache <- get
            case editCacheLookup @edit rt oldcache of
                Just t -> traceBracket "cache hit" $ return t
                Nothing -> traceBracket "cache miss" $ do
                    t <- liftIO $ runUnliftIO unlift $ read rt
                    editCacheAdd @edit rt t
                    return t
        objEdit edits =
            return $
            Just $ traceBracket "cache update" $ do
                editCacheUpdates edits
                liftIO $ runAction edits
        in MkObject {..}
