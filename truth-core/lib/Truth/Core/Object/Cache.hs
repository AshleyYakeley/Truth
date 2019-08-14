module Truth.Core.Object.Cache where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Object.UnliftIO
import Truth.Core.Read
import Truth.Debug

cacheObject ::
       forall edit. CacheableEdit edit
    => Int
    -> Object edit
    -> LifeCycleIO (Object edit)
cacheObject mus (MkCloseUnliftIO unlift (MkAnObject read push)) = do
    runAction <-
        asyncWaitRunner mus $ \editsnl -> traceBracket "cache update" $ runTransform (traceThing "cacheObject:back.update" unlift) $ pushOrFail "cached object" noEditSource $ push editsnl
    cacheVar <- liftIO $ newMVar $ cacheEmpty @ListCache @(EditCacheKey ListCache edit)
    return $ let
        objRun = traceThing "cacheObject:front.mvarUnliftIO" $ mvarUnliftIO cacheVar
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
            traceBracket "cache update request" $
            return $
            Just $ \_ -> traceBracket "cache update action" $ do
                editCacheUpdates edits
                liftIO $ runAction $ Just edits
        in MkCloseUnliftIO objRun MkAnObject {..}
