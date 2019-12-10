module Truth.Core.Object.Cache where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Read
import Truth.Core.Resource
import Truth.Debug

cacheObject ::
       forall edit. CacheableEdit edit
    => Int
    -> Object edit
    -> LifeCycleIO (Object edit)
cacheObject mus (MkResource rr (MkAnObject read push)) =
    runResourceRunnerWith rr $ \run -> do
        runAction <- asyncWaitRunner mus $ \editsnl -> traceBracket "cache update" $ traceThing "cacheObject:back.update" run $ pushOrFail "cached object" noEditSource $ push editsnl
        objRun <- liftIO $ stateResourceRunner $ cacheEmpty @ListCache @(EditCacheKey ListCache edit)
        return $ let
            objRead :: MutableRead (StateT (ListCache (EditCacheKey ListCache edit)) IO) (EditReader edit)
            objRead rt = traceBracket "cache read" $ do
                oldcache <- get
                case editCacheLookup @edit rt oldcache of
                    Just t -> do
                        traceIOM "cache hit"
                        return t
                    Nothing -> traceBracket "cache miss" $ do
                        t <- liftIO $ traceThing "cacheObject:back.read" run $ read rt
                        liftIO $ runAction Nothing -- still reading, don't push yet
                        editCacheAdd @edit rt t
                        return t
            objEdit edits =
                traceBracket "cache update request" $
                return $
                Just $ \_ -> traceBracket "cache update action" $ do
                    editCacheUpdates edits
                    liftIO $ runAction $ Just edits
            in MkResource objRun MkAnObject {..}
