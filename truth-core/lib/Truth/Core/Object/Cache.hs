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
    => ResourceContext
    -> Int
    -> Object edit
    -> LifeCycleIO (ResourceContext -> Object edit)
cacheObject rc mus obj = do
    (runAction, asyncTask) <-
        asyncWaitRunner mus $ \editsnl ->
            runResource rc obj $ \anobj -> pushOrFail "cached object" noEditSource $ objEdit anobj editsnl
    objRun <- liftIO $ stateResourceRunner $ cacheEmpty @ListCache @(EditCacheKey ListCache edit)
    return $ \rc' -> let
        objRead :: MutableRead (StateT (ListCache (EditCacheKey ListCache edit)) IO) (EditReader edit)
        objRead rt = traceBracket "cache read" $ do
            oldcache <- get
            case editCacheLookup @edit rt oldcache of
                Just t -> do
                    traceIOM "cache hit"
                    return t
                Nothing -> traceBracket "cache miss" $ do
                    t <- liftIO $ runResource rc' obj $ \(MkAnObject read _ _) -> read rt
                    liftIO $ runAction Nothing -- still reading, don't push yet
                    editCacheAdd @edit rt t
                    return t
        objEdit edits =
            traceBracket "cache update request" $
            return $
            Just $ \_ -> traceBracket "cache update action" $ do
                editCacheUpdates edits
                liftIO $ runAction $ Just edits
        objCommitTask = asyncTask <> objectCommitTask obj
        in MkResource objRun MkAnObject {..}
