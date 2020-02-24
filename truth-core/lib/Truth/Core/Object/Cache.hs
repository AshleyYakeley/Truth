module Truth.Core.Object.Cache where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Read
import Truth.Core.Resource

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
        objRead rt = do
            oldcache <- get
            case editCacheLookup @edit rt oldcache of
                Just t -> return t
                Nothing -> do
                    t <- liftIO $ runResource rc' obj $ \(MkAnObject read _ _) -> read rt
                    liftIO $ runAction Nothing -- still reading, don't push yet
                    editCacheAdd @edit rt t
                    return t
        objEdit edits =
            return $
            Just $ \_ -> do
                editCacheUpdates edits
                liftIO $ runAction $ Just edits
        objCommitTask = asyncTask <> objectCommitTask obj
        in MkResource objRun MkAnObject {..}
