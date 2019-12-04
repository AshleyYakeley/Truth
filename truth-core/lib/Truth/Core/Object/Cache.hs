module Truth.Core.Object.Cache where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Read
import Truth.Core.Resource

cacheObject ::
       forall edit. CacheableEdit edit
    => Int
    -> Object edit
    -> LifeCycleIO (Object edit)
cacheObject mus (MkResource1 rr (MkAnObject read push)) =
    runResourceRunnerWith rr $ \run -> do
        runAction <- asyncWaitRunner mus $ \editsnl -> run $ pushOrFail "cached object" noEditSource $ push editsnl
        objRun <- liftIO $ stateResourceRunner $ cacheEmpty @ListCache @(EditCacheKey ListCache edit)
        return $ let
            objRead :: MutableRead (StateT (ListCache (EditCacheKey ListCache edit)) IO) (EditReader edit)
            objRead rt = do
                oldcache <- get
                case editCacheLookup @edit rt oldcache of
                    Just t -> return t
                    Nothing -> do
                        t <- liftIO $ run $ read rt
                        liftIO $ runAction Nothing -- still reading, don't push yet
                        editCacheAdd @edit rt t
                        return t
            objEdit edits =
                return $
                Just $ \_ -> do
                    editCacheUpdates edits
                    liftIO $ runAction $ Just edits
            in MkResource1 objRun MkAnObject {..}
