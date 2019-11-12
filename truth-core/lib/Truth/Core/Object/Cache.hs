module Truth.Core.Object.Cache where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Read

cacheObject ::
       forall edit. CacheableEdit edit
    => Int
    -> Object edit
    -> LifeCycleIO (Object edit)
cacheObject mus (MkRunnable1 trun (MkAnObject read push)) =
    runMonoTransStackRunner @IO trun $ \run -> do
        runAction <- asyncWaitRunner mus $ \editsnl -> run $ pushOrFail "cached object" noEditSource $ push editsnl
        cacheVar <- liftIO $ newMVar $ cacheEmpty @ListCache @(EditCacheKey ListCache edit)
        return $ let
            objRun :: TransStackRunner '[ StateT (ListCache (EditCacheKey ListCache edit))]
            objRun = mVarTransStackRunner cacheVar
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
            in MkRunnable1 objRun MkAnObject {..}
