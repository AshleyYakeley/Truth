module Truth.Core.Object.Cache where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Object.UnliftIO
import Truth.Core.Read

cacheObject ::
       forall edit. CacheableEdit edit
    => Int
    -> Object edit
    -> LifeCycleIO (Object edit)
cacheObject mus (MkCloseUnliftIO unlift (MkAnObject read push)) = do
    runAction <-
        asyncWaitRunner mus $ \editsnl -> runTransform unlift $ pushOrFail "cached object" noEditSource $ push editsnl
    cacheVar <- liftIO $ newMVar $ cacheEmpty @ListCache @(EditCacheKey ListCache edit)
    return $ let
        objRun = mvarUnliftIO cacheVar
        objRead :: MutableRead (StateT (ListCache (EditCacheKey ListCache edit)) IO) (EditReader edit)
        objRead rt = do
            oldcache <- get
            case editCacheLookup @edit rt oldcache of
                Just t -> return t
                Nothing -> do
                    t <- liftIO $ runTransform unlift $ read rt
                    liftIO $ runAction Nothing -- still reading, don't push yet
                    editCacheAdd @edit rt t
                    return t
        objEdit edits =
            return $
            Just $ \_ -> do
                editCacheUpdates edits
                liftIO $ runAction $ Just edits
        in MkCloseUnliftIO objRun MkAnObject {..}
