module Truth.Core.Object.Cache where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Object.Run
import Truth.Core.Read

cacheObject ::
       forall edit. CacheableEdit edit
    => Int
    -> Object edit
    -> LifeCycleIO (Object edit)
cacheObject mus (MkRunnableIO unlift (MkAnObject read push)) = do
    runAction <-
        asyncWaitRunner mus $ \editsnl -> runWMFunction unlift $ pushOrFail "cached object" noEditSource $ push editsnl
    cacheVar <- liftIO $ newMVar $ cacheEmpty @ListCache @(EditCacheKey ListCache edit)
    return $ let
        objRun = mVarWIORun cacheVar
        objRead :: MutableRead (StateT (ListCache (EditCacheKey ListCache edit)) IO) (EditReader edit)
        objRead rt = do
            oldcache <- get
            case editCacheLookup @edit rt oldcache of
                Just t -> return t
                Nothing -> do
                    t <- liftIO $ runWMFunction unlift $ read rt
                    liftIO $ runAction Nothing -- still reading, don't push yet
                    editCacheAdd @edit rt t
                    return t
        objEdit edits =
            return $
            Just $ \_ -> do
                editCacheUpdates edits
                liftIO $ runAction $ Just edits
        in MkRunnableIO objRun MkAnObject {..}
