module Truth.Core.Reference.Cache where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Reference.EditContext
import Truth.Core.Reference.Reference
import Truth.Core.Resource

cacheReference ::
       forall edit. CacheableEdit edit
    => ResourceContext
    -> Int
    -> Reference edit
    -> LifeCycleIO (ResourceContext -> Reference edit)
cacheReference rc mus obj = do
    (runAction, asyncTask) <-
        asyncWaitRunner mus $ \editsnl ->
            runResource rc obj $ \anobj -> pushOrFail "cached reference" noEditSource $ refEdit anobj editsnl
    objRun <- liftIO $ stateResourceRunner $ cacheEmpty @ListCache @(EditCacheKey ListCache edit)
    return $ \rc' -> let
        refRead :: Readable (StateT (ListCache (EditCacheKey ListCache edit)) IO) (EditReader edit)
        refRead rt = do
            oldcache <- get
            case editCacheLookup @edit rt oldcache of
                Just t -> return t
                Nothing -> do
                    t <- liftIO $ runResource rc' obj $ \(MkAReference read _ _) -> read rt
                    liftIO $ runAction Nothing -- still reading, don't push yet
                    editCacheAdd @edit rt t
                    return t
        refEdit edits =
            return $
            Just $ \_ -> do
                editCacheUpdates edits
                liftIO $ runAction $ Just edits
        refCommitTask = asyncTask <> referenceCommitTask obj
        in MkResource objRun MkAReference {..}