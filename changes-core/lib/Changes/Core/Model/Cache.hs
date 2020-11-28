module Changes.Core.Model.Cache where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Model.EditContext
import Changes.Core.Model.Reference
import Changes.Core.Read
import Changes.Core.Resource
import Changes.Debug

cacheReference ::
       forall edit. CacheableEdit edit
    => ResourceContext
    -> Int
    -> Reference edit
    -> LifeCycle (ResourceContext -> Reference edit)
cacheReference rc mus obj = do
    (runAction, asyncTask) <-
        asyncWaitRunner mus $ \editsnl ->
            runResource rc obj $ \anobj -> pushOrFail "cached reference" noEditSource $ refEdit anobj editsnl
    objRun <- liftIO $ stateResourceRunner $ cacheEmpty @ListCache @(EditCacheKey ListCache edit)
    return $ \rc' -> let
        refRead :: Readable (StateT (ListCache (EditCacheKey ListCache edit)) IO) (EditReader edit)
        refRead rt = traceBracket "cache read" $ do
            oldcache <- get
            case editCacheLookup @edit rt oldcache of
                Just t -> do
                    traceIOM "cache hit"
                    return t
                Nothing -> traceBracket "cache miss" $ do
                    t <- liftIO $ runResource rc' obj $ \(MkAReference read _ _) -> read rt
                    liftIO $ runAction Nothing -- still reading, don't push yet
                    editCacheAdd @edit rt t
                    return t
        refEdit edits =
            traceBracket "cache update request" $
            return $
            Just $ \_ -> traceBracket "cache update action" $ do
                editCacheUpdates edits
                liftIO $ runAction $ Just edits
        refCommitTask = asyncTask <> referenceCommitTask obj
        in MkResource objRun MkAReference {..}
