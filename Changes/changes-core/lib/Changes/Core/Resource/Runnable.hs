module Changes.Core.Resource.Runnable
    ( Resource (..)
    , joinResource_
    , joinResource
    , runResource
    , runResourceUnlift
    , runResourceLifecycle
    , runResourceContext
    , exclusiveResource
    )
where

import Changes.Core.Import
import Changes.Core.Resource.ResourceRunner

data Resource (f :: Type -> Type)
    = forall t. MkResource
        (ResourceRunner t)
        (f t)

joinResource_ ::
    forall f1 f2 r.
    (Contravariant f1, Contravariant f2) =>
    (forall t. ResourceRunner t -> f1 t -> f2 t -> r) ->
    Resource f1 ->
    Resource f2 ->
    r
joinResource_ ff (MkResource run1 fma1) (MkResource run2 fma2) =
    ff
        (liftA2 (,) run1 run2)
        (contramap fst fma1)
        (contramap snd fma2)

joinResource ::
    forall f1 f2 f3.
    (Contravariant f1, Contravariant f2) =>
    (forall t. f1 t -> f2 t -> f3 t) ->
    Resource f1 ->
    Resource f2 ->
    Resource f3
joinResource ff =
    joinResource_ $ \run f1 f2 ->
        MkResource run $ ff f1 f2

runResource ::
    forall f r.
    ResourceContext ->
    Resource f ->
    (forall t. f t -> ReaderT t IO r) ->
    IO r
runResource rc (MkResource rr ft) call =
    runResourceRunner rc rr $ runReaderT $ call ft

runResourceUnlift ::
    forall f r.
    Contravariant f =>
    ResourceContext ->
    Resource f ->
    (f () -> IO r) ->
    IO r
runResourceUnlift rc resource call = runResource rc resource $ \ft -> do
    t <- ask
    liftIO $ call $ contramap (const t) ft

runResourceLifecycle ::
    forall f.
    Contravariant f =>
    ResourceContext ->
    Resource f ->
    LifecycleT IO IO (f ())
runResourceLifecycle rc resource = lifecycleWith $ runResourceUnlift rc resource

runResourceContext ::
    forall f r.
    ResourceContext ->
    Resource f ->
    ( forall t.
      ResourceContext ->
      (ReaderT t IO --> IO) ->
      f t ->
      IO r
    ) ->
    IO r
runResourceContext rc (MkResource rr ft) call =
    runResourceRunnerContext rc rr $ \rc' t -> call rc' (`runReaderT` t) ft

exclusiveResource ::
    forall f.
    ResourceContext ->
    Resource f ->
    LifecycleT IO IO (Resource f)
exclusiveResource rc (MkResource trun f) = do
    trun' <- exclusiveResourceRunner rc trun
    return $ MkResource trun' f
