module Changes.Core.Resource.Runnable
    ( Resource (..)
    , MapResource (..)
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

data Resource (f :: (Type -> Type) -> Type)
    = forall (tt :: [Type]). MkResource
        (ResourceRunner tt)
        (f (ReaderT (ListProduct tt) IO))

class MapResource (f :: (Type -> Type) -> Type) where
    mapResource ::
        forall m1 m2.
        (Monad m1, Monad m2) =>
        (m1 --> m2) ->
        f m1 ->
        f m2

joinResource_ ::
    forall f1 f2 r.
    (MapResource f1, MapResource f2) =>
    (forall tt. ResourceRunner tt -> f1 (ReaderT (ListProduct tt) IO) -> f2 (ReaderT (ListProduct tt) IO) -> r) ->
    Resource f1 ->
    Resource f2 ->
    r
joinResource_ ff (MkResource (run1 :: ResourceRunner tt1) fma1) (MkResource (run2 :: ResourceRunner tt2) fma2) =
    combineResourceRunners run1 run2 $ \(run12 :: ResourceRunner tt12) tf1 tf2 ->
        ff
            run12
            (mapResource (withReaderT tf1) fma1)
            (mapResource (withReaderT tf2) fma2)

joinResource ::
    forall f1 f2 f3.
    (MapResource f1, MapResource f2) =>
    (forall m. Monad m => f1 m -> f2 m -> f3 m) ->
    Resource f1 ->
    Resource f2 ->
    Resource f3
joinResource ff =
    joinResource_ $ \(run :: ResourceRunner tt) f1 f2 ->
        MkResource run $ ff f1 f2

runResource ::
    forall f r.
    ResourceContext ->
    Resource f ->
    (forall m. MonadUnliftIO m => f m -> m r) ->
    IO r
runResource rc (MkResource (rr :: ResourceRunner tt) ftt) call =
    runResourceRunner rc rr $ runReaderT $ call @(ReaderT (ListProduct tt) IO) ftt

runResourceUnlift ::
    forall f r.
    MapResource f =>
    ResourceContext ->
    Resource f ->
    (f IO -> IO r) ->
    IO r
runResourceUnlift rc resource call = runResource rc resource $ \fm -> liftIOWithUnlift $ \unlift -> call $ mapResource unlift fm

runResourceLifecycle ::
    forall f.
    MapResource f =>
    ResourceContext ->
    Resource f ->
    LifecycleT IO IO (f IO)
runResourceLifecycle rc resource = lifecycleWith $ runResourceUnlift rc resource

runResourceContext ::
    forall f r.
    ResourceContext ->
    Resource f ->
    ( forall tt.
      ResourceContext ->
      (ReaderT (ListProduct tt) IO --> IO) ->
      f (ReaderT (ListProduct tt) IO) ->
      IO r
    ) ->
    IO r
runResourceContext rc (MkResource (rr :: ResourceRunner tt) ftt) call =
    runResourceRunnerContext rc rr $ \rc' tt -> call @tt rc' (`runReaderT` tt) ftt

exclusiveResource ::
    forall f.
    MapResource f =>
    ResourceContext ->
    Resource f ->
    LifecycleT IO IO (Resource f)
exclusiveResource rc (MkResource (trun :: ResourceRunner tt) f) = do
    trun' <- exclusiveResourceRunner rc trun
    return $ MkResource trun' $ mapResource (withReaderT fst) f
