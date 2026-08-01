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
import Changes.Core.Resource.Function
import Changes.Core.Resource.ResourceRunner

data Resource (f :: (Type -> Type) -> Type)
    = forall (tt :: [TransKind]). MkResource
        (ResourceRunner tt)
        (f (ApplyStack tt IO))

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
    (forall tt. ResourceRunner tt -> f1 (ApplyStack tt IO) -> f2 (ApplyStack tt IO) -> r) ->
    Resource f1 ->
    Resource f2 ->
    r
joinResource_ ff (MkResource (run1 :: ResourceRunner tt1) fma1) (MkResource (run2 :: ResourceRunner tt2) fma2) =
    case resourceRunnerUnliftDict run1 of
        Dict ->
            case resourceRunnerUnliftDict run2 of
                Dict ->
                    combineResourceRunners run1 run2 $ \(run12 :: ResourceRunner tt12) tf1 tf2 ->
                        case resourceRunnerUnliftDict run12 of
                            Dict ->
                                case transStackDict @Monad @tt1 @IO of
                                    Dict ->
                                        case transStackDict @Monad @tt2 @IO of
                                            Dict ->
                                                case transStackDict @Monad @tt12 @IO of
                                                    Dict ->
                                                        ff
                                                            run12
                                                            (mapResource (tlfFunction tf1 (Proxy @IO)) fma1)
                                                            (mapResource (tlfFunction tf2 (Proxy @IO)) fma2)

joinResource ::
    forall f1 f2 f3.
    (MapResource f1, MapResource f2) =>
    (forall m. Monad m => f1 m -> f2 m -> f3 m) ->
    Resource f1 ->
    Resource f2 ->
    Resource f3
joinResource ff =
    joinResource_ $ \(run :: ResourceRunner tt) f1 f2 ->
        MkResource run
            $ case resourceRunnerUnliftDict run of
                Dict ->
                    case transStackDict @Monad @tt @IO of
                        Dict -> ff f1 f2

runResource ::
    forall f m r.
    MonadUnliftIO m =>
    ResourceContext ->
    Resource f ->
    (forall tt. (MonadTransStackUnlift tt, MonadUnliftIO (ApplyStack tt m)) => f (ApplyStack tt IO) -> ApplyStack tt m r) ->
    m r
runResource rc (MkResource (rr :: ResourceRunner tt) ftt) call =
    runResourceRunner rc rr $ call @tt ftt

runResourceUnlift ::
    forall f m r.
    (MapResource f, MonadUnliftIO m) =>
    ResourceContext ->
    Resource f ->
    (f IO -> m r) ->
    m r
runResourceUnlift rc resource call = let
    call' ::
        forall tt.
        MonadTransStackUnlift tt =>
        f (ApplyStack tt IO) ->
        ApplyStack tt m r
    call' ftt =
        case transStackDict @Monad @tt @IO of
            Dict -> unStackT @tt $ liftWithUnlift $ \unlift -> call $ mapResource (unlift . MkStackT) ftt
    in runResource @f @m @r rc resource $ \ @tt ftt -> call' @tt ftt

runResourceLifecycle ::
    forall f m.
    (MapResource f, MonadCoroutine m, MonadAskUnliftIO m) =>
    ResourceContext ->
    Resource f ->
    LifecycleT m m (f IO)
runResourceLifecycle rc resource = lifecycleWith $ runResourceUnlift rc resource

runResourceContext ::
    forall f m r.
    MonadUnliftIO m =>
    ResourceContext ->
    Resource f ->
    ( forall tt.
      (MonadTransStackUnlift tt, MonadUnliftIO (ApplyStack tt m)) =>
      ResourceContext -> StackUnlift tt -> f (ApplyStack tt IO) -> m r
    ) ->
    m r
runResourceContext rc (MkResource (rr :: ResourceRunner tt) ftt) call =
    runResourceRunnerContext rc rr $ \rc' run -> call @tt rc' run ftt

exclusiveResource ::
    forall f m.
    (MapResource f, MonadCoroutine m, MonadAskUnliftIO m) =>
    ResourceContext ->
    Resource f ->
    LifecycleT m m (Resource f)
exclusiveResource rc (MkResource (trun :: ResourceRunner tt) f) = do
    Dict <- return $ resourceRunnerUnliftDict trun
    Dict <- return $ transStackDict @Monad @tt @IO
    trun' <- exclusiveResourceRunner rc trun
    return $ MkResource trun' $ mapResource MkStackT f
