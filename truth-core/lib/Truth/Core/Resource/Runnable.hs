module Truth.Core.Resource.Runnable where

import Truth.Core.Import
import Truth.Core.Resource.Function
import Truth.Core.Resource.ResourceRunner

data Resource f =
    forall (tt :: [TransKind]). MkResource (ResourceRunner tt)
                                           (f tt)

class MapResource (f :: [TransKind] -> Type) where
    mapResource ::
           forall tt1 tt2. (MonadTransStackUnliftAll tt1, MonadTransStackUnliftAll tt2)
        => TransListFunction tt1 tt2
        -> f tt1
        -> f tt2

joinResource_ ::
       forall f1 f2 r. (MapResource f1, MapResource f2)
    => (forall tt. ResourceRunner tt -> f1 tt -> f2 tt -> r)
    -> Resource f1
    -> Resource f2
    -> r
joinResource_ ff (MkResource (run1 :: ResourceRunner tt1) fma1) (MkResource (run2 :: ResourceRunner tt2) fma2) =
    case resourceRunnerUnliftAllDict run1 of
        Dict ->
            case resourceRunnerUnliftAllDict run2 of
                Dict ->
                    combineResourceRunners run1 run2 $ \run12 tf1 tf2 ->
                        case resourceRunnerUnliftAllDict run12 of
                            Dict -> ff run12 (mapResource tf1 fma1) (mapResource tf2 fma2)

joinResource ::
       forall f1 f2 f3. (MapResource f1, MapResource f2)
    => (forall tt. MonadTransStackUnliftAll tt => f1 tt -> f2 tt -> f3 tt)
    -> Resource f1
    -> Resource f2
    -> Resource f3
joinResource ff =
    joinResource_ $ \run f1 f2 ->
        MkResource run $
        case resourceRunnerUnliftAllDict run of
            Dict -> ff f1 f2

runResource ::
       forall f r.
       Resource f
    -> (forall tt. (MonadTransStackUnliftAll tt, MonadUnliftIO (ApplyStack tt IO)) => StackUnliftAll tt -> f tt -> r)
    -> r
runResource (MkResource rr ftt) call = runResourceRunnerWith rr $ \run -> call run ftt

exclusiveResource :: (MapResource f) => Resource f -> LifeCycleIO (Resource f)
exclusiveResource (MkResource trun f) = do
    Dict <- return $ resourceRunnerUnliftAllDict trun
    trun' <- exclusiveResourceRunner trun
    return $ MkResource trun' $ mapResource stackTransListFunction f
