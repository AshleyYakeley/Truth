module Truth.Core.Resource.Runnable where

import Truth.Core.Import
import Truth.Core.Resource.Function
import Truth.Core.Resource.ResourceRunner

data Resource1 f (a :: k) =
    forall (tt :: [TransKind]). MkResource1 (ResourceRunner tt)
                                            (f tt a)

class MapResource (f :: [TransKind] -> k) where
    mapResource ::
           forall tt1 tt2. (MonadTransStackUnliftAll tt1, MonadTransStackUnliftAll tt2)
        => TransListFunction tt1 tt2
        -> KindFunction (f tt1) (f tt2)

joinResource1_ ::
       forall k1 k2 f1 f2 (a1 :: k1) (a2 :: k2) r. (InKind a1, InKind a2, MapResource f1, MapResource f2)
    => (forall tt. ResourceRunner tt -> f1 tt a1 -> f2 tt a2 -> r)
    -> Resource1 f1 a1
    -> Resource1 f2 a2
    -> r
joinResource1_ ff (MkResource1 (run1 :: ResourceRunner tt1) fma1) (MkResource1 (run2 :: ResourceRunner tt2) fma2) =
    case resourceRunnerUnliftAllDict run1 of
        Dict ->
            case resourceRunnerUnliftAllDict run2 of
                Dict ->
                    combineResourceRunners run1 run2 $ \run12 tf1 tf2 ->
                        case resourceRunnerUnliftAllDict run12 of
                            Dict ->
                                ff
                                    run12
                                    ((unNestedMorphism $ mapResource tf1) fma1)
                                    ((unNestedMorphism $ mapResource tf2) fma2)

joinResource1 ::
       forall k1 k2 k3 f1 f2 f3 (a1 :: k1) (a2 :: k2) (a3 :: k3). (InKind a1, InKind a2, MapResource f1, MapResource f2)
    => (forall tt. MonadTransStackUnliftAll tt => f1 tt a1 -> f2 tt a2 -> f3 tt a3)
    -> Resource1 f1 a1
    -> Resource1 f2 a2
    -> Resource1 f3 a3
joinResource1 ff =
    joinResource1_ $ \run f1 f2 ->
        MkResource1 run $
        case resourceRunnerUnliftAllDict run of
            Dict -> ff f1 f2

runResource ::
       forall m f a r. MonadUnliftIO m
    => Resource1 f a
    -> (forall tt. (MonadTransStackUnliftAll tt, MonadUnliftIO (ApplyStack tt m)) => f tt a -> ApplyStack tt m r)
    -> m r
runResource (MkResource1 (rr :: _ tt) f) call =
    runResourceRunnerWith rr $
    case transStackDict @MonadUnliftIO @tt @m of
        Dict -> \unlift -> unlift $ call @tt f

exclusiveResource :: (MapResource f, InKind a) => Resource1 f a -> LifeCycleIO (Resource1 f a)
exclusiveResource (MkResource1 trun f) = do
    Dict <- return $ resourceRunnerUnliftAllDict trun
    trun' <- exclusiveResourceRunner trun
    return $ MkResource1 trun' $ unNestedMorphism (mapResource stackTransListFunction) f
