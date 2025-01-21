module Changes.Core.Resource.SingleRunner
    ( SingleRunner
    , mkSingleRunner
    , singleRunnerUnliftDict
    , discardingSingleRunner
    , runSingleRunner
    , runSingleRunnerContext
    )
where

import Changes.Core.Import

data SingleRunner (t :: TransKind) where
    MkSingleRunner ::
        forall (t :: TransKind).
        MonadTransUnlift t =>
        IOWitness t ->
        Unlift MonadUnliftIO t ->
        SingleRunner t

instance TestEquality SingleRunner where
    testEquality (MkSingleRunner wa _) (MkSingleRunner wb _) = testEquality wa wb

instance TestOrder SingleRunner where
    testCompare (MkSingleRunner wa _) (MkSingleRunner wb _) = testCompare wa wb

mkSingleRunner ::
    forall (t :: TransKind).
    MonadTransUnlift t =>
    IOWitness t ->
    Unlift MonadUnliftIO t ->
    SingleRunner t
mkSingleRunner = MkSingleRunner

singleRunnerUnliftDict :: SingleRunner t -> Dict (MonadTransUnlift t)
singleRunnerUnliftDict (MkSingleRunner _ _) = Dict

discardingSingleRunner :: SingleRunner t -> SingleRunner t
discardingSingleRunner (MkSingleRunner w run) = MkSingleRunner w $ toDiscardingUnlift run

mkAnySingleRunner :: MonadTransUnlift t => IOWitness t -> WUnlift MonadUnliftIO t -> SingleRunner t
mkAnySingleRunner wit (MkWUnlift unlift) = MkSingleRunner wit unlift

fetchInSomeList :: TestEquality w => [Some w] -> w t -> Maybe (w t, w t -> [Some w])
fetchInSomeList [] _ = Nothing
fetchInSomeList (MkSome it : aa) wt
    | Just Refl <- testEquality it wt = Just (it, \it' -> MkSome it' : aa)
fetchInSomeList (a : aa) wt = do
    (it, f) <- fetchInSomeList aa wt
    return (it, \it' -> a : f it')

fetchSingleRunner ::
    forall t.
    [Some SingleRunner] ->
    SingleRunner t ->
    (WUnlift MonadUnliftIO t -> [Some SingleRunner], WUnlift MonadUnliftIO t, Bool)
fetchSingleRunner rr sr@(MkSingleRunner swit srun) =
    case fetchInSomeList rr sr of
        Nothing -> (\unlift -> (MkSome $ mkAnySingleRunner swit unlift) : rr, MkWUnlift srun, True)
        Just (MkSingleRunner cwit crun, f) -> (\unlift -> f (mkAnySingleRunner cwit unlift), MkWUnlift crun, False)

runSingleRunner ::
    forall t m r.
    MonadUnliftIO m =>
    [Some SingleRunner] ->
    SingleRunner t ->
    ((MonadTransUnlift t, MonadUnliftIO (t m)) => t m r) ->
    m r
runSingleRunner rr sr call =
    case singleRunnerUnliftDict sr of
        Dict ->
            case hasTransConstraint @MonadUnliftIO @t @m of
                Dict -> let
                    (_, MkWUnlift run, _) = fetchSingleRunner rr sr
                    in run call

runSingleRunnerContext ::
    forall t m r.
    MonadUnliftIO m =>
    [Some SingleRunner] ->
    SingleRunner t ->
    ((MonadTransUnlift t, MonadUnliftIO (t m)) => [Some SingleRunner] -> Unlift MonadUnliftIO t -> m r) ->
    m r
runSingleRunnerContext rr sr call =
    case singleRunnerUnliftDict sr of
        Dict ->
            case hasTransConstraint @MonadUnliftIO @t @m of
                Dict -> let
                    (rr', run, isRunner) = fetchSingleRunner rr sr
                    in case isRunner of
                        True -> unWUnlift run $ liftWithUnlift $ \unlift -> call (rr' $ MkWUnlift unlift) unlift
                        False -> call (rr' run) $ unWUnlift run
