module Changes.Core.Resource.SingleRunner
    ( SingleRunner
    , mkSingleRunner
    , singleRunnerUnliftAllDict
    , discardingSingleRunner
    , runSingleRunner
    , runSingleRunnerContext
    ) where

import Changes.Core.Import

data SingleRunner (t :: TransKind) where
    MkSingleRunner
        :: forall (t :: TransKind). MonadTransUnliftAll t
        => IOWitness t
        -> UnliftAll MonadUnliftIO t
        -> SingleRunner t

instance TestEquality SingleRunner where
    testEquality (MkSingleRunner wa _) (MkSingleRunner wb _) = testEquality wa wb

instance TestOrder SingleRunner where
    testOrder (MkSingleRunner wa _) (MkSingleRunner wb _) = testOrder wa wb

mkSingleRunner ::
       forall (t :: TransKind). MonadTransUnliftAll t
    => IOWitness t
    -> UnliftAll MonadUnliftIO t
    -> SingleRunner t
mkSingleRunner = MkSingleRunner

singleRunnerUnliftAllDict :: SingleRunner t -> Dict (MonadTransUnliftAll t)
singleRunnerUnliftAllDict (MkSingleRunner _ _) = Dict

discardingSingleRunner :: SingleRunner t -> SingleRunner t
discardingSingleRunner (MkSingleRunner w run) = MkSingleRunner w $ discardingRunner run

mkAnySingleRunner :: MonadTransUnliftAll t => IOWitness t -> WUnliftAll MonadUnliftIO t -> SingleRunner t
mkAnySingleRunner wit (MkWUnliftAll unlift) = MkSingleRunner wit unlift

fetchInAnyWList :: TestEquality w => [AnyW w] -> w t -> Maybe (w t, w t -> [AnyW w])
fetchInAnyWList [] _ = Nothing
fetchInAnyWList (MkAnyW it:aa) wt
    | Just Refl <- testEquality it wt = Just (it, \it' -> MkAnyW it' : aa)
fetchInAnyWList (a:aa) wt = do
    (it, f) <- fetchInAnyWList aa wt
    return (it, \it' -> a : f it')

fetchSingleRunner ::
       forall t.
       [AnyW SingleRunner]
    -> SingleRunner t
    -> (WUnliftAll MonadUnliftIO t -> [AnyW SingleRunner], WUnliftAll MonadUnliftIO t, Bool)
fetchSingleRunner rr sr@(MkSingleRunner swit srun) =
    case fetchInAnyWList rr sr of
        Nothing -> (\unlift -> (MkAnyW $ mkAnySingleRunner swit unlift) : rr, MkWUnliftAll srun, True)
        Just (MkSingleRunner cwit crun, f) -> (\unlift -> f (mkAnySingleRunner cwit unlift), MkWUnliftAll crun, False)

runSingleRunner ::
       forall t m r. MonadUnliftIO m
    => [AnyW SingleRunner]
    -> SingleRunner t
    -> ((MonadTransUnliftAll t, MonadUnliftIO (t m)) => t m r)
    -> m r
runSingleRunner rr sr call =
    case singleRunnerUnliftAllDict sr of
        Dict ->
            case hasTransConstraint @MonadUnliftIO @t @m of
                Dict -> let
                    (_, MkWUnliftAll run, _) = fetchSingleRunner rr sr
                    in run call

runSingleRunnerContext ::
       forall t m r. MonadUnliftIO m
    => [AnyW SingleRunner]
    -> SingleRunner t
    -> ((MonadTransUnliftAll t, MonadUnliftIO (t m)) => [AnyW SingleRunner] -> UnliftAll MonadUnliftIO t -> m r)
    -> m r
runSingleRunnerContext rr sr call =
    case singleRunnerUnliftAllDict sr of
        Dict ->
            case hasTransConstraint @MonadUnliftIO @t @m of
                Dict -> let
                    (rr', run, isRunner) = fetchSingleRunner rr sr
                    in case isRunner of
                           True ->
                               runWUnliftAll run $
                               liftWithUnliftAll $ \unlift -> call (rr' $ MkWUnliftAll unlift) unlift
                           False -> call (rr' run) $ runWUnliftAll run
