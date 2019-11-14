module Truth.Core.Resource.SingleRunner where

import Truth.Core.Import

data SingleRunner (t :: TransKind) where
    StaticSingleRunner
        :: forall (t :: TransKind). MonadTransUnliftAll t
        => IOWitness t
        -> UnliftAll t
        -> SingleRunner t
    DynamicSingleRunner
        :: forall (t :: TransKind). MonadTransUnliftAll t
        => IO (WUnliftAll t)
        -> SingleRunner t

instance TestEquality SingleRunner where
    testEquality (StaticSingleRunner wa _) (StaticSingleRunner wb _) = testEquality wa wb
    testEquality _ _ = Nothing

instance TestOrder SingleRunner where
    testOrder (StaticSingleRunner wa _) (StaticSingleRunner wb _) = testOrder wa wb
    testOrder (StaticSingleRunner _ _) (DynamicSingleRunner _) = WLT
    testOrder (DynamicSingleRunner _) _ = WGT

runSingleRunner :: forall t r. SingleRunner t -> (MonadTransUnliftAll t => UnliftAll t -> r) -> r
runSingleRunner (StaticSingleRunner _ run) call = call run
runSingleRunner (DynamicSingleRunner iorun) call =
    call $ \tma -> do
        MkWUnliftAll run <- liftIO iorun
        run tma

singleRunnerUnliftAllDict :: SingleRunner t -> Dict (MonadTransUnliftAll t)
singleRunnerUnliftAllDict (StaticSingleRunner _ _) = Dict
singleRunnerUnliftAllDict (DynamicSingleRunner _) = Dict

stateSingleRunner :: s -> SingleRunner (StateT s)
stateSingleRunner s =
    DynamicSingleRunner $ do
        var <- newMVar s
        return $ wMVarRun var
