module Truth.Core.Resource.SingleRunner where

import Truth.Core.Import

data SingleRunner (t :: TransKind) where
    StaticSingleRunner
        :: forall (t :: TransKind). MonadTransAskUnlift t
        => IOWitness t
        -> UnliftAll MonadUnliftIO t
        -> SingleRunner t
    DynamicSingleRunner
        :: forall (t :: TransKind). MonadTransUnliftAll t
        => IO (WUnliftAll MonadUnliftIO t)
        -> SingleRunner t

instance TestEquality SingleRunner where
    testEquality (StaticSingleRunner wa _) (StaticSingleRunner wb _) = testEquality wa wb
    testEquality _ _ = Nothing

instance TestOrder SingleRunner where
    testOrder (StaticSingleRunner wa _) (StaticSingleRunner wb _) = testOrder wa wb
    testOrder (StaticSingleRunner _ _) (DynamicSingleRunner _) = WLT
    testOrder (DynamicSingleRunner _) _ = WGT

data SingleRunnerOrder (a :: TransKind) (b :: TransKind) where
    SRLT :: forall a b. SingleRunnerOrder a b
    SREQ
        :: forall a. MonadTransAskUnlift a
        => SingleRunnerOrder a a
    SRGT :: forall a b. SingleRunnerOrder a b

singleRunnerOrder :: SingleRunner a -> SingleRunner b -> SingleRunnerOrder a b
singleRunnerOrder (StaticSingleRunner wa _) (StaticSingleRunner wb _) =
    case testOrder wa wb of
        WLT -> SRLT
        WEQ -> SREQ
        WGT -> SRGT
singleRunnerOrder (StaticSingleRunner _ _) (DynamicSingleRunner _) = SRLT
singleRunnerOrder (DynamicSingleRunner _) _ = SRGT

runSingleRunner :: forall t r. SingleRunner t -> (MonadTransUnliftAll t => UnliftAll MonadUnliftIO t -> r) -> r
runSingleRunner (StaticSingleRunner _ run) call = call run
runSingleRunner (DynamicSingleRunner iorun) call =
    call $ \tma -> do
        MkWUnliftAll run <- liftIO iorun
        run tma

singleRunnerUnliftAllDict :: SingleRunner t -> Dict (MonadTransUnliftAll t)
singleRunnerUnliftAllDict (StaticSingleRunner _ _) = Dict
singleRunnerUnliftAllDict (DynamicSingleRunner _) = Dict

singleRunnerComposeDict ::
       forall (ct :: TransKind -> Constraint) (t :: TransKind). (forall t'. MonadTransUnliftAll t' => ct t')
    => SingleRunner t
    -> Compose Dict ct t
singleRunnerComposeDict sr =
    case singleRunnerUnliftAllDict sr of
        Dict -> Compose Dict
