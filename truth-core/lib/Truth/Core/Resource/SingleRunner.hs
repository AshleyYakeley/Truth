module Truth.Core.Resource.SingleRunner where

import Truth.Core.Import

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

runSingleRunner :: SingleRunner t -> UnliftAll MonadUnliftIO t
runSingleRunner (MkSingleRunner _ run) = run

singleRunnerUnliftAllDict :: SingleRunner t -> Dict (MonadTransUnliftAll t)
singleRunnerUnliftAllDict (MkSingleRunner _ _) = Dict

singleRunnerComposeDict ::
       forall (ct :: TransKind -> Constraint) (t :: TransKind). (forall t'. MonadTransUnliftAll t' => ct t')
    => SingleRunner t
    -> Compose Dict ct t
singleRunnerComposeDict sr =
    case singleRunnerUnliftAllDict sr of
        Dict -> Compose Dict

discardingSingleRunner :: SingleRunner t -> SingleRunner t
discardingSingleRunner (MkSingleRunner w run) = MkSingleRunner w $ discardingRunner run
