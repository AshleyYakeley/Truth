module Language.Expression.Dolan.Solver.CrumbleM where

import Language.Expression.Common
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeResult
import Language.Expression.Dolan.TypeSystem
import Shapes

newtype CrumbleM (ground :: GroundTypeKind) a = MkCrumbleM
    { unCrumbleM :: ReaderT (String -> NameRigidity) (ComposeInner (TypeResult ground) (DolanTypeCheckM ground)) a
    }

deriving newtype instance
         forall (ground :: GroundTypeKind) . IsDolanGroundType ground =>
                                             Functor (CrumbleM ground)

deriving newtype instance
         forall (ground :: GroundTypeKind) . IsDolanGroundType ground =>
                                             Applicative (CrumbleM ground)

deriving newtype instance
         forall (ground :: GroundTypeKind) . IsDolanGroundType ground =>
                                             Monad (CrumbleM ground)

deriving newtype instance
         forall (ground :: GroundTypeKind) . (IsDolanGroundType ground,
                                              MonadException (DolanM ground)) =>
                                             MonadException (CrumbleM ground)

deriving newtype instance
         forall (ground :: GroundTypeKind) . (IsDolanGroundType ground,
                                              MonadIO (DolanM ground)) =>
                                             MonadIO (CrumbleM ground)

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => MonadThrow (TypeError ground) (CrumbleM ground) where
    throw err = liftResultToCrumbleM $ throw err

liftFullToCrumbleMWithUnlift ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => ((forall r. CrumbleM ground r -> DolanTypeCheckM ground (TypeResult ground r)) -> DolanTypeCheckM ground (TypeResult ground a))
    -> CrumbleM ground a
liftFullToCrumbleMWithUnlift call =
    MkCrumbleM $ do
        MkWUnlift unlift <- askUnlift
        lift $ MkComposeInner $ call $ unComposeInner . unlift . unCrumbleM

liftToCrumbleM ::
       forall (ground :: GroundTypeKind). IsDolanGroundType ground
    => DolanTypeCheckM ground --> CrumbleM ground
liftToCrumbleM tca = MkCrumbleM $ lift $ lift tca

liftResultToCrumbleM ::
       forall (ground :: GroundTypeKind). IsDolanGroundType ground
    => TypeResult ground --> CrumbleM ground
liftResultToCrumbleM rea = MkCrumbleM $ lift $ liftInner rea

runCrumbleMResult ::
       forall (ground :: GroundTypeKind) a.
       (String -> NameRigidity)
    -> CrumbleM ground a
    -> DolanTypeCheckM ground (TypeResult ground a)
runCrumbleMResult rigidity ca = unComposeInner $ runReaderT (unCrumbleM ca) rigidity

joinFirstCrumbleM ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => CrumbleM ground a
    -> CrumbleM ground a
    -> CrumbleM ground a
joinFirstCrumbleM (MkCrumbleM ca) (MkCrumbleM cb) =
    MkCrumbleM $ do
        MkWUnlift unlift <- askUnlift
        lift $ MkComposeInner $ liftA2 joinFirstResult (unComposeInner $ unlift ca) (unComposeInner $ unlift cb)

firstCrumbleM ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => NonEmpty (CrumbleM ground a)
    -> CrumbleM ground a
firstCrumbleM (ca :| []) = ca
firstCrumbleM (ca :| (b:bb)) = joinFirstCrumbleM ca $ firstCrumbleM $ b :| bb

forFirstCrumbleM ::
       forall (ground :: GroundTypeKind) a b. IsDolanGroundType ground
    => NonEmpty a
    -> (a -> CrumbleM ground b)
    -> CrumbleM ground b
forFirstCrumbleM l f = firstCrumbleM $ fmap f l

crumbleMRigidity ::
       forall (ground :: GroundTypeKind). IsDolanGroundType ground
    => CrumbleM ground (String -> NameRigidity)
crumbleMRigidity = MkCrumbleM ask
