module Language.Expression.Dolan.Solver.CrumbleM where

import Shapes

import Language.Expression.Dolan.SubtypeChain
import Language.Expression.Dolan.TypeResult
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeSystem

newtype CrumbleM (ground :: GroundTypeKind) a = MkCrumbleM
    { unCrumbleM :: ReaderT () (DolanRenameTypeM ground) a
    }
    deriving newtype (Functor, Applicative, Monad, MonadException)

instance forall (ground :: GroundTypeKind). MonadThrow (TypeError ground) (CrumbleM ground) where
    throw = throwExc

instance forall (ground :: GroundTypeKind). MonadCatch (TypeError ground) (CrumbleM ground) where
    catch = catchExc

liftFullToCrumbleMWithUnlift ::
    forall (ground :: GroundTypeKind) a.
    ((forall r. CrumbleM ground r -> DolanRenameTypeM ground r) -> DolanRenameTypeM ground a) ->
    CrumbleM ground a
liftFullToCrumbleMWithUnlift call =
    MkCrumbleM $ do
        MkWUnlift unlift <- askUnlift
        lift $ call $ unlift . unCrumbleM

liftToCrumbleM ::
    forall (ground :: GroundTypeKind).
    DolanRenameTypeM ground --> CrumbleM ground
liftToCrumbleM tca = MkCrumbleM $ lift tca

liftResultToCrumbleM ::
    forall (ground :: GroundTypeKind).
    TypeResult ground --> CrumbleM ground
liftResultToCrumbleM rea = liftToCrumbleM $ lift $ lift rea

runCrumbleM ::
    forall (ground :: GroundTypeKind) a.
    () ->
    CrumbleM ground a ->
    DolanRenameTypeM ground a
runCrumbleM ccRigidity ca = runReaderT (unCrumbleM ca) ccRigidity

runCrumbleMResult ::
    forall (ground :: GroundTypeKind) a.
    () ->
    CrumbleM ground a ->
    DolanRenameTypeM ground (TypeResult ground a)
runCrumbleMResult ccRigidity ca = tryExc $ runCrumbleM ccRigidity ca

runCrumbleMCheck ::
    forall (ground :: GroundTypeKind) t a.
    VarRenameable t =>
    t ->
    CrumbleM ground a ->
    DolanRenameTypeM ground (Maybe a)
runCrumbleMCheck e ca = do
    ta <- lift $ runVarRenamerT (renameableVars e) [] $ runCrumbleMResult () ca
    return $ resultToMaybe ta

joinFirstCrumbleM ::
    forall (ground :: GroundTypeKind) a.
    CrumbleM ground a ->
    CrumbleM ground a ->
    CrumbleM ground a
joinFirstCrumbleM (MkCrumbleM ca) (MkCrumbleM cb) =
    MkCrumbleM $ do
        MkWUnlift unlift <- askUnlift
        lift $ joinTypeCheckResult (unlift ca) (unlift cb)

firstCrumbleM ::
    forall (ground :: GroundTypeKind) a.
    NonEmpty (CrumbleM ground a) ->
    CrumbleM ground a
firstCrumbleM (ca :| []) = ca
firstCrumbleM (ca :| (b : bb)) = joinFirstCrumbleM ca $ firstCrumbleM $ b :| bb

forFirstCrumbleM ::
    forall (ground :: GroundTypeKind) a b.
    NonEmpty a ->
    (a -> CrumbleM ground b) ->
    CrumbleM ground b
forFirstCrumbleM l f = firstCrumbleM $ fmap f l

crumbleMRigidity ::
    forall (ground :: GroundTypeKind).
    CrumbleM ground (String -> NameRigidity)
crumbleMRigidity = MkCrumbleM $ lift renamerGetNameRigidity

crumbleMGetSubtypeChain ::
    forall (ground :: GroundTypeKind).
    CrumbleM ground (GetSubtypeChain ground)
crumbleMGetSubtypeChain = MkCrumbleM $ lift $ lift ask
