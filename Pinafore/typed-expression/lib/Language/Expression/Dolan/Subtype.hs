module Language.Expression.Dolan.Subtype where

import Data.Shim
import Shapes

import Language.Expression.Dolan.Solver.CrumbleM
import Language.Expression.Dolan.SubtypeChain
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeResult
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeSystem

type IsDolanSubtypeGroundType :: GroundTypeKind -> Constraint
class IsDolanGroundType ground => IsDolanSubtypeGroundType ground where
    shouldMerge :: DolanVarID ground -> DolanVarID ground -> Bool

getSubtypeChain :: forall (ground :: GroundTypeKind). DolanTypeM ground (GetSubtypeChain ground)
getSubtypeChain = ask

throwTypeError :: forall (ground :: GroundTypeKind) a. TypeError ground -> DolanTypeM ground a
throwTypeError = throwExc

liftTypeResult ::
    forall (ground :: GroundTypeKind).
    TypeResult ground
        --> DolanTypeM ground
liftTypeResult = lift

liftCrumbleMRigidity ::
    forall (ground :: GroundTypeKind).
    (String -> NameRigidity) ->
    CrumbleM ground
        --> DolanRenameTypeM ground
liftCrumbleMRigidity rigidity cra =
    tunnel $ \tun -> do
        gsc <- getSubtypeChain
        liftTypeResult $ runReaderT (tun $ runCrumbleM rigidity cra) gsc

liftCrumbleM ::
    forall (ground :: GroundTypeKind).
    CrumbleM ground
        --> DolanRenameTypeM ground
liftCrumbleM cra = do
    rigidity <- renamerGetNameRigidity
    liftCrumbleMRigidity rigidity cra

getSubtypeChainRenamed ::
    forall (ground :: GroundTypeKind) (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb).
    IsDolanGroundType ground =>
    GetSubtypeChain ground ->
    ground dva gta ->
    ground dvb gtb ->
    CrumbleM ground (SubtypeChain ground dva gta dvb gtb)
getSubtypeChainRenamed gsc ga gb = do
    chain <- liftResultToCrumbleM $ unGetSubtypeChain gsc ga gb
    liftToCrumbleM $ unEndoM (renameType @(DolanTypeSystem ground) [] FreeName) chain
