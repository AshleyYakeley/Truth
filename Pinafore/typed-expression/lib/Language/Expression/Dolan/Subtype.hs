module Language.Expression.Dolan.Subtype where

import Data.Shim
import Shapes

import Language.Expression.Dolan.SubtypeChain
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeResult
import Language.Expression.Dolan.TypeSystem

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

getSubtypeChainRenamed ::
    forall (ground :: GroundTypeKind) (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb).
    IsDolanGroundType ground =>
    GetSubtypeChain ground ->
    ground dva gta ->
    ground dvb gtb ->
    DolanRenameTypeM ground (SubtypeChain ground dva gta dvb gtb)
getSubtypeChainRenamed gsc ga gb = do
    chain <- lift $ lift $ unGetSubtypeChain gsc ga gb
    renameSubtypeChain chain
