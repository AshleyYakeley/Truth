module Language.Expression.Dolan.Solver.Safety where

import Data.Shim
import Language.Expression.Dolan.Solver.FlipType
import Language.Expression.Dolan.Solver.WholeConstraint
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

-- | just used for debugging
class CheckSafety t where
    checkSafety :: t -> Result RecursiveTypeError ()

instance forall (ground :: GroundTypeKind) sv polarity t. IsDolanGroundType ground =>
             CheckSafety (CCRPolarArgument (DolanType ground) polarity sv t) where
    checkSafety (CoCCRPolarArgument t) = checkSafety t
    checkSafety (ContraCCRPolarArgument t) = checkSafety t
    checkSafety (RangeCCRPolarArgument p q) = do
        checkSafety p
        checkSafety q

instance forall (ground :: GroundTypeKind) dv gt polarity t. IsDolanGroundType ground =>
             CheckSafety (CCRPolarArguments dv (DolanType ground) gt polarity t) where
    checkSafety NilCCRArguments = return ()
    checkSafety (ConsCCRArguments arg args) = do
        checkSafety arg
        checkSafety args

instance forall (ground :: GroundTypeKind) polarity t. IsDolanGroundType ground =>
             CheckSafety (DolanSingularType ground polarity t) where
    checkSafety (VarDolanSingularType _) = return ()
    checkSafety (RecursiveDolanSingularType var t) = do
        _ <- safeRecursiveDolanSingularType var t
        checkSafety t
    checkSafety (GroundedDolanSingularType (MkDolanGroundedType _ args)) = checkSafety args

instance forall (ground :: GroundTypeKind) polarity t. IsDolanGroundType ground =>
             CheckSafety (DolanType ground polarity t) where
    checkSafety NilDolanType = return ()
    checkSafety (ConsDolanType t1 tr) = do
        checkSafety t1
        checkSafety tr

instance forall (ground :: GroundTypeKind) polarity t. IsDolanGroundType ground =>
             CheckSafety (FlipType ground polarity t) where
    checkSafety (NormalFlipType t) = checkSafety t
    checkSafety (InvertFlipType t) = checkSafety t

instance forall (ground :: GroundTypeKind) t. IsDolanGroundType ground => CheckSafety (WholeConstraint ground t) where
    checkSafety (MkWholeConstraint tp tq) = do
        checkSafety tp
        checkSafety tq
