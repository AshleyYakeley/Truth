module Language.Expression.Dolan.Simplify.AutomateRecursion
    ( automateRecursion
    , automateRecursionInType
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Rename ()
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

class HasRecursion t where
    hasRecursion :: t -> Bool

instance (forall polarity' t'. HasRecursion (ft polarity' t')) => HasRecursion (CCRPolarArgument ft polarity sv t) where
    hasRecursion (CoCCRPolarArgument t) = hasRecursion t
    hasRecursion (ContraCCRPolarArgument t) = hasRecursion t
    hasRecursion (RangeCCRPolarArgument p q) = hasRecursion p || hasRecursion q

instance forall (w :: CCRArgumentKind) dv gt t. (forall sv a. HasRecursion (w sv a)) =>
             HasRecursion (CCRArguments w dv gt t) where
    hasRecursion NilCCRArguments = False
    hasRecursion (ConsCCRArguments arg1 argr) = hasRecursion arg1 || hasRecursion argr

instance forall (ground :: GroundTypeKind) polarity t. HasRecursion (DolanGroundedType ground polarity t) where
    hasRecursion (MkDolanGroundedType _ args) = hasRecursion args

instance forall (ground :: GroundTypeKind) polarity t. HasRecursion (DolanSingularType ground polarity t) where
    hasRecursion (GroundedDolanSingularType t) = hasRecursion t
    hasRecursion (VarDolanSingularType _) = False
    hasRecursion (RecursiveDolanSingularType _ _) = True

instance forall (ground :: GroundTypeKind) polarity t. HasRecursion (DolanType ground polarity t) where
    hasRecursion NilDolanType = False
    hasRecursion (ConsDolanType t1 tr) = hasRecursion t1 || hasRecursion tr

automateRecursionInType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> DolanTypeCheckM ground (DolanShimWit ground polarity t)
automateRecursionInType t
    | hasRecursion t = do
        t' <- unEndoM (varRename fixedRenameSource) t
        return $ mkShimWit t'
automateRecursionInType t = return $ mkShimWit t

automateRecursion ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanGroundType ground, PShimWitMappable (DolanShim ground) (DolanType ground) a)
    => EndoM (DolanTypeCheckM ground) a
automateRecursion = mapPShimWitsM automateRecursionInType automateRecursionInType
