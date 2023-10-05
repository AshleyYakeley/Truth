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

data Equation (ground :: GroundTypeKind) where
    MkEquation
        :: forall (ground :: GroundTypeKind) polarity t.
           PolarityType polarity
        -> TypeVarT t
        -> DolanType ground polarity t
        -> Equation ground

type Extracter (ground :: GroundTypeKind) = Writer [Equation ground]

class ExtractEquations (ground :: GroundTypeKind) (t :: Type) where
    extractEquations :: t -> Extracter ground t

instance forall (ground :: GroundTypeKind) ft polarity sv t. ( Is PolarityType polarity
         , forall polarity' t'. Is PolarityType polarity' => ExtractEquations ground (ft polarity' t')
         ) => ExtractEquations ground (CCRPolarArgument ft polarity sv t) where
    extractEquations (CoCCRPolarArgument t) = fmap CoCCRPolarArgument $ extractEquations t
    extractEquations (ContraCCRPolarArgument t) =
        withInvertPolarity @polarity $ fmap ContraCCRPolarArgument $ extractEquations t
    extractEquations (RangeCCRPolarArgument p q) =
        withInvertPolarity @polarity $ liftA2 RangeCCRPolarArgument (extractEquations p) (extractEquations q)

instance forall (ground :: GroundTypeKind) (w :: CCRArgumentKind) dv gt t. (forall sv a.
                                                                                    ExtractEquations ground (w sv a)) =>
             ExtractEquations ground (CCRArguments w dv gt t) where
    extractEquations NilCCRArguments = return NilCCRArguments
    extractEquations (ConsCCRArguments arg1 argr) =
        liftA2 ConsCCRArguments (extractEquations arg1) (extractEquations argr)

instance forall (ground :: GroundTypeKind) polarity t. Is PolarityType polarity =>
             ExtractEquations ground (DolanGroundedType ground polarity t) where
    extractEquations (MkDolanGroundedType g args) = fmap (MkDolanGroundedType g) $ extractEquations args

instance forall (ground :: GroundTypeKind) polarity t. Is PolarityType polarity =>
             ExtractEquations ground (DolanSingularType ground polarity t) where
    extractEquations (GroundedDolanSingularType t) = fmap GroundedDolanSingularType $ extractEquations t
    extractEquations t@(VarDolanSingularType _) = return t
    extractEquations (RecursiveDolanSingularType var t) = do
        t' <- extractEquations t
        tell $ pure $ MkEquation representative var t'
        return $ VarDolanSingularType var

instance forall (ground :: GroundTypeKind) polarity t. Is PolarityType polarity =>
             ExtractEquations ground (DolanType ground polarity t) where
    extractEquations NilDolanType = pure NilDolanType
    extractEquations (ConsDolanType t1 tr) = liftA2 ConsDolanType (extractEquations t1) (extractEquations tr)

automateRecursionInType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> DolanTypeCheckM ground (DolanShimWit ground polarity t)
automateRecursionInType t
    | hasRecursion t = do
        t' <- unEndoM (varRename fixedRenameSource) t
        let (_t0, _eqns) = runWriter $ extractEquations @ground t'
        return $ mkShimWit t'
automateRecursionInType t = return $ mkShimWit t

automateRecursion ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanGroundType ground, PShimWitMappable (DolanShim ground) (DolanType ground) a)
    => EndoM (DolanTypeCheckM ground) a
automateRecursion = mapPShimWitsM automateRecursionInType automateRecursionInType
