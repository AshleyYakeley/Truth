{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Dolan.Subtype
    ( SubtypeContext(..)
    , subtypeDolanArguments
    , IsDolanSubtypeGroundType(..)
    ) where

import Data.Shim
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Variance
import Shapes

type SubtypeContext :: (Polarity -> Type -> Type) -> ShimKind Type -> (Type -> Type) -> Polarity -> Polarity -> Type
data SubtypeContext w shim m pola polb = MkSubtypeContext
    { subtypeTypes :: forall ta tb. w pola ta -> w polb tb -> m (shim ta tb)
    , subtypeInverted :: SubtypeContext w shim m (InvertPolarity polb) (InvertPolarity pola)
    }

subtypeVariance ::
       forall (w :: Polarity -> Type -> Type) (shim :: ShimKind Type) m pola polb sv a b.
       (Applicative m, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext w shim m pola polb
    -> VarianceType sv
    -> SingleArgument sv w pola a
    -> SingleArgument sv w polb b
    -> m (VarianceCategory shim sv a b)
subtypeVariance sc CovarianceType ta tb = subtypeTypes sc ta tb
subtypeVariance sc ContravarianceType ta tb = do
    ba <- subtypeTypes (subtypeInverted sc) tb ta
    return $ MkCatDual ba
subtypeVariance sc RangevarianceType (MkRangeType tpa tqa) (MkRangeType tpb tqb) = do
    pba <- subtypeTypes (subtypeInverted sc) tpb tpa
    qab <- subtypeTypes sc tqa tqb
    return $ MkCatRange pba qab

subtypeArguments ::
       forall (w :: Polarity -> Type -> Type) (pshim :: PolyShimKind) m pola polb dv (gta :: DolanVarianceKind dv) (gtb :: DolanVarianceKind dv) ta tb.
       (DolanVarianceInCategory pshim, Applicative m, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext w (pshim Type) m pola polb
    -> DolanVarianceType dv
    -> DolanVarianceMap dv gta
    -> DolanVarianceMap dv gtb
    -> DolanArguments dv w gta pola ta
    -> DolanArguments dv w gtb polb tb
    -> m (pshim (DolanVarianceKind dv) gta gtb -> pshim Type ta tb)
subtypeArguments _ NilListType NilDolanVarianceMap NilDolanVarianceMap NilDolanArguments NilDolanArguments = pure id
subtypeArguments sc (ConsListType svt dvt) (ConsDolanVarianceMap dvma) (ConsDolanVarianceMap dvmb) (ConsDolanArguments sta dta) (ConsDolanArguments stb dtb) =
    case applyFunctionKindWitness (inKind @_ @gta) sta of
        Dict ->
            case applyFunctionKindWitness (inKind @_ @gtb) stb of
                Dict ->
                    case applyFunctionKindWitness (inKind @_ @gta) stb of
                        Dict ->
                            case varianceCoercibleKind svt of
                                Dict ->
                                    case dolanVarianceInCategory @pshim dvt of
                                        Dict -> do
                                            sfunc <- subtypeVariance sc svt sta stb
                                            f <- subtypeArguments sc dvt dvma dvmb dta dtb
                                            pure $ \conv -> f (applyPolyShim svt conv sfunc)

subtypeDolanArguments ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) m pola polb dv gt argsa argsb.
       ( IsDolanGroundType ground
       , DolanVarianceInCategory pshim
       , Applicative m
       , Is PolarityType pola
       , Is PolarityType polb
       )
    => SubtypeContext (DolanType ground) (pshim Type) m pola polb
    -> ground dv gt
    -> DolanArguments dv (DolanType ground) gt pola argsa
    -> DolanArguments dv (DolanType ground) gt polb argsb
    -> m (pshim Type argsa argsb)
subtypeDolanArguments sc gt argsa argsb = let
    vkt = groundTypeVarianceType gt
    dvm = groundTypeVarianceMap gt
    in case dolanVarianceMapInKind dvm of
           Dict ->
               case dolanVarianceInCategory @pshim vkt of
                   Dict -> fmap (\f -> f cid) $ subtypeArguments sc vkt dvm dvm argsa argsb

class (IsDolanGroundType ground, MonadPlus (DolanM ground)) => IsDolanSubtypeGroundType (ground :: GroundTypeKind) where
    type DolanM ground :: Type -> Type
    subtypeGroundTypes ::
           forall m pola polb dva gta a dvb gtb b. (Applicative m, Is PolarityType pola, Is PolarityType polb)
        => MFunction (DolanM ground) m
        -> SubtypeContext (DolanType ground) (DolanPolyShim ground Type) m pola polb
        -> ground dva gta
        -> DolanArguments dva (DolanType ground) gta pola a
        -> ground dvb gtb
        -> DolanArguments dvb (DolanType ground) gtb polb b
        -> m (DolanPolyShim ground Type a b)
    throwTypeRecursiveError ::
           Is PolarityType polarity => SymbolType name -> DolanPlainType ground polarity t -> DolanM ground a
    throwTypeConvertInverseError ::
           DolanPlainType ground 'Negative p -> DolanPlainType ground 'Positive q -> DolanM ground a
    throwTypeSubsumeError ::
           Is PolarityType polarity
        => DolanSingularType ground polarity tinf
        -> DolanPlainType ground polarity tdecl
        -> DolanM ground a
    throwTypeNotInvertible :: Is PolarityType polarity => DolanType ground polarity t -> DolanM ground a
