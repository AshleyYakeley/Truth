{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan
    ( module I
    , MPolarityType(..)
    , MPolarW(..)
    , MPolarRangeType(..)
    , isMPolarity
    , InvertMPolarW(..)
    , invertMPolarity
    , toMPolar
    , toMPolarWM
    , mapMPolarW
    , mapMPolarWM
    , forMPolarW
    , fromMPolarSingle
    , GroundTypeKind
    , TypeError(..)
    , DolanPolyShim
    , DolanShim
    , DolanPolyIsoShim
    , invertTypeM
    , CCRVariances
    , CCRVariancesKind
    , CCRVariancesType
    , CCRVariancesMap(..)
    , lazyCCRVariancesMap
    , CCRVariancesShim(..)
    , HasCCRVariances(..)
    , CovaryType
    , covaryCoercibleKind
    , CovaryMap(..)
    , HasCovaryMap(..)
    , covaryToCCRVariancesType
    , ccrVariancesToCovaryType
    , covaryToCCRVariancesMap
    , ccrVariancesMapToCovary
    , DolanGroundedType(..)
    , DolanSingularType(..)
    , DolanType(..)
    , unrollRecursiveType
    , unrollTopType
    , unToRangeShimWit
    , unFromRangeShimWit
    , invertType
    , DolanTypeSystem
    , IsDolanGroundType(..)
    , IsDolanFunctionGroundType(..)
    , IsDolanSubtypeGroundType(..)
    , IsDolanSubtypeEntriesGroundType(..)
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.FreeVars as I
import Language.Expression.Dolan.Invert
import Language.Expression.Dolan.Mono as I
import Language.Expression.Dolan.Nonpolar as I
import Language.Expression.Dolan.Rename ()
import Language.Expression.Dolan.Simplify ()
import Language.Expression.Dolan.Subtype as I
import Language.Expression.Dolan.SubtypeEntry as I
import Language.Expression.Dolan.Type as I
import Language.Expression.Dolan.TypeResult
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unroll
import Shapes

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground =>
             AbstractTypeSystem (DolanTypeSystem ground) where
    type TSInner (DolanTypeSystem ground) = DolanM ground
    bottomShimWit = MkSome $ mkShimWit NilDolanType
    cleanOpenExpression expr@(ClosedExpression _) = return expr
    cleanOpenExpression expr@(OpenExpression _ (ClosedExpression _)) = return expr
    cleanOpenExpression expr = do
        let
            combineWits ::
                   forall a b.
                   DolanVarWit ground a
                -> DolanVarWit ground b
                -> _ (Maybe (Expression (DolanVarWit ground) (a, b)))
            combineWits (MkNameWitness na ta) (MkNameWitness nb tb) =
                return $
                if shouldMerge @ground na nb
                    then Just $ fmap (\(MkMeetType ab) -> ab) $ varExpression $ MkNameWitness na $ joinMeetShimWit ta tb
                    else Nothing
        expr' <- combineExpressionWitnessesM combineWits expr
        return $ reverseExpression expr'

class (Eq (DolanVarID ground), IsDolanSubtypeGroundType ground) => IsDolanFunctionGroundType (ground :: GroundTypeKind) where
    functionGroundType :: ground '[ ContraCCRVariance, CoCCRVariance] (->)

instance forall (ground :: GroundTypeKind). IsDolanFunctionGroundType ground =>
             CompleteTypeSystem (DolanTypeSystem ground) where
    tsFunctionPosWitness ta tb =
        shimWitToDolan $
        mkPolarShimWit $
        MkDolanGroundedType functionGroundType $
        ConsCCRArguments (ContraCCRPolarArgument ta) $ ConsCCRArguments (CoCCRPolarArgument tb) NilCCRArguments
    tsFunctionNegWitness ta tb =
        shimWitToDolan $
        mkPolarShimWit $
        MkDolanGroundedType functionGroundType $
        ConsCCRArguments (ContraCCRPolarArgument ta) $ ConsCCRArguments (CoCCRPolarArgument tb) NilCCRArguments
