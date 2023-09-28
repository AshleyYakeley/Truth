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
    , DolanPolyShim
    , DolanPolyIsoShim
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
    , joinMeetShimWit
    , unrollRecursiveType
    , unToRangeShimWit
    , unFromRangeShimWit
    , biRangeSomeFor
    , SubtypeContext(..)
    , invertType
    , DolanTypeSystem
    , IsDolanGroundType(..)
    , IsDolanFunctionGroundType(..)
    , IsDolanSubtypeGroundType(..)
    , IsDolanSubtypeEntriesGroundType(..)
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.FreeVars as I
import Language.Expression.Dolan.Mono as I
import Language.Expression.Dolan.Nonpolar as I
import Language.Expression.Dolan.Rename ()
import Language.Expression.Dolan.Simplify ()
import Language.Expression.Dolan.Solver (invertType)
import Language.Expression.Dolan.Subtype as I
import Language.Expression.Dolan.SubtypeEntry as I
import Language.Expression.Dolan.Type as I
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unroll
import Shapes

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground =>
             AbstractTypeSystem (DolanTypeSystem ground) where
    type TSInner (DolanTypeSystem ground) = DolanM ground
    bottomShimWit = MkSome $ mkShimWit NilDolanType

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
