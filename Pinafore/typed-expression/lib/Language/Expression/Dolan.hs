{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan
    ( module Control.Applicative.Wrapped
    , PShimWit
    , MPolarityType(..)
    , MPolarW(..)
    , MPolarRangeType(..)
    , isMPolarity
    , InvertMPolarW(..)
    , invertMPolarity
    , toMPolar
    , toMPolarWM
    , mapMPolarW
    , forMPolarW
    , fromMPolarSingle
    , GroundTypeKind
    , DolanPolyShim
    , DolanPolyIsoShim
    , DolanVariance
    , DolanVarianceKind
    , DolanVarianceType
    , DolanVarianceMap(..)
    , DolanVarianceInCategory(..)
    , dolanVarianceInKind
    , HasDolanVariance(..)
    , CovaryType
    , covaryCoercibleKind
    , CovaryMap
    , HasCovaryMap(..)
    , covaryToDolanVarianceType
    , dolanVarianceToCovaryType
    , covaryToDolanVarianceMap
    , DolanSingularType(..)
    , DolanType(..)
    , singleDolanType
    , dolanTypeToSingular
    , DolanShimWit
    , singleDolanShimWit
    , nilDolanShimWit
    , joinMeetShimWit
    , varDolanShimWit
    , unrollRecursiveType
    , unToRangeShimWit
    , unFromRangeShimWit
    , biRangeAnyF
    , SingleArgument
    , DolanArguments(..)
    , ArgTypeF(..)
    , mapArgsTypeF
    , saturateArgsConstraint
    , dolanArgumentsToArguments
    , SubtypeContext(..)
    , subtypeDolanArguments
    , invertTypeMaybe
    , invertType
    , DolanTypeSystem
    , IsDolanGroundType(..)
    , IsDolanFunctionGroundType(..)
    , IsDolanSubtypeGroundType(..)
    , IsDolanSubtypeEntriesGroundType(..)
    , SubtypeArguments(..)
    , SubtypeConversion(..)
    , simpleSubtypeConversion
    , nilSubtypeConversion
    , idSubtypeConversion
    , composeSubtypeConversion
    , SubtypeConversionEntry(..)
    , simpleSubtypeConversionEntry
    , saturateGroundType
    , module Language.Expression.Dolan.Nonpolar
    , module Language.Expression.Dolan.Mono
    ) where

import Control.Applicative.Wrapped
import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.Covariance
import Language.Expression.Dolan.Invert
import Language.Expression.Dolan.MPolarity
import Language.Expression.Dolan.Mono
import Language.Expression.Dolan.Nonpolar
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.RangeF
import Language.Expression.Dolan.Rename ()
import Language.Expression.Dolan.Simplify ()
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unifier (invertType)
import Language.Expression.Dolan.Unroll
import Language.Expression.Dolan.Variance
import Shapes

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground =>
             AbstractTypeSystem (DolanTypeSystem ground) where
    type TSInner (DolanTypeSystem ground) = DolanM ground

class (Eq (DolanVarID ground), IsDolanSubtypeGroundType ground) => IsDolanFunctionGroundType (ground :: GroundTypeKind) where
    functionGroundType :: ground '[ ContraCCRVariance, CoCCRVariance] (->)

instance forall (ground :: GroundTypeKind). IsDolanFunctionGroundType ground =>
             CompleteTypeSystem (DolanTypeSystem ground) where
    tsFunctionPosWitness ta tb =
        singleDolanShimWit $
        mkPolarShimWit $
        GroundedDolanSingularType functionGroundType $ ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments
    tsFunctionNegWitness ta tb =
        singleDolanShimWit $
        mkPolarShimWit $
        GroundedDolanSingularType functionGroundType $ ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments
