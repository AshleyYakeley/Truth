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
    , DolanVariance
    , DolanVarianceKind
    , DolanVarianceType
    , DolanVarianceMap(..)
    , HasDolanVary(..)
    , CovaryType
    , CovaryMap
    , covarymap
    , covaryToDolanVarianceMap
    , DolanSingularType(..)
    , DolanType(..)
    , singleDolanType
    , dolanTypeToSingular
    , DolanShimWit
    , singleDolanShimWit
    , joinMeetShimWit
    , varDolanShimWit
    , unrollRecursiveType
    , unToRangeShimWit
    , unFromRangeShimWit
    , biRangeAnyF
    , SingleArgument
    , DolanArguments(..)
    , saturateArgsConstraint
    , dolanArgumentsToArguments
    , SubtypeContext(..)
    , subtypeDolanArguments
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
    , SubypeConversionEntry(..)
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

class (Eq (DolanName ground), IsDolanSubtypeGroundType ground) => IsDolanFunctionGroundType (ground :: GroundTypeKind) where
    functionGroundType :: ground '[ 'Contravariance, 'Covariance] (->)

instance forall (ground :: GroundTypeKind). IsDolanFunctionGroundType ground =>
             CompleteTypeSystem (DolanTypeSystem ground) where
    tsFunctionPosWitness ta tb =
        singleDolanShimWit $
        mkShimWit $
        GroundDolanSingularType functionGroundType $ ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments
    tsFunctionNegWitness ta tb =
        singleDolanShimWit $
        mkShimWit $
        GroundDolanSingularType functionGroundType $ ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments
