{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan
    ( PShimWit
    , MPolarityType(..)
    , MPolarW(..)
    , MPolarRangeType(..)
    , isMPolarity
    , InvertMPolarW(..)
    , invertMPolarity
    , toMPolar
    , toMPolarWM
    , fromMPolarSingle
    , GroundTypeKind
    , DolanPolyShim
    , DolanVariance
    , DolanVarianceKind
    , DolanVarianceType
    , DolanVarianceMap(..)
    , dolanVary
    , CovaryType
    , CovaryMap
    , covarymap
    , covaryToDolanVarianceMap
    , DolanSingularType(..)
    , DolanType(..)
    , singleDolanType
    , singleDolanShimWit
    , joinMeetDolanShimWit
    , unToRangeShimWit
    , unFromRangeShimWit
    , biRangeAnyF
    , SingleArgument
    , DolanArguments(..)
    , dolanArgumentsToArguments
    , SubtypeContext(..)
    , subtypeDolanArguments
    , DolanTypeSystem
    , dolanSimplifyTypes
    , DolanName
    , IsDolanGroundType(..)
    , IsDolanFunctionGroundType(..)
    , IsDolanSubtypeGroundType(..)
    , module Language.Expression.Dolan.Nonpolar
    , module Language.Expression.Dolan.Concrete
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Concrete
import Language.Expression.Dolan.Covariance
import Language.Expression.Dolan.MPolarity
import Language.Expression.Dolan.Nonpolar
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.RangeF
import Language.Expression.Dolan.Rename ()
import Language.Expression.Dolan.Simplify
import Language.Expression.Dolan.Subsume
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unify
import Language.Expression.Dolan.Variance
import Shapes

class (Eq (DolanName ground), IsDolanSubtypeGroundType ground) => IsDolanFunctionGroundType (ground :: GroundTypeKind) where
    functionGroundType :: ground '[ 'Contravariance, 'Covariance] (->)

instance forall (ground :: GroundTypeKind). IsDolanFunctionGroundType ground => TypeSystem (DolanTypeSystem ground) where
    type TSRenamer (DolanTypeSystem ground) = VarRenamerT (DolanTypeSystem ground)
    type TSUnifier (DolanTypeSystem ground) = DolanUnifier ground
    type TSScoped (DolanTypeSystem ground) = DolanM ground
    type TSSubsumer (DolanTypeSystem ground) = DolanSubsumer ground
    tsFunctionPosWitness ta tb =
        singleDolanShimWit $
        mkShimWit $
        GroundDolanSingularType functionGroundType $ ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments
    tsFunctionNegWitness ta tb =
        singleDolanShimWit $
        mkShimWit $
        GroundDolanSingularType functionGroundType $ ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments
