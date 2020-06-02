{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan
    ( module I
    , module Language.Expression.Dolan
    ) where

import Data.Shim
import Language.Expression.Dolan.Arguments as I
import Language.Expression.Dolan.Bisubstitute as I
import Language.Expression.Dolan.Covariance as I
import Language.Expression.Dolan.MPolarity as I
import Language.Expression.Dolan.PShimWit as I
import Language.Expression.Dolan.RangeF as I
import Language.Expression.Dolan.Rename ()
import Language.Expression.Dolan.Simplify as I
import Language.Expression.Dolan.Subsume as I
import Language.Expression.Dolan.Subtype as I
import Language.Expression.Dolan.Type as I
import Language.Expression.Dolan.TypeSystem as I
import Language.Expression.Dolan.Unify as I
import Language.Expression.Dolan.Variance as I
import Language.Expression.Renamer
import Language.Expression.TypeSystem
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
