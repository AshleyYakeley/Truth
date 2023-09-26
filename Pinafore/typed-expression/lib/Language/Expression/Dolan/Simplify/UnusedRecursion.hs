module Language.Expression.Dolan.Simplify.UnusedRecursion
    ( eliminateUnusedRecursion
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.FreeVars
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

elimUnusuedInShimWit ::
       forall (ground :: GroundTypeKind) polarity tv. (IsDolanGroundType ground, Is PolarityType polarity)
    => TypeVarT tv
    -> DolanShimWit ground polarity tv
    -> DolanShimWit ground polarity tv
elimUnusuedInShimWit var tw@(MkShimWit t _) =
    if variableOccursIn var t
        then shimWitToDolan $ recursiveDolanShimWit var tw
        else tw

elimInSingularType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity t
    -> DolanShimWit ground polarity t
elimInSingularType (RecursiveDolanSingularType var pt) = elimUnusuedInShimWit var $ elimInType (Just var) pt
elimInSingularType t = shimWitToDolan $ mapDolanSingularType (elimInType Nothing) t

elimInType ::
       forall (ground :: GroundTypeKind) polarity tv t. (IsDolanGroundType ground, Is PolarityType polarity)
    => Maybe (TypeVarT tv)
    -> DolanType ground polarity t
    -> DolanShimWit ground polarity t
elimInType _ NilDolanType = nilDolanShimWit
elimInType mn@(Just rn) (ConsDolanType (VarDolanSingularType n) tr)
    | Just Refl <- testEquality rn n = joinMeetShimWit (unsafeDeleteVarShimWit n) (elimInType mn tr)
elimInType mn (ConsDolanType t1 tr) = joinMeetShimWit (elimInSingularType t1) (elimInType mn tr)

eliminateUnusedRecursion ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanGroundType ground, PShimWitMappable (DolanShim ground) (DolanType ground) a)
    => Endo a
eliminateUnusedRecursion = mapPShimWits @_ @(DolanType ground) (elimInType Nothing) (elimInType Nothing)
