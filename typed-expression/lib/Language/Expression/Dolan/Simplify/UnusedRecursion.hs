module Language.Expression.Dolan.Simplify.UnusedRecursion
    ( eliminateUnusedRecursion
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.MapType
import Language.Expression.Dolan.Occur
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Recursive
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

elimUnusuedInShimWit ::
       forall (ground :: GroundTypeKind) polarity name t. (IsDolanGroundType ground, Is PolarityType polarity)
    => SymbolType name
    -> DolanShimWit ground polarity t
    -> DolanShimWit ground polarity (Recursive (UVar Type name) t)
elimUnusuedInShimWit var tw@(MkShimWit t _) =
    if occursInType var t
        then singleDolanShimWit $ recursiveDolanShimWit var tw
        else mapShimWit (isoPolarForwards recursiveIsoNull) tw

elimInSingularType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity t
    -> DolanShimWit ground polarity t
elimInSingularType (RecursiveDolanSingularType var pt) = elimUnusuedInShimWit var $ elimInType (Just var) pt
elimInSingularType t = singleDolanShimWit $ mapDolanSingularType (elimInType Nothing) t

elimInType ::
       forall (ground :: GroundTypeKind) polarity name t. (IsDolanGroundType ground, Is PolarityType polarity)
    => Maybe (SymbolType name)
    -> DolanType ground polarity t
    -> DolanShimWit ground polarity t
elimInType _ NilDolanType = nilDolanShimWit
elimInType mn@(Just rn) (ConsDolanType (VarDolanSingularType n) tr)
    | Just Refl <- testEquality rn n = joinMeetShimWit (unsafeDeleteVarShimWit n) (elimInType mn tr)
elimInType mn (ConsDolanType t1 tr) = joinMeetShimWit (elimInSingularType t1) (elimInType mn tr)

eliminateUnusedRecursion ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanGroundType ground, PShimWitMappable (DolanPolyShim ground Type) (DolanType ground) a)
    => a
    -> a
eliminateUnusedRecursion = mapPShimWits @_ @(DolanType ground) (elimInType Nothing) (elimInType Nothing)
