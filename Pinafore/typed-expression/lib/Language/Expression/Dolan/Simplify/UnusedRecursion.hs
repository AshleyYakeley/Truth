module Language.Expression.Dolan.Simplify.UnusedRecursion
    ( eliminateUnusedRecursion
    )
where

import Data.Shim
import Shapes

import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.FreeVars
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeSystem

elimUnusuedInShimWit ::
    forall (ground :: GroundTypeKind) polarity tv.
    (IsDolanGroundType ground, Is PolarityType polarity) =>
    TypeVarT tv ->
    DolanShimWit ground polarity tv ->
    DolanShimWit ground polarity tv
elimUnusuedInShimWit var tw@(MkShimWit t _) =
    if variableOccursIn var t
        then shimWitToDolan $ recursiveDolanShimWit var tw
        else tw

elimInSingularType ::
    forall (ground :: GroundTypeKind) polarity t.
    (IsDolanGroundType ground, Is PolarityType polarity) =>
    DolanSingularType ground polarity t ->
    DolanShimWit ground polarity t
elimInSingularType (RecursiveDolanSingularType var pt) = elimUnusuedInShimWit var $ elimInType pt
elimInSingularType t = shimWitToDolan $ mapDolanSingularType elimInType t

elimInType ::
    forall (ground :: GroundTypeKind) polarity t.
    (IsDolanGroundType ground, Is PolarityType polarity) =>
    DolanType ground polarity t ->
    DolanShimWit ground polarity t
elimInType NilDolanType = nilDolanShimWit
elimInType (ConsDolanType t1 tr) = joinMeetShimWit (elimInSingularType t1) (elimInType tr)

eliminateUnusedRecursion ::
    forall (ground :: GroundTypeKind) a.
    (IsDolanGroundType ground, PShimWitMappable (DolanShim ground) (DolanType ground) a) =>
    Endo a
eliminateUnusedRecursion = mapPShimWits @_ @(DolanType ground) elimInType elimInType
