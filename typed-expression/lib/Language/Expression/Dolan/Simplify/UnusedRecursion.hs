module Language.Expression.Dolan.Simplify.UnusedRecursion
    ( eliminateUnusedRecursion
    ) where

import Data.Shim
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

elimInPlainType ::
       forall (ground :: GroundTypeKind) polarity name t. (IsDolanGroundType ground, Is PolarityType polarity)
    => Maybe (SymbolType name)
    -> DolanPlainType ground polarity t
    -> DolanSemiIsoPlainShimWit ground polarity t
elimInPlainType _ NilDolanPlainType = nilDolanPlainShimWit
elimInPlainType mn@(Just rn) (ConsDolanPlainType (VarDolanSingularType n) tr)
    | Just Refl <- testEquality rn n = joinMeetSemiIsoShimWit (unsafeDeleteVarPlainShimWit n) (elimInPlainType mn tr)
elimInPlainType mn (ConsDolanPlainType t1 tr) =
    consDolanPlainShimWit (mapDolanSingularType elimInType t1) (elimInPlainType mn tr)

elimUnusuedInShimWit ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanSemiIsoShimWit ground polarity t
    -> DolanSemiIsoShimWit ground polarity t
elimUnusuedInShimWit (MkShimWit t conv)
    | Just pt <- dolanTypeToPlainNonrec t = MkShimWit (PlainDolanType pt) conv
elimUnusuedInShimWit t = t

elimInType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> DolanSemiIsoShimWit ground polarity t
elimInType (PlainDolanType pt) = chainShimWit (mkShimWit . PlainDolanType) $ elimInPlainType Nothing pt
elimInType (RecursiveDolanType var pt) = elimUnusuedInShimWit $ recursiveMapType (elimInPlainType (Just var)) var pt

eliminateUnusedRecursion ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanGroundType ground, PShimWitMappable (DolanPolyShim ground Type) (DolanType ground) a)
    => a
    -> a
eliminateUnusedRecursion =
    mapPShimWits
        @_
        @(DolanType ground)
        (reshimWit polySemiIsoForwards . elimInType)
        (reshimWit polySemiIsoForwards . elimInType)
