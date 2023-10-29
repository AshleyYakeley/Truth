module Language.Expression.Dolan.Type.MixedType where

import Data.Shim
import Language.Expression.Dolan.Type.Combine
import Language.Expression.Dolan.Type.DolanType
import Language.Expression.Dolan.Type.FlipType
import Language.Expression.Dolan.Type.InvertedType
import Language.Expression.Dolan.TypeSystem
import Shapes

data MixedType (ground :: GroundTypeKind) polarity t where
    MkMixedType
        :: forall (ground :: GroundTypeKind) polarity nt invt.
           DolanType ground polarity nt
        -> InvertedType ground polarity invt
        -> MixedType ground polarity (JoinMeetType polarity nt invt)

instance forall (ground :: GroundTypeKind) polarity t. (ShowGroundType ground, Is PolarityType polarity) =>
             Show (MixedType ground polarity t) where
    show (MkMixedType nt it) = allShow nt <> ", " <> show it

joinMeetMixedType ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity (a :: Type) (b :: Type).
       (JoinMeetIsoShim (pshim Type), Is PolarityType polarity)
    => MixedType ground polarity a
    -> MixedType ground polarity b
    -> PShimWit (pshim Type) (MixedType ground) polarity (JoinMeetType polarity a b)
joinMeetMixedType (MkMixedType nta ita) (MkMixedType ntb itb) =
    case joinMeetType nta ntb of
        MkShimWit ntab convn ->
            case joinMeetInvertedType ita itb of
                MkShimWit itab convi -> MkShimWit (MkMixedType ntab itab) $ iPolarPair convn convi . iPolarSwap4

type MixedShimWit (ground :: GroundTypeKind) polarity = PShimWit (DolanShim ground) (MixedType ground) polarity

nilMixedType ::
       forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity)
    => MixedShimWit ground polarity (LimitType polarity)
nilMixedType = MkShimWit (MkMixedType NilDolanType NilInvertedType) iPolarR1

plainMixedType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> MixedShimWit ground polarity t
plainMixedType t = MkShimWit (MkMixedType t NilInvertedType) iPolarR1

invertedMixedType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => InvertedType ground polarity t
    -> MixedShimWit ground polarity t
invertedMixedType t = MkShimWit (MkMixedType NilDolanType t) iPolarR2

invertMixedType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground (InvertPolarity polarity) t
    -> MixedShimWit ground polarity t
invertMixedType t = mapShimWit iPolarR1 (invertedMixedType $ singleInvertedType t)

flipMixedType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => FlipType ground polarity t
    -> MixedShimWit ground polarity t
flipMixedType (NormalFlipType t) = plainMixedType t
flipMixedType (InvertFlipType t) = invertMixedType t
