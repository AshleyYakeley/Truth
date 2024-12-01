module Language.Expression.Dolan.Type.InvertedType where

import Data.Shim
import Language.Expression.Dolan.Type.DolanType
import Language.Expression.Dolan.TypeSystem
import Shapes

type InvertedType :: GroundTypeKind -> Polarity -> Type -> Type
data InvertedType ground polarity t where
    NilInvertedType :: forall (ground :: GroundTypeKind) polarity. InvertedType ground polarity (LimitType polarity)
    ConsInvertedType
        :: forall (ground :: GroundTypeKind) polarity t1 tr.
           DolanType ground (InvertPolarity polarity) t1
        -> InvertedType ground polarity tr
        -> InvertedType ground polarity (JoinMeetType polarity t1 tr)

invertedSomeTypes ::
       forall (ground :: GroundTypeKind) polarity t. (Is PolarityType polarity)
    => InvertedType ground polarity t
    -> [Some (DolanType ground (InvertPolarity polarity))]
invertedSomeTypes NilInvertedType = []
invertedSomeTypes (ConsInvertedType t1 tr) = MkSome t1 : invertedSomeTypes tr

instance forall (ground :: GroundTypeKind) polarity t. (ShowGroundType ground, Is PolarityType polarity) =>
             Show (InvertedType ground polarity t) where
    show t = withInvertPolarity @polarity $ show $ invertedSomeTypes t

singleInvertedType ::
       forall (ground :: GroundTypeKind) polarity t.
       DolanType ground (InvertPolarity polarity) t
    -> InvertedType ground polarity (JoinMeetType polarity t (LimitType polarity))
singleInvertedType t = ConsInvertedType t NilInvertedType

joinMeetInvertedType ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity (a :: Type) (b :: Type).
       (JoinMeetIsoShim (pshim Type), Is PolarityType polarity)
    => InvertedType ground polarity a
    -> InvertedType ground polarity b
    -> PShimWit (pshim Type) (InvertedType ground) polarity (JoinMeetType polarity a b)
joinMeetInvertedType NilInvertedType tb = withInvertPolarity @polarity $ MkShimWit tb iPolarL2
joinMeetInvertedType (ConsInvertedType ta tr) tb =
    withInvertPolarity @polarity $
    case joinMeetInvertedType tr tb of
        MkShimWit trb convrb -> MkShimWit (ConsInvertedType ta trb) $ iPolarPair id convrb . iPolarSwapL

joinMeetInvertedShimWit ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) (a :: Type) (b :: Type).
       (JoinMeetIsoShim (pshim Type), Is PolarityType polarity)
    => PolarShimWit (pshim Type) (InvertedType ground polarity) polarity a
    -> PolarShimWit (pshim Type) (InvertedType ground polarity) polarity b
    -> PolarShimWit (pshim Type) (InvertedType ground polarity) polarity (JoinMeetType polarity a b)
joinMeetInvertedShimWit (MkShimWit ta conva) (MkShimWit tb convb) =
    withInvertPolarity @polarity $ ccontramap (iPolarPair conva convb) $ joinMeetInvertedType ta tb
