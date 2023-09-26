module Language.Expression.Dolan.InvertedCombine where

import Data.Shim
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

type InvertedCombinedDolanType :: GroundTypeKind -> Polarity -> Type -> Type
data InvertedCombinedDolanType ground polarity t where
    NilInvertedCombinedDolanType
        :: forall (ground :: GroundTypeKind) polarity. InvertedCombinedDolanType ground polarity (LimitType polarity)
    ConsInvertedCombinedDolanType
        :: forall (ground :: GroundTypeKind) polarity t1 tr.
           DolanType ground (InvertPolarity polarity) t1
        -> InvertedCombinedDolanType ground polarity tr
        -> InvertedCombinedDolanType ground polarity (JoinMeetType polarity t1 tr)

joinMeetInvertedCombinedType ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity (a :: Type) (b :: Type).
       (JoinMeetIsoShim (pshim Type), Is PolarityType polarity)
    => InvertedCombinedDolanType ground polarity a
    -> InvertedCombinedDolanType ground polarity b
    -> PolarShimWit (pshim Type) (InvertedCombinedDolanType ground polarity) polarity (JoinMeetType polarity a b)
joinMeetInvertedCombinedType NilInvertedCombinedDolanType tb = withInvertPolarity @polarity $ MkShimWit tb iPolarL2
joinMeetInvertedCombinedType (ConsInvertedCombinedDolanType ta tr) tb =
    withInvertPolarity @polarity $
    case joinMeetInvertedCombinedType tr tb of
        MkShimWit trb convrb -> MkShimWit (ConsInvertedCombinedDolanType ta trb) $ iPolarPair id convrb . iPolarSwapL

joinMeetInvertedCombinedShimWit ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) (a :: Type) (b :: Type).
       (JoinMeetIsoShim (pshim Type), Is PolarityType polarity)
    => PolarShimWit (pshim Type) (InvertedCombinedDolanType ground polarity) polarity a
    -> PolarShimWit (pshim Type) (InvertedCombinedDolanType ground polarity) polarity b
    -> PolarShimWit (pshim Type) (InvertedCombinedDolanType ground polarity) polarity (JoinMeetType polarity a b)
joinMeetInvertedCombinedShimWit (MkShimWit ta conva) (MkShimWit tb convb) =
    withInvertPolarity @polarity $ ccontramap (iPolarPair conva convb) $ joinMeetInvertedCombinedType ta tb
