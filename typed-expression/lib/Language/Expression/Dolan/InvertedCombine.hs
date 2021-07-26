module Language.Expression.Dolan.InvertedCombine where

import Data.Shim
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

type InvertedCombinedDolanType :: GroundTypeKind -> Polarity -> Type -> Type
data InvertedCombinedDolanType ground polarity t where
    NilInvertedCombinedDolanType
        :: forall (ground :: GroundTypeKind) polarity.
           InvertedCombinedDolanType ground polarity (LimitType (InvertPolarity polarity))
    ConsInvertedCombinedDolanType
        :: forall (ground :: GroundTypeKind) polarity t1 tr.
           DolanType ground polarity t1
        -> InvertedCombinedDolanType ground polarity tr
        -> InvertedCombinedDolanType ground polarity (JoinMeetType (InvertPolarity polarity) t1 tr)

joinMeetInvertedCombinedType ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity (a :: Type) (b :: Type).
       (JoinMeetIsoCategory (pshim Type), Is PolarityType polarity)
    => InvertedCombinedDolanType ground polarity a
    -> InvertedCombinedDolanType ground polarity b
    -> PolarShimWit (pshim Type) (InvertedCombinedDolanType ground polarity) (InvertPolarity polarity) (JoinMeetType (InvertPolarity polarity) a b)
joinMeetInvertedCombinedType NilInvertedCombinedDolanType tb = invertPolarity @polarity $ MkShimWit tb iPolarL2
joinMeetInvertedCombinedType (ConsInvertedCombinedDolanType ta tr) tb =
    invertPolarity @polarity $
    case joinMeetInvertedCombinedType tr tb of
        MkShimWit trb convrb -> MkShimWit (ConsInvertedCombinedDolanType ta trb) $ iPolarPair cid convrb <.> iPolarSwapL

joinMeetInvertedCombinedShimWit ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) (a :: Type) (b :: Type).
       (JoinMeetIsoCategory (pshim Type), Is PolarityType polarity)
    => PolarShimWit (pshim Type) (InvertedCombinedDolanType ground polarity) (InvertPolarity polarity) a
    -> PolarShimWit (pshim Type) (InvertedCombinedDolanType ground polarity) (InvertPolarity polarity) b
    -> PolarShimWit (pshim Type) (InvertedCombinedDolanType ground polarity) (InvertPolarity polarity) (JoinMeetType (InvertPolarity polarity) a b)
joinMeetInvertedCombinedShimWit (MkShimWit ta conva) (MkShimWit tb convb) =
    invertPolarity @polarity $ ccontramap (iPolarPair conva convb) $ joinMeetInvertedCombinedType ta tb
