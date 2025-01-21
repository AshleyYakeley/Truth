module Data.Shim.Mono.PShimWit where

import Shapes

import Data.Shim.Mono.General
import Data.Shim.Mono.PolarJoinMeet
import Data.Shim.Mono.PolarShimWit
import Data.Shim.Mono.ShimWit
import Data.Shim.Polar

type PShimWit :: forall k. ShimKind k -> (Polarity -> k -> Type) -> Polarity -> k -> Type
type PShimWit shim wit polarity = PolarShimWit shim (wit polarity) polarity

chainPShimWit2 ::
    forall (shim :: ShimKind Type) (w :: Polarity -> Type -> Type) (polarity :: Polarity) (a :: Type) (b :: Type).
    (JoinMeetIsoShim shim, Is PolarityType polarity) =>
    (forall a' b'. w polarity a' -> w polarity b' -> PShimWit shim w polarity (JoinMeetType polarity a' b')) ->
    PShimWit shim w polarity a ->
    PShimWit shim w polarity b ->
    PShimWit shim w polarity (JoinMeetType polarity a b)
chainPShimWit2 f (MkShimWit ta conva) (MkShimWit tb convb) = ccontramap (iPolarPair conva convb) $ f ta tb

type FuncShimWit :: (Type -> Type) -> ShimKind Type -> ShimKind Type
data FuncShimWit w shim a b
    = forall c. MkFuncShimWit
        (w c)
        (shim a b -> shim a c)

mapFuncShimWit :: FuncShimWit w shim a b -> shim a b -> ShimWit shim w a
mapFuncShimWit (MkFuncShimWit w f) conv = MkShimWit w $ f conv
