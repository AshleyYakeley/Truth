module Language.Expression.Dolan.PShimWit where

import Data.Shim
import Language.Expression.Common
import Shapes

type PShimWit :: forall k. ShimKind k -> (Polarity -> k -> Type) -> Polarity -> k -> Type
type PShimWit shim wit polarity = PolarShimWit shim (wit polarity) polarity

type PShimWitMappable (shim :: ShimKind k) (wit :: Polarity -> k -> Type)
     = WitnessMappable (PShimWit shim wit 'Positive) (PShimWit shim wit 'Negative)

mapPShimWitsM ::
       forall m shim wit a. (Category shim, Applicative m, PShimWitMappable shim wit a)
    => (forall t. wit 'Positive t -> m (PShimWit shim wit 'Positive t))
    -> (forall t. wit 'Negative t -> m (PShimWit shim wit 'Negative t))
    -> a
    -> m a
mapPShimWitsM mapPos mapNeg = mapWitnessesM (chainPolarShimWitM mapPos) (chainPolarShimWitM mapNeg)

mapPShimWits ::
       forall shim wit a. (Category shim, PShimWitMappable shim wit a)
    => (forall t. wit 'Positive t -> PShimWit shim wit 'Positive t)
    -> (forall t. wit 'Negative t -> PShimWit shim wit 'Negative t)
    -> a
    -> a
mapPShimWits mapPos mapNeg = mapWitnesses (chainPolarShimWit mapPos) (chainPolarShimWit mapNeg)

chainPShimWit2 ::
       forall (shim :: ShimKind Type) (w :: Polarity -> Type -> Type) (polarity :: Polarity) (a :: Type) (b :: Type).
       (JoinMeetIsoCategory shim, Is PolarityType polarity)
    => (forall a' b'. w polarity a' -> w polarity b' -> PShimWit shim w polarity (JoinMeetType polarity a' b'))
    -> PShimWit shim w polarity a
    -> PShimWit shim w polarity b
    -> PShimWit shim w polarity (JoinMeetType polarity a b)
chainPShimWit2 f (MkShimWit ta conva) (MkShimWit tb convb) = ccontramap (iPolarPair conva convb) $ f ta tb

type FuncShimWit :: ShimKind Type -> (Polarity -> Type -> Type) -> Polarity -> Type -> Type -> Type
data FuncShimWit shim w polarity a b =
    forall c. MkFuncShimWit (w polarity c)
                            (PolarMap shim polarity a b -> PolarMap shim polarity a c)

mapFuncShimWit :: FuncShimWit shim w polarity a b -> PolarMap shim polarity a b -> PShimWit shim w polarity a
mapFuncShimWit (MkFuncShimWit w f) conv = MkShimWit w $ f conv
