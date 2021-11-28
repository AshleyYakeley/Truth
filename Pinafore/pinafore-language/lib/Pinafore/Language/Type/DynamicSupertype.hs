module Pinafore.Language.Type.DynamicSupertype where

import Data.Shim
import Language.Expression.Dolan
import Shapes

type GreatestDynamicSupertype :: GroundTypeKind -> PolyShimKind -> Type -> Type
data GreatestDynamicSupertype ground pshim t =
    forall dt. MkGreatestDynamicSupertype (PShimWit (pshim Type) (DolanType ground) 'Negative dt)
                                          (pshim Type t dt)
                                          (pshim Type dt (Maybe t))

isomapGDS ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) (a :: Type) (b :: Type). (ApplyPolyShim pshim)
    => Isomorphism (pshim Type) a b
    -> GreatestDynamicSupertype ground pshim a
    -> GreatestDynamicSupertype ground pshim b
isomapGDS (MkIsomorphism ab ba) (MkGreatestDynamicSupertype d ad dma) =
    MkGreatestDynamicSupertype d (ad <.> ba) (applyCoPolyShim cid ab <.> dma)

type MakeGreatestDynamicSupertype :: GroundTypeKind -> PolyShimKind -> (Type -> Type) -> Constraint
class MakeGreatestDynamicSupertype ground pshim w where
    toNegativeShimWit :: forall (dt :: Type). w dt -> PShimWit (pshim Type) (DolanType ground) 'Negative dt

makeGDS ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) (w :: Type -> Type) (t :: Type) (dt :: Type).
       MakeGreatestDynamicSupertype ground pshim w
    => w dt
    -> pshim Type t dt
    -> pshim Type dt (Maybe t)
    -> GreatestDynamicSupertype ground pshim t
makeGDS wt = MkGreatestDynamicSupertype $ toNegativeShimWit wt

codecGDS ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) (w :: Type -> Type) (t :: Type) (dt :: Type).
       FunctionShim (pshim Type)
    => MakeGreatestDynamicSupertype ground pshim w =>
               String -> w dt -> Codec dt t -> GreatestDynamicSupertype ground pshim t
codecGDS s wt codec =
    makeGDS wt (functionToShim (s <> ":encode") $ encode codec) (functionToShim (s <> ":decode") $ decode codec)

type PolyGreatestDynamicSupertype :: GroundTypeKind -> PolyShimKind -> forall (dv :: DolanVariance) ->
                                                                               DolanVarianceKind dv -> Type
type PolyGreatestDynamicSupertype ground pshim dv gt
     = forall (t :: Type).
               DolanArguments dv (DolanType ground) gt 'Positive t -> Maybe (GreatestDynamicSupertype ground pshim t)
