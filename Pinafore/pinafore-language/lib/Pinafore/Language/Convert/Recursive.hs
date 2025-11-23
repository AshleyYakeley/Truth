module Pinafore.Language.Convert.Recursive (Rec0 (..), mapRec0, rec0Representational) where

import Language.Expression.Dolan
import Unsafe.Type (semisafeCoercion)

import Import
import Pinafore.Language.Convert.FromQIsoShim
import Pinafore.Language.Convert.HasType
import Pinafore.Language.Type

newtype Rec0 (f :: Type -> Type) = MkRec0 {unRec0 :: f (Rec0 f)}

mapRec0 :: Functor f => (f --> g) -> Rec0 f -> Rec0 g
mapRec0 fg (MkRec0 frf) = MkRec0 $ fg $ fmap (mapRec0 fg) frf

rec0Representational :: forall (f :: Type -> Type) (g :: Type -> Type). RepresentationalRole f => Coercion f g -> Coercion (Rec0 f) (Rec0 g)
rec0Representational cfg = semisafeCoercion $ let
    cf :: Coercion (Rec0 f) (f (Rec0 f))
    cf = MkCoercion
    cg :: Coercion (g (Rec0 g)) (Rec0 g)
    cg = MkCoercion

    cr :: Coercion (Rec0 f) (Rec0 g)
    cr = cg . cur . cf
    cur :: Coercion (f (Rec0 f)) (g (Rec0 g))
    cur = applyCoercion1 cfg cr
    in cr

newUnusedTypeVar :: forall a r. VarRenameable a => a -> (forall tv. TypeVarT tv -> r) -> r
newUnusedTypeVar a = newTypeVar $ renameUnusedName @QTypeSystem a

instance
    forall (pshim :: PolyShimKind) polarity (f :: Type -> Type).
    ( FromQIsoShim pshim
    , SubstitutablePolyShim pshim
    , Is PolarityType polarity
    , VarianceOf f ~ 'Covariance
    , HasVariance f
    , HasQPartialGroundedType pshim polarity f
    ) =>
    HasQType pshim polarity (Rec0 f)
    where
    qType = let
        rollConv :: PolarShim (pshim Type) polarity (Rec0 f) (f (Rec0 f))
        rollConv =
            fromQShimsPolar
                (coerceShim "unRec0")
                (coerceShim "MkRec0")

        fPGT :: PShimWit (pshim (Type -> Type)) (QPartialGroundedType '[CoCCRVariance]) polarity f
        fPGT = qPartialGroundedType @pshim @polarity @'[CoCCRVariance] @f
        in newUnusedTypeVar fPGT $ \tv ->
            assignTypeVarT @(Rec0 f) tv
                $ shimWitToDolan
                $ recursiveDolanShimWit tv
                $ mapShimWit rollConv
                $ chainShimWit typeToDolan
                $ applyDolanPartialGroundedShimWit
                    fPGT
                    (coCCRArgument $ varDolanShimWit tv)

instance
    forall (f :: Type -> Type).
    ( VarianceOf f ~ 'Covariance
    , RepresentationalRole f
    , HasQNonpolarPartialGroundedType f
    ) =>
    HasQNonpolarType (Rec0 f)
    where
    qNonpolarType = let
        fPGT :: ShimWit Coercion (QNonpolarPartialGroundedType '[CoCCRVariance]) f
        fPGT = qNonpolarPartialGroundedType @'[CoCCRVariance] @f
        in case fPGT of
            MkShimWit (gt :: _ f') MkCoercion ->
                let
                    rollConv :: Coercion (Rec0 f') (f' (Rec0 f'))
                    rollConv = MkCoercion
                    in newUnusedTypeVar fPGT $ \tv ->
                        assignTypeVarT @(Rec0 f') tv
                            $ mapShimWit (rollConv . rec0Representational MkCoercion)
                            $ mkShimWit
                            $ RecursiveNonpolarType (coerceTypeVarT tv)
                            $ GroundedNonpolarType
                            $ nonpolarPartialToGroundedType
                            $ ApplyNonpolarPartialGroundedType gt
                            $ CoNonpolarArgument
                            $ VarNonpolarType tv
