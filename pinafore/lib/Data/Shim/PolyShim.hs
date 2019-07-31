module Data.Shim.PolyShim where

import Data.Shim.JoinMeet
import Data.Shim.Variance
import Shapes

-- type PolyShim (shim :: forall kc. kc -> kc -> Type) = forall k. CoercibleKind k => InCategory (shim :: k -> k -> Type)
class (forall k. CoercibleKind k => InCategory (shim :: k -> k -> Type)) =>
          ApPolyShim (shim :: forall kc. kc -> kc -> Type) where
    coLift ::
           forall kp kq (f :: kp -> kq) (g :: kp -> kq) (a :: kp). (CoercibleKind kp, InKind f, InKind g, InKind a)
        => Maybe (Dict (RepresentationalRole f))
        -> Maybe (Dict (RepresentationalRole g))
        -> shim f g
        -> shim (f a) (g a)
    apShimFunc ::
           forall (var :: Variance) k (f :: VarianceKind var -> k) (g :: VarianceKind var -> k) (a :: VarianceKind var) (b :: VarianceKind var).
           ( InKind f
           , InKind g
           , InKind a
           , InKind b
           , CatFunctor (VarianceCategory KindFunction var) KindFunction f
           , CatFunctor (VarianceCategory KindFunction var) KindFunction g
           )
        => VarianceType var
        -> Maybe (Dict (RepresentationalRole f))
        -> Maybe (Dict (RepresentationalRole g))
        -> shim f g
        -> VarianceCategory shim var a b
        -> shim (f a) (g b)

apShimFuncR ::
       forall (shim :: forall kc. kc -> kc -> Type) (var :: Variance) k (f :: VarianceKind var -> k) (g :: VarianceKind var -> k) (a :: VarianceKind var) (b :: VarianceKind var).
       ( ApPolyShim shim
       , InKind f
       , InKind g
       , InKind a
       , InKind b
       , CatFunctor (VarianceCategory KindFunction var) KindFunction f
       , CatFunctor (VarianceCategory KindFunction var) KindFunction g
       , RepresentationalRole f
       , RepresentationalRole g
       )
    => VarianceType var
    -> shim f g
    -> VarianceCategory shim var a b
    -> shim (f a) (g b)
apShimFuncR vt = apShimFunc vt (Just Dict) (Just Dict)

apShimFuncNR ::
       forall (shim :: forall kc. kc -> kc -> Type) (var :: Variance) k (f :: VarianceKind var -> k) (g :: VarianceKind var -> k) (a :: VarianceKind var) (b :: VarianceKind var).
       ( ApPolyShim shim
       , InKind f
       , InKind g
       , InKind a
       , InKind b
       , CatFunctor (VarianceCategory KindFunction var) KindFunction f
       , CatFunctor (VarianceCategory KindFunction var) KindFunction g
       )
    => VarianceType var
    -> shim f g
    -> VarianceCategory shim var a b
    -> shim (f a) (g b)
apShimFuncNR vt = apShimFunc vt Nothing Nothing

class ( forall k. CoercibleKind k => EnhancedFunction (shim :: k -> k -> Type)
      , Shim (shim :: Type -> Type -> Type)
      , ApPolyShim shim
      ) => EnhancedPolyShim (shim :: forall k. k -> k -> Type)
