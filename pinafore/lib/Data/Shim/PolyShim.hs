module Data.Shim.PolyShim where

import Data.Shim.JoinMeet
import Data.Shim.Range
import Data.Shim.Variance
import Shapes

class LiftPolyCategory (shim :: forall kc. kc -> kc -> Type) where
    coLift ::
           forall kp kq (f :: kp -> kq) (g :: kp -> kq) (a :: kp). (CoercibleKind kp, InKind f, InKind g, InKind a)
        => Maybe (Dict (RepresentationalRole f))
        -> Maybe (Dict (RepresentationalRole g))
        -> shim f g
        -> shim (f a) (g a)
    apShimFunc ::
           forall (var :: Variance) kq (f :: VarianceKind var -> kq) (g :: VarianceKind var -> kq) (a :: VarianceKind var) (b :: VarianceKind var).
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

class ( forall k. CoercibleKind k => EnhancedFunction (shim :: k -> k -> Type)
      , forall k. CoercibleKind k => InCategory (shim :: k -> k -> Type)
      , Shim (shim :: Type -> Type -> Type)
      , LiftPolyCategory shim
      ) => PolyShim (shim :: forall k. k -> k -> Type)

coShimFuncR ::
       forall (shim :: forall k. k -> k -> Type) kq (f :: Type -> kq) (g :: Type -> kq) (a :: Type) (b :: Type).
       ( PolyShim shim
       , InKind f
       , InKind g
       , InKind a
       , InKind b
       , CatFunctor KindFunction KindFunction f
       , CatFunctor KindFunction KindFunction g
       , RepresentationalRole f
       , RepresentationalRole g
       )
    => shim f g
    -> shim a b
    -> shim (f a) (g b)
coShimFuncR = apShimFunc CovarianceType (Just Dict) (Just Dict)

contraShimFuncR ::
       forall (shim :: forall k. k -> k -> Type) kq (f :: Type -> kq) (g :: Type -> kq) (a :: Type) (b :: Type).
       ( PolyShim shim
       , InKind f
       , InKind g
       , InKind a
       , InKind b
       , CatFunctor (CatDual KindFunction) KindFunction f
       , CatFunctor (CatDual KindFunction) KindFunction g
       , RepresentationalRole f
       , RepresentationalRole g
       )
    => shim f g
    -> CatDual shim a b
    -> shim (f a) (g b)
contraShimFuncR = apShimFunc ContravarianceType (Just Dict) (Just Dict)

rangeShimFuncR ::
       forall (shim :: forall k. k -> k -> Type) kq (f :: (Type, Type) -> kq) (g :: (Type, Type) -> kq) (a :: ( Type
                                                                                                              , Type)) (b :: ( Type
                                                                                                                             , Type)).
       ( PolyShim shim
       , InKind f
       , RepresentationalRole f
       , InKind g
       , RepresentationalRole g
       , InKind a
       , InKind b
       , CatFunctor (CatRange KindFunction) KindFunction f
       , CatFunctor (CatRange KindFunction) KindFunction g
       )
    => shim f g
    -> CatRange shim a b
    -> shim (f a) (g b)
rangeShimFuncR = apShimFunc RangevarianceType (Just Dict) (Just Dict)
