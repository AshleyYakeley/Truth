module Data.Shim.PolyShim where

import Data.Shim.JoinMeet
import Data.Shim.Range
import Shapes

class LiftPolyCategory (shim :: forall kc. kc -> kc -> Type) where
    coLift ::
           forall kp kq (f :: kp -> kq) (g :: kp -> kq) (a :: kp). (CoercibleKind kp, InKind f, InKind g, InKind a)
        => Maybe (Dict (RepresentationalRole f))
        -> Maybe (Dict (RepresentationalRole g))
        -> shim f g
        -> shim (f a) (g a)

class ( forall k. CoercibleKind k => EnhancedFunction (shim :: k -> k -> Type)
      , forall k. CoercibleKind k => InCategory (shim :: k -> k -> Type)
      , Shim (shim :: Type -> Type -> Type)
      , LiftPolyCategory shim
      ) => PolyShim (shim :: forall k. k -> k -> Type) where
    coShimFunc ::
           forall kp kq (f :: kp -> kq) (g :: kp -> kq) (a :: kp) (b :: kp).
           (CoercibleKind kp, InKind f, InKind g, InKind a, InKind b, CatFunctor KindFunction KindFunction g)
        => Maybe (Dict (RepresentationalRole f))
        -> Maybe (Dict (RepresentationalRole g))
        -> shim f g
        -> shim a b
        -> shim (f a) (g b)
    contraShimFunc ::
           forall kp kq (f :: kp -> kq) (g :: kp -> kq) (a :: kp) (b :: kp).
           (CoercibleKind kp, InKind f, InKind g, InKind a, InKind b, CatFunctor (CatDual KindFunction) KindFunction g)
        => Maybe (Dict (RepresentationalRole f))
        -> Maybe (Dict (RepresentationalRole g))
        -> shim f g
        -> shim b a
        -> shim (f a) (g b)
    rangeShimFunc ::
           forall kp kq (f :: (kp, kp) -> kq) (g :: (kp, kp) -> kq) (pa :: kp) (pb :: kp) (qa :: kp) (qb :: kp).
           ( kp ~ Type
           , CoercibleKind kp
           , InKind f
           , InKind g
           , InKind pa
           , InKind pb
           , InKind qa
           , InKind qb
           , CatFunctor (CatRange KindFunction) KindFunction g
           )
        => Maybe (Dict (RepresentationalRole f))
        -> Maybe (Dict (RepresentationalRole g))
        -> shim f g
        -> CatRange shim '( pa, qa) '( pb, qb)
        -> shim (f '( pa, qa)) (g '( pb, qb))

coShimFuncR ::
       forall (shim :: forall k. k -> k -> Type) kp kq (f :: kp -> kq) (g :: kp -> kq) (a :: kp) (b :: kp).
       ( PolyShim shim
       , CoercibleKind kp
       , InKind f
       , InKind g
       , InKind a
       , InKind b
       , CatFunctor KindFunction KindFunction g
       , RepresentationalRole f
       , RepresentationalRole g
       )
    => shim f g
    -> shim a b
    -> shim (f a) (g b)
coShimFuncR = coShimFunc (Just Dict) (Just Dict)

contraShimFuncR ::
       forall (shim :: forall k. k -> k -> Type) kp kq (f :: kp -> kq) (g :: kp -> kq) (a :: kp) (b :: kp).
       ( PolyShim shim
       , CoercibleKind kp
       , InKind f
       , InKind g
       , InKind a
       , InKind b
       , CatFunctor (CatDual KindFunction) KindFunction g
       , RepresentationalRole f
       , RepresentationalRole g
       )
    => shim f g
    -> shim b a
    -> shim (f a) (g b)
contraShimFuncR = contraShimFunc (Just Dict) (Just Dict)

rangeShimFuncR ::
       forall (shim :: forall k. k -> k -> Type) kp kq (f :: (kp, kp) -> kq) (g :: (kp, kp) -> kq) (pa :: kp) (pb :: kp) (qa :: kp) (qb :: kp).
       ( PolyShim shim
       , kp ~ Type
       , CoercibleKind kp
       , InKind f
       , RepresentationalRole f
       , InKind g
       , RepresentationalRole g
       , InKind pa
       , InKind pb
       , InKind qa
       , InKind qb
       , CatFunctor (CatRange KindFunction) KindFunction g
       )
    => shim f g
    -> CatRange shim '( pa, qa) '( pb, qb)
    -> shim (f '( pa, qa)) (g '( pb, qb))
rangeShimFuncR = rangeShimFunc (Just Dict) (Just Dict)
