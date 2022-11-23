module Data.PurityFunction where

import Shapes.Import

data PurityType (m :: Type -> Type) (f :: Type -> Type) where
    PureType :: PurityType m Identity
    ImpureType :: PurityType m m

purityIs ::
       forall (c :: (Type -> Type) -> Constraint) (m :: Type -> Type) (f :: Type -> Type) (r :: Type). (c m, c Identity)
    => PurityType m f
    -> (c f => r)
    -> r
purityIs PureType r = r
purityIs ImpureType r = r

matchPurityType ::
       Applicative m
    => PurityType m fa
    -> PurityType m fb
    -> (forall fab. Applicative fab => PurityType m fab -> (fa --> fab) -> (fb --> fab) -> r)
    -> r
matchPurityType PureType PureType call = call PureType id id
matchPurityType PureType ImpureType call = call ImpureType (pure . runIdentity) id
matchPurityType ImpureType PureType call = call ImpureType id (pure . runIdentity)
matchPurityType ImpureType ImpureType call = call ImpureType id id

runPurity :: Applicative m => PurityType m f -> f a -> m a
runPurity PureType (Identity a) = pure a
runPurity ImpureType ma = ma

data PurityFunction m a b =
    forall f. MkPurityFunction (PurityType m f)
                               (Kleisli f a b)

pattern PureFunction :: (a -> b) -> PurityFunction m a b

pattern PureFunction f <-
        MkPurityFunction PureType
          (Kleisli ((\ f a -> runIdentity $ f a) -> f))
  where PureFunction f
          = MkPurityFunction PureType (Kleisli $ \ a -> Identity $ f a)

pattern ImpureFunction :: (a -> m b) -> PurityFunction m a b

pattern ImpureFunction f <- MkPurityFunction ImpureType (Kleisli f)
  where ImpureFunction f = MkPurityFunction ImpureType (Kleisli f)

{-# COMPLETE PureFunction, ImpureFunction #-}

mapKleisli :: (f1 --> f2) -> Kleisli f1 a b -> Kleisli f2 a b
mapKleisli f (Kleisli afb) = Kleisli $ \a -> f $ afb a

matchPurityFunction ::
       Applicative m
    => (forall f. PurityType m f -> Kleisli f a1 b1 -> Kleisli f a2 b2 -> Kleisli f a12 b12)
    -> PurityFunction m a1 b1
    -> PurityFunction m a2 b2
    -> PurityFunction m a12 b12
matchPurityFunction f (MkPurityFunction purity1 k1) (MkPurityFunction purity2 k2) =
    matchPurityType purity1 purity2 $ \purity12 c1 c2 ->
        MkPurityFunction purity12 $ f purity12 (mapKleisli c1 k1) (mapKleisli c2 k2)

applyPurityFunction :: Applicative m => PurityFunction m a b -> a -> m b
applyPurityFunction (MkPurityFunction ImpureType (Kleisli amb)) a = amb a
applyPurityFunction (MkPurityFunction PureType (Kleisli amb)) a = pure $ runIdentity $ amb a

instance Functor m => Functor (PurityFunction m t) where
    fmap ab (MkPurityFunction purity mf) = purityIs @Functor purity $ MkPurityFunction purity $ fmap ab mf

instance Applicative m => Applicative (PurityFunction m t) where
    pure a = MkPurityFunction PureType $ pure a
    liftA2 f = matchPurityFunction $ \purity ka kb -> purityIs @Applicative purity $ liftA2 f ka kb

instance Alternative (PurityFunction Maybe t) where
    empty = MkPurityFunction ImpureType $ empty
    MkPurityFunction PureType k1 <|> _ = MkPurityFunction PureType k1
    MkPurityFunction ImpureType (Kleisli amb1) <|> MkPurityFunction PureType (Kleisli aib2) = let
        mii :: Maybe a -> Identity a -> Identity a
        mii (Just a) _ = Identity a
        mii Nothing ia = ia
        in MkPurityFunction PureType $ Kleisli $ liftA2 mii amb1 aib2
    MkPurityFunction ImpureType k1 <|> MkPurityFunction ImpureType k2 = MkPurityFunction ImpureType $ k1 <|> k2

instance Monad m => Category (PurityFunction m) where
    id = arr id
    (.) = matchPurityFunction $ \purity ka kb -> purityIs @Monad purity $ (.) ka kb

instance Monad m => Arrow (PurityFunction m) where
    arr f = MkPurityFunction PureType $ arr f
    first (MkPurityFunction purity ab) = purityIs @Monad purity $ MkPurityFunction purity $ first ab
    second (MkPurityFunction purity ab) = purityIs @Monad purity $ MkPurityFunction purity $ second ab
