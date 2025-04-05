module Data.PurityFunction where

import Shapes.Import

data PurityType (m :: Type -> Type) (f :: Type -> Type) where
    PureType :: PurityType m Identity
    ImpureType :: PurityType m m

instance Show (PurityType m f) where
    show PureType = "pure"
    show ImpureType = "impure"

purityIs ::
    forall (c :: (Type -> Type) -> Constraint) (m :: Type -> Type) (f :: Type -> Type) (r :: Type).
    (c m, c Identity) =>
    PurityType m f ->
    (c f => r) ->
    r
purityIs PureType r = r
purityIs ImpureType r = r

purityTypeProduct ::
    Applicative m =>
    PurityType m fa ->
    PurityType m fb ->
    (forall fab. Applicative fab => PurityType m fab -> (fa --> fab) -> (fb --> fab) -> r) ->
    r
purityTypeProduct PureType PureType call = call PureType id id
purityTypeProduct PureType ImpureType call = call ImpureType (pure . runIdentity) id
purityTypeProduct ImpureType PureType call = call ImpureType id (pure . runIdentity)
purityTypeProduct ImpureType ImpureType call = call ImpureType id id

purityTypeSum ::
    PurityType Maybe fa ->
    PurityType Maybe fb ->
    (forall fab. Applicative fab => PurityType Maybe fab -> (forall x. fa x -> fb x -> fab x) -> r) ->
    r
purityTypeSum PureType _ call = call PureType $ \fax _ -> fax
purityTypeSum ImpureType PureType call =
    call PureType $ \case
        Just x -> \_ -> Identity x
        Nothing -> id
purityTypeSum ImpureType ImpureType call =
    call ImpureType $ \case
        Just x -> \_ -> Just x
        Nothing -> id

runPurity :: Applicative m => PurityType m f -> f a -> m a
runPurity PureType (Identity a) = pure a
runPurity ImpureType ma = ma

runPurityCases :: String -> PurityType Maybe f -> f a -> a
runPurityCases err purity fa = fromMaybe (error err) $ runPurity purity fa

data PurityFunction m e a b
    = forall f. MkPurityFunction
        (PurityType m f)
        (e (a -> f b))

pattern PureFunction ::
    Functor e =>
    e (a -> b) -> PurityFunction m e a b
pattern PureFunction ef <-
    MkPurityFunction PureType (fmap $ (fmap runIdentity) -> ef)
    where
        PureFunction ef =
            MkPurityFunction PureType $ fmap (\f a -> Identity $ f a) ef

pattern ImpureFunction ::
    Functor e =>
    e (a -> m b) -> PurityFunction m e a b
pattern ImpureFunction ef <- MkPurityFunction ImpureType ef
    where
        ImpureFunction ef = MkPurityFunction ImpureType ef

{-# COMPLETE PureFunction, ImpureFunction #-}

matchPurityFunction ::
    (Applicative m, Applicative e) =>
    (forall f. PurityType m f -> (a1 -> f b1) -> (a2 -> f b2) -> (a12 -> f b12)) ->
    PurityFunction m e a1 b1 ->
    PurityFunction m e a2 b2 ->
    PurityFunction m e a12 b12
matchPurityFunction f (MkPurityFunction purity1 k1) (MkPurityFunction purity2 k2) =
    purityTypeProduct purity1 purity2 $ \purity12 c1 c2 ->
        MkPurityFunction purity12 $ liftA2 (f purity12) (fmap (fmap c1) k1) (fmap (fmap c2) k2)

runPurityFunction :: (Applicative m, Functor e) => PurityFunction m e a b -> e (a -> m b)
runPurityFunction (PureFunction eab) = fmap (\ab a -> pure $ ab a) eab
runPurityFunction (ImpureFunction eamb) = eamb

applyPurityFunction :: Applicative e => PurityFunction m e a b -> e a -> PurityFunction m e () b
applyPurityFunction (MkPurityFunction pt ekfab) ea = MkPurityFunction pt $ liftA2 (\afb a () -> afb a) ekfab ea

instance (Functor m, Functor e) => Functor (PurityFunction m e t) where
    fmap ab (MkPurityFunction purity mf) = purityIs @Functor purity $ MkPurityFunction purity $ fmap (fmap $ fmap ab) mf

instance (Applicative m, Applicative e) => Applicative (PurityFunction m e t) where
    pure a = MkPurityFunction PureType $ pure $ pure $ pure a
    liftA2 f = matchPurityFunction $ \purity ka kb -> purityIs @Applicative purity $ liftA2 (liftA2 f) ka kb

instance Applicative e => Alternative (PurityFunction Maybe e t) where
    empty = MkPurityFunction ImpureType $ pure $ pure empty
    MkPurityFunction p1 eamb1 <|> MkPurityFunction p2 eamb2 =
        purityTypeSum p1 p2 $ \p12 conv -> MkPurityFunction p12 $ liftA2 (liftA2 conv) eamb1 eamb2

-- Kleisli $ \a -> conv (amb1 a) (amb2 a)

instance (Monad m, Applicative e) => Category (PurityFunction m e) where
    id = arr id
    (.) = matchPurityFunction $ \purity kbc kab -> purityIs @Monad purity $ \a -> kab a >>= kbc

instance (Monad m, Applicative e) => Arrow (PurityFunction m e) where
    arr f = MkPurityFunction PureType $ pure $ pure . f
    first (MkPurityFunction purity ab) =
        purityIs @Monad purity $ MkPurityFunction purity $ fmap (\bfc (b, d) -> fmap (\c -> (c, d)) $ bfc b) ab
    second (MkPurityFunction purity ab) =
        purityIs @Monad purity $ MkPurityFunction purity $ fmap (\bfc (d, b) -> fmap (\c -> (d, c)) $ bfc b) ab

instance Show (PurityFunction m e a b) where
    show (MkPurityFunction purity _) = show purity

instance AllConstraint Show (PurityFunction m e a) where
    allConstraint = Dict
