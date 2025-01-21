module Data.EndoM where

import Shapes.Import

newtype EndoM m a = MkEndoM
    { unEndoM :: a -> m a
    }

-- | Opposite of 'Endo', first argument of `(<>)` is run first.
instance Monad m => Semigroup (EndoM m a) where
    MkEndoM ama1 <> MkEndoM ama2 = MkEndoM $ \a -> ama1 a >>= ama2

instance Monad m => Monoid (EndoM m a) where
    mempty = MkEndoM return

instance Functor m => Invariant (EndoM m) where
    invmap ab ba (MkEndoM ama) = MkEndoM $ \b -> fmap ab $ ama $ ba b

instance Functor m => Summable (EndoM m) where
    rVoid = MkEndoM absurd
    MkEndoM ama <+++> MkEndoM bmb =
        MkEndoM $ \case
            Left a -> fmap Left $ ama a
            Right b -> fmap Right $ bmb b

instance Applicative m => Productable (EndoM m) where
    rUnit = MkEndoM $ \_ -> pure ()
    MkEndoM ama <***> MkEndoM bmb = MkEndoM $ \(a, b) -> liftA2 (,) (ama a) (bmb b)

endoFor :: (Applicative m, Traversable t) => EndoM m a -> EndoM m (t a)
endoFor (MkEndoM ama) = MkEndoM $ \ta -> for ta ama

endoMToEndo :: EndoM Identity a -> Endo a
endoMToEndo (MkEndoM aia) = Endo $ \a -> runIdentity $ aia a

endoToEndoM :: Applicative m => Endo a -> EndoM m a
endoToEndoM (Endo aa) = MkEndoM $ \a -> pure $ aa a

type Endo' (w :: k -> Type) = forall t. Endo (w t)

type EndoM' (m :: Type -> Type) (w :: k -> Type) = forall t. EndoM m (w t)

endoSomeFor ::
    forall m k (w :: k -> Type) (f :: k -> Type).
    Functor m =>
    EndoM' m w ->
    EndoM m (SomeFor f w)
endoSomeFor wtmwt = MkEndoM $ \(MkSomeFor wa fa) -> fmap (\wa' -> MkSomeFor wa' fa) $ unEndoM wtmwt wa

tellEndoM :: Monoid w => (a -> w) -> EndoM (Writer w) a
tellEndoM f = MkEndoM $ \a -> tell (f a) >> return a

execEndoMWriter :: EndoM (Writer w) a -> a -> w
execEndoMWriter (MkEndoM awa) a = execWriter $ awa a

hoistEndoM :: (m1 --> m2) -> EndoM m1 --> EndoM m2
hoistEndoM mm (MkEndoM ama) = MkEndoM $ \a -> mm $ ama a

liftEndoM :: (MonadTrans t, Monad m) => EndoM m --> EndoM (t m)
liftEndoM = hoistEndoM lift
