module Language.Expression.Common.Pattern.Pattern
    ( Pattern(..)
    , purityFunctionPattern
    , contramap1Pattern
    , patternFreeWitnesses
    , anyPattern
    , varPattern
    ) where

import Shapes

type Pattern :: (Type -> Type) -> Type -> Type -> Type
data Pattern w q a =
    forall t. MkPattern [SomeFor ((->) t) w]
                        (PurityFunction Maybe q (t, a))

instance Functor (Pattern w q) where
    fmap ab (MkPattern ww pf) = MkPattern ww $ fmap (fmap ab) pf

purityFunctionPattern :: PurityFunction Maybe a b -> Pattern w a b
purityFunctionPattern pf = MkPattern [] $ fmap (\b -> ((), b)) pf

contramap1Pattern :: PurityFunction Maybe q p -> Pattern w p a -> Pattern w q a
contramap1Pattern qp (MkPattern ww pf) = MkPattern ww $ pf . qp

contramapWits :: (p -> q) -> [SomeFor ((->) q) w] -> [SomeFor ((->) p) w]
contramapWits pq = fmap $ \(MkSomeFor w conv) -> MkSomeFor w $ conv . pq

combineWits :: [SomeFor ((->) p) w] -> [SomeFor ((->) q) w] -> [SomeFor ((->) (p, q)) w]
combineWits ww1 ww2 = contramapWits fst ww1 <> contramapWits snd ww2

instance Applicative (Pattern w q) where
    pure a = MkPattern [] $ pure ((), a)
    MkPattern ww1 pf <*> MkPattern ww2 qf =
        MkPattern (combineWits ww1 ww2) $ liftA2 (\(t1, ab) (t2, a) -> ((t1, t2), ab a)) pf qf

instance Category (Pattern w) where
    id = MkPattern [] $ arr $ \a -> ((), a)
    MkPattern ww1 bc . MkPattern ww2 ab =
        MkPattern (combineWits ww1 ww2) $ fmap (\(t2, (t1, c)) -> ((t1, t2), c)) (second bc) . ab

instance Arrow (Pattern w) where
    arr ab = MkPattern [] $ arr $ \a -> ((), ab a)
    first (MkPattern ww pf) = MkPattern ww $ fmap (\((t, c), d) -> (t, (c, d))) $ first pf

patternFreeWitnesses :: (forall t. w t -> r) -> Pattern w q a -> [r]
patternFreeWitnesses wr (MkPattern ww _) = fmap (\(MkSomeFor wt _) -> wr wt) ww

anyPattern :: Pattern w q ()
anyPattern = pure ()

varPattern :: w t -> Pattern w t ()
varPattern wt = MkPattern [MkSomeFor wt id] $ arr $ \t -> (t, ())
