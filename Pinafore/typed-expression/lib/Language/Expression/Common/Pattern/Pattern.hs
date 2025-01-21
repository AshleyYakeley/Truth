module Language.Expression.Common.Pattern.Pattern
    ( Pattern (..)
    , purePattern
    , patternFreeWitnesses
    , anyPattern
    , varPattern
    )
where

import Shapes

type Pattern :: (Type -> Type) -> (Type -> Type -> Type) -> Type -> Type -> Type
data Pattern w c a b
    = forall t. MkPattern
        [SomeFor ((->) t) w]
        (c a (t, b))

instance Functor (c a) => Functor (Pattern w c a) where
    fmap ab (MkPattern ww pf) = MkPattern ww $ fmap (fmap ab) pf

instance Arrow c => Category (Pattern w c) where
    id = MkPattern [] $ arr $ \a -> ((), a)
    MkPattern ww1 bc . MkPattern ww2 ab =
        MkPattern (concatPatternWits ww1 ww2) $ arr (\(t2, (t1, c)) -> ((t1, t2), c)) . second bc . ab

instance Arrow c => Arrow (Pattern w c) where
    arr ab = MkPattern [] $ arr $ \a -> ((), ab a)
    first (MkPattern ww pf) = MkPattern ww $ arr (\((t, c), d) -> (t, (c, d))) . first pf

purePattern :: Functor (c a) => c a b -> Pattern w c a b
purePattern pf = MkPattern [] $ fmap (\b -> ((), b)) pf

contramapWits :: (p -> q) -> [SomeFor ((->) q) w] -> [SomeFor ((->) p) w]
contramapWits pq = fmap $ \(MkSomeFor w conv) -> MkSomeFor w $ conv . pq

concatPatternWits :: [SomeFor ((->) p) w] -> [SomeFor ((->) q) w] -> [SomeFor ((->) (p, q)) w]
concatPatternWits ww1 ww2 = contramapWits fst ww1 <> contramapWits snd ww2

instance Applicative (c a) => Applicative (Pattern w c a) where
    pure a = MkPattern [] $ pure ((), a)
    MkPattern ww1 pf <*> MkPattern ww2 qf =
        MkPattern (concatPatternWits ww1 ww2) $ liftA2 (\(t1, ab) (t2, a) -> ((t1, t2), ab a)) pf qf

showPatWit :: AllConstraint Show w => SomeFor ((->) t) w -> String
showPatWit (MkSomeFor wa _) = allShow wa

instance (AllConstraint Show w, AllConstraint Show (c a)) => Show (Pattern w c a b) where
    show (MkPattern ww pf) = "{" <> intercalate "," (fmap showPatWit ww) <> "} " <> allShow pf

patternFreeWitnesses :: (forall t. w t -> r) -> Pattern w c a b -> [r]
patternFreeWitnesses wr (MkPattern ww _) = fmap (\(MkSomeFor wt _) -> wr wt) ww

anyPattern :: Applicative (c a) => Pattern w c a ()
anyPattern = pure ()

varPattern :: Arrow c => w t -> Pattern w c t ()
varPattern wt = MkPattern [MkSomeFor wt id] $ arr $ \t -> (t, ())
