module Language.Expression.Common.Pattern where

import Shapes

data Pattern w q a
    = ClosedPattern (q -> Maybe a)
    | forall t. OpenPattern (w t)
                            (Pattern w q (t, a))

instance Functor (Pattern w q) where
    fmap ab (ClosedPattern a) = ClosedPattern $ fmap (fmap ab) a
    fmap ab (OpenPattern wt pat) = OpenPattern wt $ fmap (\(t, a) -> (t, ab a)) pat

mapPattern :: (q -> a -> Maybe b) -> Pattern w q a -> Pattern w q b
mapPattern qamb (ClosedPattern qma) = ClosedPattern $ \q -> qma q >>= qamb q
mapPattern qamb (OpenPattern wt pat) = OpenPattern wt $ mapPattern (\q (t, a) -> fmap (\b -> (t, b)) (qamb q a)) pat

contramap1Pattern :: (q -> Maybe p) -> Pattern w p a -> Pattern w q a
contramap1Pattern qp (ClosedPattern pma) = ClosedPattern $ \q -> qp q >>= pma
contramap1Pattern qp (OpenPattern wt pat) = OpenPattern wt $ contramap1Pattern qp pat

instance Applicative (Pattern w q) where
    pure a = ClosedPattern $ \_ -> Just a
    ClosedPattern qmab <*> pat = mapPattern (\q a -> fmap (\ab -> ab a) $ qmab q) pat
    OpenPattern wt patab <*> pata = OpenPattern wt $ (\(t, ab) a -> (t, ab a)) <$> patab <*> pata

instance Category (Pattern w) where
    id = ClosedPattern Just
    ClosedPattern bmc . pat = mapPattern (\_ -> bmc) pat
    OpenPattern wt patbc . patab = OpenPattern wt $ patbc . patab

instance Arrow (Pattern w) where
    arr ab = ClosedPattern $ \a -> Just $ ab a
    first (ClosedPattern amb) =
        ClosedPattern $ \(a, d) -> do
            b <- amb a
            return (b, d)
    first (OpenPattern wt pat) = OpenPattern wt $ fmap (\((t, c), d) -> (t, (c, d))) $ first pat

patternFreeWitnesses :: (forall t. w t -> r) -> Pattern w q a -> [r]
patternFreeWitnesses _wr (ClosedPattern _) = []
patternFreeWitnesses wr (OpenPattern wt pat) = (wr wt) : patternFreeWitnesses wr pat

varPattern :: w t -> Pattern w t ()
varPattern wt = OpenPattern wt $ ClosedPattern $ \t -> Just (t, ())
