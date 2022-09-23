module Language.Expression.Common.Pattern.Pattern
    ( Pattern(..)
    , contramap1Pattern
    , patternFreeWitnesses
    , varPattern
    ) where

import Shapes

data Pattern w q a
    = ClosedPattern (PurityFunction Maybe q a)
    | forall t. OpenPattern (w t)
                            (Pattern w q (t, a))

instance Functor (Pattern w q) where
    fmap ab (ClosedPattern a) = ClosedPattern $ fmap ab a
    fmap ab (OpenPattern wt pat) = OpenPattern wt $ fmap (\(t, a) -> (t, ab a)) pat

apPattern :: PurityFunction Maybe q (a -> b) -> Pattern w q a -> Pattern w q b
apPattern fmqab (ClosedPattern fmqa) = ClosedPattern $ fmqab <*> fmqa
apPattern fmqab (OpenPattern wt pat) = OpenPattern wt $ apPattern (fmap second fmqab) pat

mapPattern :: PurityFunction Maybe a b -> Pattern w q a -> Pattern w q b
mapPattern fmab (ClosedPattern fmqa) = ClosedPattern $ fmab . fmqa
mapPattern fmab (OpenPattern wt pat) = OpenPattern wt $ mapPattern (second fmab) pat

contramap1Pattern :: PurityFunction Maybe q p -> Pattern w p a -> Pattern w q a
contramap1Pattern qp (ClosedPattern pma) = ClosedPattern $ pma . qp
contramap1Pattern qp (OpenPattern wt pat) = OpenPattern wt $ contramap1Pattern qp pat

instance Applicative (Pattern w q) where
    pure a = ClosedPattern $ pure a
    ClosedPattern qmab <*> pat = apPattern qmab pat
    OpenPattern wt patab <*> pata = OpenPattern wt $ (\(t, ab) a -> (t, ab a)) <$> patab <*> pata

instance Category (Pattern w) where
    id = ClosedPattern id
    ClosedPattern bmc . pat = mapPattern bmc pat
    OpenPattern wt patbc . patab = OpenPattern wt $ patbc . patab

instance Arrow (Pattern w) where
    arr ab = ClosedPattern $ arr ab
    first (ClosedPattern amb) = ClosedPattern $ first amb
    first (OpenPattern wt pat) = OpenPattern wt $ fmap (\((t, c), d) -> (t, (c, d))) $ first pat

patternFreeWitnesses :: (forall t. w t -> r) -> Pattern w q a -> [r]
patternFreeWitnesses _wr (ClosedPattern _) = []
patternFreeWitnesses wr (OpenPattern wt pat) = (wr wt) : patternFreeWitnesses wr pat

varPattern :: w t -> Pattern w t ()
varPattern wt = OpenPattern wt $ ClosedPattern $ PureFunction $ \t -> (t, ())
