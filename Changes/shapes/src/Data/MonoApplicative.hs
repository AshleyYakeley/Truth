module Data.MonoApplicative where

import Shapes.Import

class (MonoPointed mono, MonoFunctor mono) => MonoApplicative mono where
    oliftA2 :: (Element mono -> Element mono -> Element mono) -> mono -> mono -> mono
    osequenceA ::
        forall t.
        Traversable t =>
        (t (Element mono) -> Element mono) ->
        t mono ->
        mono
    default oliftA2 ::
        (Applicative f, mono ~ f (Element mono)) =>
        (Element mono -> Element mono -> Element mono) -> mono -> mono -> mono
    oliftA2 = liftA2
    default osequenceA ::
        (Traversable t, Applicative f, mono ~ f (Element mono)) => (t (Element mono) -> Element mono) -> t mono -> mono
    osequenceA conv tfa = fmap conv $ sequenceA tfa

instance MonoApplicative (Identity a)

instance MonoApplicative (Maybe a)

instance MonoApplicative [a]

instance MonoApplicative (a -> b)

instance MonoApplicative (Either a b)

instance Monoid a => MonoApplicative (a, b)
