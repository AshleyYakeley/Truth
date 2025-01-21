module Data.Shim.CCR.JMShimWit where

import Shapes

import Data.Shim.CCR.JM
import Data.Shim.Mono
import Data.Shim.Polar

type JMShimWit :: forall k. (k -> Type) -> Polarity -> k -> Type
type JMShimWit (wit :: k -> Type) polarity = PolarShimWit (JMShim k) wit polarity

toJMShimWit ::
    forall k wit (t :: k).
    ToPolarShimWit (JMShim k) wit t =>
    JMShimWit wit 'Positive t
toJMShimWit = toPolarShimWit

fromJMShimWit ::
    forall k wit (t :: k).
    FromPolarShimWit (JMShim k) wit t =>
    JMShimWit wit 'Negative t
fromJMShimWit = fromPolarShimWit

jmToValue ::
    forall wit (t :: Type).
    ToPolarShimWit (JMShim Type) wit t =>
    t ->
    SomeOf (JMShimWit wit 'Positive)
jmToValue = MkSomeOf $ toJMShimWit @Type @wit @t
