module Data.Shim.CCR.JMPolyShimWit where

import Shapes

import Data.Shim.CCR.JMPolyShim
import Data.Shim.Mono
import Data.Shim.Polar

type JMPolyShimWit :: forall k. (k -> Type) -> Polarity -> k -> Type
type JMPolyShimWit (wit :: k -> Type) polarity = PolarShimWit (JMPolyShim k) wit polarity

toJMPolyShimWit ::
    forall k wit (t :: k).
    ToPolarShimWit (JMPolyShim k) wit t =>
    JMPolyShimWit wit 'Positive t
toJMPolyShimWit = toPolarShimWit

fromJMPolyShimWit ::
    forall k wit (t :: k).
    FromPolarShimWit (JMPolyShim k) wit t =>
    JMPolyShimWit wit 'Negative t
fromJMPolyShimWit = fromPolarShimWit

jmToValue ::
    forall wit (t :: Type).
    ToPolarShimWit (JMPolyShim Type) wit t =>
    t ->
    SomeOf (JMPolyShimWit wit 'Positive)
jmToValue = MkSomeOf $ toJMPolyShimWit @Type @wit @t
