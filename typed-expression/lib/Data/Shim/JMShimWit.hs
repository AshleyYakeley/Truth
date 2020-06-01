{-# OPTIONS -fno-warn-orphans #-}

module Data.Shim.JMShimWit where

import Data.Shim.JMShim
import Data.Shim.Polarity
import Data.Shim.ShimWit
import Shapes

type JMShimWit :: forall k. (k -> Type) -> Polarity -> k -> Type
type JMShimWit (wit :: k -> Type) = ShimWit (JMShim k) wit

toJMShimWit ::
       forall k wit (t :: k). ToShimWit (JMShim k) wit t
    => JMShimWit wit 'Positive t
toJMShimWit = toShimWit

fromJMShimWit ::
       forall k wit (t :: k). FromShimWit (JMShim k) wit t
    => JMShimWit wit 'Negative t
fromJMShimWit = fromShimWit

jmToValue ::
       forall wit (t :: Type). ToShimWit (JMShim Type) wit t
    => t
    -> AnyValue (JMShimWit wit 'Positive)
jmToValue = MkAnyValue $ toJMShimWit @Type @wit @t
