{-# OPTIONS -fno-warn-orphans #-}

module Data.Shim.JMShimWit where

import Data.Shim.JMShim
import Data.Shim.Polarity
import Data.Shim.ShimWit
import Shapes

type JMShimWit (wit :: k -> Type) = ShimWit JMShim wit

mkJMShimWit ::
       forall (k :: Type) polarity wit (t :: k).
       (InKind t, CoercibleKind k, InCategory (KindFunction :: k -> k -> Type), Is PolarityType polarity)
    => wit t
    -> JMShimWit wit polarity t
mkJMShimWit = mkShimWit

toJMShimWit ::
       forall wit t. ToShimWit JMShim wit t
    => JMShimWit wit 'Positive t
toJMShimWit = toShimWit

fromJMShimWit ::
       forall wit t. FromShimWit JMShim wit t
    => JMShimWit wit 'Negative t
fromJMShimWit = fromShimWit

jmToValue ::
       forall wit t. ToShimWit JMShim wit t
    => t
    -> AnyValue (JMShimWit wit 'Positive)
jmToValue = MkAnyValue $ toJMShimWit @wit @t
