module Language.Expression.Common.Open.Abstract where

import Shapes

class AbstractWitness (f :: (Type -> Type) -> Type -> Type) where
    abstractWitness :: TestEquality w => w a -> f w b -> f w (a -> b)

abstractWitnesses :: (Functor (f w), AbstractWitness f, TestEquality w) => [SomeFor ((->) a) w] -> f w b -> f w (a -> b)
abstractWitnesses [] fwb = fmap (\b _ -> b) fwb
abstractWitnesses (MkSomeFor wt at : ww) fwb =
    fmap (\tab a -> tab (at a) a) $ abstractWitness wt $ abstractWitnesses ww fwb
