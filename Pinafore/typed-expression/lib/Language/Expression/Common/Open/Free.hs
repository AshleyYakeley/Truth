module Language.Expression.Common.Open.Free where

import Shapes

class HasFreeWitnesses (f :: (kt -> Type) -> ka -> Type) where
    freeWitnesses :: forall (w :: kt -> Type) (a :: ka) (r :: Type). (forall (t :: kt). w t -> r) -> f w a -> [r]
    isClosed :: f w a -> Bool
    isClosed expr =
        case freeWitnesses (\_ -> ()) expr of
            [] -> True
            _ : _ -> False
    freeWitnessCount :: f w a -> Int
    freeWitnessCount expr = length $ freeWitnesses (\_ -> ()) expr
