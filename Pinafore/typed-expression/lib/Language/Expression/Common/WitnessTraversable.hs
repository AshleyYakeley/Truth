{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.WitnessTraversable where

import Data.Shim
import Shapes

class WitnessTraversable (poswit :: k -> Type) (negwit :: k -> Type) (a :: Type) where
    traverseWitnessesM ::
           forall m. Applicative m
        => EndoM' m poswit
        -> EndoM' m negwit
        -> EndoM m a

traverseWitnesses ::
       forall k (poswit :: k -> Type) (negwit :: k -> Type) (a :: Type). WitnessTraversable poswit negwit a
    => Endo' poswit
    -> Endo' negwit
    -> Endo a
traverseWitnesses mapPos mapNeg = endoMToEndo $ traverseWitnessesM (endoToEndoM mapPos) (endoToEndoM mapNeg)

traversableGetWitnesses ::
       forall k (poswit :: k -> Type) (negwit :: k -> Type) a. (WitnessTraversable poswit negwit a)
    => a
    -> [Either (Some poswit) (Some negwit)]
traversableGetWitnesses =
    execEndoMWriter $
    traverseWitnessesM @k (tellEndoM $ \t -> pure $ Left $ MkSome t) (tellEndoM $ \t -> pure $ Right $ MkSome t)

instance WitnessTraversable poswit negwit () where
    traverseWitnessesM _ _ = rUnit

instance WitnessTraversable poswit negwit Void where
    traverseWitnessesM _ _ = rVoid

instance (WitnessTraversable poswit negwit p, WitnessTraversable poswit negwit q) =>
             WitnessTraversable poswit negwit (p, q) where
    traverseWitnessesM mapPos mapNeg = traverseWitnessesM mapPos mapNeg <***> traverseWitnessesM mapPos mapNeg

instance (WitnessTraversable poswit negwit p, WitnessTraversable poswit negwit q) =>
             WitnessTraversable poswit negwit (Either p q) where
    traverseWitnessesM mapPos mapNeg = traverseWitnessesM mapPos mapNeg <+++> traverseWitnessesM mapPos mapNeg

instance WitnessTraversable (poswit :: k -> Type) negwit (poswit t) where
    traverseWitnessesM mapPos _ = mapPos

instance WitnessTraversable poswit (negwit :: k -> Type) (negwit t) where
    traverseWitnessesM _ mapNeg = mapNeg

instance WitnessTraversable poswit negwit (SomeOf poswit) where
    traverseWitnessesM mapPos _ = endoSomeFor mapPos

instance WitnessTraversable poswit negwit (Some poswit) where
    traverseWitnessesM mapPos _ = endoSomeFor mapPos

instance WitnessTraversable poswit negwit (Some negwit) where
    traverseWitnessesM _ mapNeg = endoSomeFor mapNeg

endoExtractShimWit :: (Functor m, Category shim) => EndoM' m (ShimWit shim w) -> EndoM m (Some w)
endoExtractShimWit ems = MkEndoM $ \(MkSome w) -> fmap (\(MkShimWit w' _) -> MkSome w') $ unEndoM ems $ mkShimWit w

instance forall k (shim :: ShimKind k) (poswit :: k -> Type) (negwit :: k -> Type). Category shim =>
             WitnessTraversable (PolarShimWit shim poswit 'Positive) (PolarShimWit shim negwit 'Negative) (Some poswit) where
    traverseWitnessesM mapPos _ = endoExtractShimWit mapPos

instance forall k (shim :: ShimKind k) (poswit :: k -> Type) (negwit :: k -> Type). Category shim =>
             WitnessTraversable (PolarShimWit shim poswit 'Positive) (PolarShimWit shim negwit 'Negative) (Some negwit) where
    traverseWitnessesM _ mapNeg = endoExtractShimWit mapNeg
