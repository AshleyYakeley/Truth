{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.WitnessMappable where

import Data.Shim
import Shapes

class WitnessMappable (poswit :: k -> Type) (negwit :: k -> Type) (a :: Type) where
    mapWitnessesM ::
           forall m. Applicative m
        => (forall (t :: k). poswit t -> m (poswit t))
        -> (forall (t :: k). negwit t -> m (negwit t))
        -> a
        -> m a

mapWitnesses ::
       forall k (poswit :: k -> Type) (negwit :: k -> Type) (a :: Type). WitnessMappable poswit negwit a
    => (forall (t :: k). poswit t -> poswit t)
    -> (forall (t :: k). negwit t -> negwit t)
    -> a
    -> a
mapWitnesses mapPos mapNeg a = runIdentity $ mapWitnessesM (\t -> Identity $ mapPos t) (\t -> Identity $ mapNeg t) a

mappableGetWitnesses ::
       forall k (poswit :: k -> Type) (negwit :: k -> Type) a. (WitnessMappable poswit negwit a)
    => a
    -> [Either (Some poswit) (Some negwit)]
mappableGetWitnesses a =
    execWriter $
    mapWitnessesM
        @k
        (\t -> do
             tell $ pure $ Left $ MkSome t
             return t)
        (\t -> do
             tell $ pure $ Right $ MkSome t
             return t)
        a

instance WitnessMappable poswit negwit () where
    mapWitnessesM _ _ = pure

instance WitnessMappable poswit negwit Void where
    mapWitnessesM _ _ = absurd

instance (WitnessMappable poswit negwit p, WitnessMappable poswit negwit q) => WitnessMappable poswit negwit (p, q) where
    mapWitnessesM mapPos mapNeg (p, q) = (,) <$> mapWitnessesM mapPos mapNeg p <*> mapWitnessesM mapPos mapNeg q

instance (WitnessMappable poswit negwit p, WitnessMappable poswit negwit q) =>
             WitnessMappable poswit negwit (Either p q) where
    mapWitnessesM mapPos mapNeg (Left p) = Left <$> mapWitnessesM mapPos mapNeg p
    mapWitnessesM mapPos mapNeg (Right q) = Right <$> mapWitnessesM mapPos mapNeg q

instance WitnessMappable (poswit :: k -> Type) negwit (poswit t) where
    mapWitnessesM mapPos _ = mapPos

instance WitnessMappable poswit (negwit :: k -> Type) (negwit t) where
    mapWitnessesM _ mapNeg = mapNeg

instance WitnessMappable poswit negwit (SomeOf poswit) where
    mapWitnessesM mapPos _ (MkSomeOf tw val) = do
        tw' <- mapPos tw
        pure $ MkSomeOf tw' val

instance WitnessMappable poswit negwit (Some poswit) where
    mapWitnessesM mapPos _ (MkSome pa) = do
        pa' <- mapPos pa
        pure $ MkSome pa'

instance WitnessMappable poswit negwit (Some negwit) where
    mapWitnessesM _ mapNeg (MkSome pa) = do
        pa' <- mapNeg pa
        pure $ MkSome pa'

instance forall k (shim :: ShimKind k) (poswit :: k -> Type) (negwit :: k -> Type). Category shim =>
             WitnessMappable (PolarShimWit shim poswit 'Positive) (PolarShimWit shim negwit 'Negative) (Some poswit) where
    mapWitnessesM mapPos _ (MkSome w) = fmap (\(MkShimWit w' _) -> MkSome w') $ mapPos $ mkPolarShimWit w

instance forall k (shim :: ShimKind k) (poswit :: k -> Type) (negwit :: k -> Type). Category shim =>
             WitnessMappable (PolarShimWit shim poswit 'Positive) (PolarShimWit shim negwit 'Negative) (Some negwit) where
    mapWitnessesM _ mapNeg (MkSome w) = fmap (\(MkShimWit w' _) -> MkSome w') $ mapNeg $ mkPolarShimWit w
