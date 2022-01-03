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
    -> [Either (AnyW poswit) (AnyW negwit)]
mappableGetWitnesses a =
    execWriter $
    mapWitnessesM
        @k
        (\t -> do
             tell $ pure $ Left $ MkAnyW t
             return t)
        (\t -> do
             tell $ pure $ Right $ MkAnyW t
             return t)
        a

instance WitnessMappable (poswit :: k -> Type) negwit (poswit t) where
    mapWitnessesM mapPos _ = mapPos

instance WitnessMappable poswit (negwit :: k -> Type) (negwit t) where
    mapWitnessesM _ mapNeg = mapNeg

instance WitnessMappable poswit negwit (AnyValue poswit) where
    mapWitnessesM mapPos _ (MkAnyValue tw val) = do
        tw' <- mapPos tw
        pure $ MkAnyValue tw' val

instance WitnessMappable poswit negwit (AnyW poswit) where
    mapWitnessesM mapPos _ (MkAnyW pa) = do
        pa' <- mapPos pa
        pure $ MkAnyW pa'

instance WitnessMappable poswit negwit (AnyW negwit) where
    mapWitnessesM _ mapNeg (MkAnyW pa) = do
        pa' <- mapNeg pa
        pure $ MkAnyW pa'

instance forall k (shim :: ShimKind k) (poswit :: k -> Type) (negwit :: k -> Type). Category shim =>
             WitnessMappable (PolarShimWit shim poswit 'Positive) (PolarShimWit shim negwit 'Negative) (AnyW poswit) where
    mapWitnessesM mapPos _ (MkAnyW w) = fmap (\(MkShimWit w' _) -> MkAnyW w') $ mapPos $ mkPolarShimWit w

instance forall k (shim :: ShimKind k) (poswit :: k -> Type) (negwit :: k -> Type). Category shim =>
             WitnessMappable (PolarShimWit shim poswit 'Positive) (PolarShimWit shim negwit 'Negative) (AnyW negwit) where
    mapWitnessesM _ mapNeg (MkAnyW w) = fmap (\(MkShimWit w' _) -> MkAnyW w') $ mapNeg $ mkPolarShimWit w
