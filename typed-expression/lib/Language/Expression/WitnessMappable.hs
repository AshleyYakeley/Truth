module Language.Expression.WitnessMappable where

import Data.Shim
import Shapes

class WitnessMappable (poswit :: k -> Type) (negwit :: k -> Type) (a :: Type) where
    mapWitnessesM ::
           forall m. Monad m
        => (forall (t :: k). InKind t => poswit t -> m (poswit t))
        -> (forall (t :: k). InKind t => negwit t -> m (negwit t))
        -> a
        -> m a

mapWitnesses ::
       forall k (poswit :: k -> Type) (negwit :: k -> Type) (a :: Type). WitnessMappable poswit negwit a
    => (forall (t :: k). InKind t => poswit t -> poswit t)
    -> (forall (t :: k). InKind t => negwit t -> negwit t)
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

instance InKind t => WitnessMappable (poswit :: k -> Type) negwit (poswit t) where
    mapWitnessesM mapPos _ = mapPos

instance InKind t => WitnessMappable poswit (negwit :: k -> Type) (negwit t) where
    mapWitnessesM _ mapNeg = mapNeg

instance WitnessMappable poswit negwit (AnyInKind poswit) where
    mapWitnessesM mapPos _ (MkAnyInKind pa) = do
        pa' <- mapPos pa
        return $ MkAnyInKind pa'

instance WitnessMappable poswit negwit (AnyInKind negwit) where
    mapWitnessesM _ mapNeg (MkAnyInKind pa) = do
        pa' <- mapNeg pa
        return $ MkAnyInKind pa'

instance forall k (shim :: MapKind k) (poswit :: k -> Type) (negwit :: k -> Type). InCategory shim =>
             WitnessMappable (ShimWit shim poswit 'Positive) (ShimWit shim negwit 'Negative) (AnyInKind poswit) where
    mapWitnessesM mapPos _ (MkAnyInKind w) = do
        MkShimWit w' _ <- mapPos $ mkShimWit w
        return $ MkAnyInKind w'

instance forall k (shim :: MapKind k) (poswit :: k -> Type) (negwit :: k -> Type). InCategory shim =>
             WitnessMappable (ShimWit shim poswit 'Positive) (ShimWit shim negwit 'Negative) (AnyInKind negwit) where
    mapWitnessesM _ mapNeg (MkAnyInKind w) = do
        MkShimWit w' _ <- mapNeg $ mkShimWit w
        return $ MkAnyInKind w'
