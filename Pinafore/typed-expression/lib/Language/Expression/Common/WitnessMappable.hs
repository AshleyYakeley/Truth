{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.WitnessMappable where

import Data.Shim
import Shapes

class WitnessMappable (poswit :: k -> Type) (negwit :: k -> Type) (a :: Type) where
    mapWitnessesM ::
           forall m. Applicative m
        => EndoM' m poswit
        -> EndoM' m negwit
        -> EndoM m a

mapWitnesses ::
       forall k (poswit :: k -> Type) (negwit :: k -> Type) (a :: Type). WitnessMappable poswit negwit a
    => Endo' poswit
    -> Endo' negwit
    -> Endo a
mapWitnesses mapPos mapNeg = endoMToEndo $ mapWitnessesM (endoToEndoM mapPos) (endoToEndoM mapNeg)

mappableGetWitnesses ::
       forall k (poswit :: k -> Type) (negwit :: k -> Type) a. (WitnessMappable poswit negwit a)
    => a
    -> [Either (Some poswit) (Some negwit)]
mappableGetWitnesses =
    execEndoMWriter $
    mapWitnessesM @k (tellEndoM $ \t -> pure $ Left $ MkSome t) (tellEndoM $ \t -> pure $ Right $ MkSome t)

instance WitnessMappable poswit negwit () where
    mapWitnessesM _ _ = rUnit

instance WitnessMappable poswit negwit Void where
    mapWitnessesM _ _ = rVoid

instance (WitnessMappable poswit negwit p, WitnessMappable poswit negwit q) => WitnessMappable poswit negwit (p, q) where
    mapWitnessesM mapPos mapNeg = mapWitnessesM mapPos mapNeg <***> mapWitnessesM mapPos mapNeg

instance (WitnessMappable poswit negwit p, WitnessMappable poswit negwit q) =>
             WitnessMappable poswit negwit (Either p q) where
    mapWitnessesM mapPos mapNeg = mapWitnessesM mapPos mapNeg <+++> mapWitnessesM mapPos mapNeg

instance WitnessMappable (poswit :: k -> Type) negwit (poswit t) where
    mapWitnessesM mapPos _ = mapPos

instance WitnessMappable poswit (negwit :: k -> Type) (negwit t) where
    mapWitnessesM _ mapNeg = mapNeg

instance WitnessMappable poswit negwit (SomeOf poswit) where
    mapWitnessesM mapPos _ = endoSomeFor mapPos

instance WitnessMappable poswit negwit (Some poswit) where
    mapWitnessesM mapPos _ = endoSomeFor mapPos

instance WitnessMappable poswit negwit (Some negwit) where
    mapWitnessesM _ mapNeg = endoSomeFor mapNeg

endoExtractShimWit :: (Functor m, Category shim) => EndoM' m (ShimWit shim w) -> EndoM m (Some w)
endoExtractShimWit ems = MkEndoM $ \(MkSome w) -> fmap (\(MkShimWit w' _) -> MkSome w') $ unEndoM ems $ mkShimWit w

instance forall k (shim :: ShimKind k) (poswit :: k -> Type) (negwit :: k -> Type). Category shim =>
             WitnessMappable (PolarShimWit shim poswit 'Positive) (PolarShimWit shim negwit 'Negative) (Some poswit) where
    mapWitnessesM mapPos _ = endoExtractShimWit mapPos

instance forall k (shim :: ShimKind k) (poswit :: k -> Type) (negwit :: k -> Type). Category shim =>
             WitnessMappable (PolarShimWit shim poswit 'Positive) (PolarShimWit shim negwit 'Negative) (Some negwit) where
    mapWitnessesM _ mapNeg = endoExtractShimWit mapNeg

instance (forall a. WitnessMappable poswit negwit (w a)) => WitnessMappable poswit negwit (ListType w tt) where
    mapWitnessesM mapPos mapNeg =
        MkEndoM $ \case
            NilListType -> pure NilListType
            ConsListType wa la ->
                ConsListType <$> unEndoM (mapWitnessesM mapPos mapNeg) wa <*> unEndoM (mapWitnessesM mapPos mapNeg) la
