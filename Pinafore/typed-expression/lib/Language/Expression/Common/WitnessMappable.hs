{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.WitnessMappable where

--import Data.Shim
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

instance (forall a. WitnessMappable poswit negwit (w a)) => WitnessMappable poswit negwit (SomeFor f w) where
    mapWitnessesM mapPos mapNeg = endoSomeFor $ mapWitnessesM mapPos mapNeg

instance (forall a. WitnessMappable poswit negwit (w a)) => WitnessMappable poswit negwit (ListType w tt) where
    mapWitnessesM mapPos mapNeg =
        MkEndoM $ \case
            NilListType -> pure NilListType
            ConsListType wa la ->
                ConsListType <$> unEndoM (mapWitnessesM mapPos mapNeg) wa <*> unEndoM (mapWitnessesM mapPos mapNeg) la
