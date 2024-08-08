{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.TypeSystem.WitnessMappable where

import Data.Shim
import Language.Expression.Common
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

instance (forall t. WitnessMappable poswit negwit (e t)) => WitnessMappable poswit negwit (PurityFunction m e a b) where
    mapWitnessesM mapPos mapNeg =
        MkEndoM $ \case
            MkPurityFunction pt ekfab -> fmap (MkPurityFunction pt) $ unEndoM (mapWitnessesM mapPos mapNeg) ekfab

type PShimWitMappable (shim :: ShimKind k) (wit :: Polarity -> k -> Type)
     = WitnessMappable (PShimWit shim wit 'Positive) (PShimWit shim wit 'Negative)

mapPShimWitsM ::
       forall m shim wit a. (Category shim, Applicative m, PShimWitMappable shim wit a)
    => (forall t. wit 'Positive t -> m (PShimWit shim wit 'Positive t))
    -> (forall t. wit 'Negative t -> m (PShimWit shim wit 'Negative t))
    -> EndoM m a
mapPShimWitsM mapPos mapNeg = mapWitnessesM (MkEndoM $ chainPolarShimWitM mapPos) (MkEndoM $ chainPolarShimWitM mapNeg)

mapPShimWits ::
       forall shim wit a. (Category shim, PShimWitMappable shim wit a)
    => (forall t. wit 'Positive t -> PShimWit shim wit 'Positive t)
    -> (forall t. wit 'Negative t -> PShimWit shim wit 'Negative t)
    -> Endo a
mapPShimWits mapPos mapNeg = mapWitnesses (Endo $ chainPolarShimWit mapPos) (Endo $ chainPolarShimWit mapNeg)

class (forall a. WitnessMappable poswit negwit (f a)) =>
          WitnessMappable1 (poswit :: k -> Type) (negwit :: k -> Type) (f :: Type -> Type)

instance (forall a. WitnessMappable poswit negwit (f a)) =>
             WitnessMappable1 (poswit :: k -> Type) (negwit :: k -> Type) (f :: Type -> Type)

instance (forall t. WitnessMappable poswit negwit (w t)) => WitnessMappable poswit negwit (Expression w a) where
    mapWitnessesM mapPos mapNeg =
        MkEndoM $ \case
            ClosedExpression a -> pure $ ClosedExpression a
            OpenExpression wt expr ->
                liftA2
                    OpenExpression
                    (unEndoM (mapWitnessesM mapPos mapNeg) wt)
                    (unEndoM (mapWitnessesM mapPos mapNeg) expr)

instance (forall t. WitnessMappable poswit negwit (w t), forall t. WitnessMappable poswit negwit (c a t)) =>
             WitnessMappable poswit negwit (Pattern w c a b) where
    mapWitnessesM mapPos mapNeg = let
        mapNW :: EndoM' _ w
        mapNW = MkEndoM $ \wt -> unEndoM (mapWitnessesM mapPos mapNeg) wt
        in MkEndoM $ \(MkPattern ww pf) ->
               liftA2 MkPattern (unEndoM (endoFor $ endoSomeFor mapNW) ww) (unEndoM (mapWitnessesM mapPos mapNeg) pf)

instance (forall t. WitnessMappable poswit negwit (w t)) => WitnessMappable poswit negwit (LiftedExpression f w a) where
    mapWitnessesM mapPos mapNeg =
        MkEndoM $ \case
            ClosedLiftedExpression a -> pure $ ClosedLiftedExpression a
            OpenLiftedExpression wt expr ->
                liftA2
                    OpenLiftedExpression
                    (unEndoM (mapWitnessesM mapPos mapNeg) wt)
                    (unEndoM (mapWitnessesM mapPos mapNeg) expr)

instance WitnessMappable poswit negwit (NameWitness name poswit t) where
    mapWitnessesM mapPos _mapNeg =
        MkEndoM $ \(MkNameWitness name tt) -> do
            tt' <- unEndoM mapPos tt
            return $ MkNameWitness name tt'

instance WitnessMappable poswit negwit (NameWitness name negwit t) where
    mapWitnessesM _mapPos mapNeg =
        MkEndoM $ \(MkNameWitness name tt) -> do
            tt' <- unEndoM mapNeg tt
            return $ MkNameWitness name tt'

instance ( forall t. WitnessMappable poswit negwit (patwit t)
         , forall t. WitnessMappable poswit negwit (expwit t)
         , forall t. WitnessMappable poswit negwit (funcwit t)
         ) => WitnessMappable poswit negwit (SealedPattern patwit expwit funcwit a) where
    mapWitnessesM mapPos mapNeg =
        MkEndoM $ \(MkSealedPattern tt pat) -> do
            tt' <- unEndoM (mapWitnessesM mapPos mapNeg) tt
            pat' <- unEndoM (mapWitnessesM mapPos mapNeg) pat
            pure $ MkSealedPattern tt' $ pat'

instance ( forall t. WitnessMappable poswit negwit (patwit t)
         , forall t. WitnessMappable poswit negwit (expwit t)
         , forall t. WitnessMappable poswit negwit (funcwit t)
         ) =>
             WitnessMappable (poswit :: Type -> Type) (negwit :: Type -> Type) (PatternConstructor patwit expwit funcwit poswit) where
    mapWitnessesM mapPos mapNeg =
        MkEndoM $ \(MkPatternConstructor (lvw :: ListType wit lt) pat) -> do
            pat' <- unEndoM (mapWitnessesM @Type mapPos mapNeg) pat
            hwit <- unEndoM (mapWitnessesM @Type (liftListProductPolwit mapPos) mapNeg) $ MkListProductType lvw
            pure $
                case hwit of
                    MkListProductType (lvw' :: ListType wit lt') ->
                        case injectiveListProduct @lt @lt' of
                            Refl -> MkPatternConstructor lvw' pat'

instance (forall t. WitnessMappable poswit negwit (varw t)) =>
             WitnessMappable poswit negwit (SealedExpression varw poswit) where
    mapWitnessesM mapPos mapNeg =
        MkEndoM $ \(MkSealedExpression tt expr) -> do
            tt' <- unEndoM mapPos tt
            expr' <- unEndoM (mapWitnessesM mapPos mapNeg) expr
            pure $ MkSealedExpression tt' expr'

instance (forall t. WitnessMappable poswit negwit (varw t)) =>
             WitnessMappable poswit negwit (SealedPartialExpression varw poswit) where
    mapWitnessesM mapPos mapNeg =
        MkEndoM $ \(MkSealedExpression (MkPartialWit purity tt) expr) -> do
            tt' <- unEndoM mapPos tt
            expr' <- unEndoM (mapWitnessesM mapPos mapNeg) expr
            pure $ MkSealedExpression (MkPartialWit purity tt') expr'

instance (forall t. WitnessMappable poswit negwit (varw t)) =>
             WitnessMappable poswit negwit (SealedFExpression varw poswit f) where
    mapWitnessesM mapPos mapNeg =
        MkEndoM $ \(MkSealedFExpression tt expr) -> do
            tt' <- unEndoM mapPos tt
            expr' <- unEndoM (mapWitnessesM mapPos mapNeg) expr
            pure $ MkSealedFExpression tt' expr'
