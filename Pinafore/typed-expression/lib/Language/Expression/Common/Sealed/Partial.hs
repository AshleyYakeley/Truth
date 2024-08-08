{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Sealed.Partial where

import Language.Expression.Common.Sealed.Sealed
import Language.Expression.Common.WitnessMappable
import Shapes

data PartialWit (w :: Type -> Type) (t :: Type) where
    MkPartialWit :: forall w f t. PurityType Maybe f -> w t -> PartialWit w (f t)

type SealedPartialExpression (varw :: Type -> Type) (tw :: Type -> Type) = SealedExpression varw (PartialWit tw)

instance (forall t. WitnessMappable poswit negwit (varw t)) =>
             WitnessMappable poswit negwit (SealedPartialExpression varw poswit) where
    mapWitnessesM mapPos mapNeg =
        MkEndoM $ \(MkSealedExpression (MkPartialWit purity tt) expr) -> do
            tt' <- unEndoM mapPos tt
            expr' <- unEndoM (mapWitnessesM mapPos mapNeg) expr
            pure $ MkSealedExpression (MkPartialWit purity tt') expr'

sealedToPartialExpression :: SealedExpression varw tw -> SealedPartialExpression varw tw
sealedToPartialExpression (MkSealedExpression twt expr) =
    MkSealedExpression (MkPartialWit PureType twt) $ fmap Identity expr

partialToSealedExpression :: SealedPartialExpression varw tw -> SealedExpression varw tw
partialToSealedExpression (MkSealedExpression (MkPartialWit purity twt) expr) =
    MkSealedExpression twt $ fmap (runPurityCases purity) expr

neverSealedPartialExpression :: tw a -> SealedPartialExpression varw tw
neverSealedPartialExpression twt = MkSealedExpression (MkPartialWit ImpureType twt) $ pure Nothing
