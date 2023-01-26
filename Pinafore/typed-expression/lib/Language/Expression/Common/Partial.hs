{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Partial where

import Language.Expression.Common.Sealed
import Language.Expression.Common.WitnessMappable
import Shapes

data PartialWit (w :: Type -> Type) (t :: Type) where
    MkPartialWit :: forall w f t. PurityType Maybe f -> w t -> PartialWit w (f t)

type SealedPartialExpression (name :: Type) (vw :: Type -> Type) (tw :: Type -> Type)
     = SealedExpression name vw (PartialWit tw)

instance WitnessMappable poswit negwit (SealedPartialExpression name negwit poswit) where
    mapWitnessesM mapPos mapNeg =
        MkEndoM $ \(MkSealedExpression (MkPartialWit purity tt) expr) -> do
            tt' <- unEndoM mapPos tt
            expr' <- unEndoM (mapWitnessesM mapPos mapNeg) expr
            pure $ MkSealedExpression (MkPartialWit purity tt') expr'

sealedToPartialExpression :: SealedExpression name vw tw -> SealedPartialExpression name vw tw
sealedToPartialExpression (MkSealedExpression twt expr) =
    MkSealedExpression (MkPartialWit PureType twt) $ fmap Identity expr

partialToSealedExpression :: SealedPartialExpression name vw tw -> SealedExpression name vw tw
partialToSealedExpression (MkSealedExpression (MkPartialWit purity twt) expr) =
    MkSealedExpression twt $ fmap (runPurityCases purity) expr

neverSealedPartialExpression :: tw a -> SealedPartialExpression name vw tw
neverSealedPartialExpression twt = MkSealedExpression (MkPartialWit ImpureType twt) $ pure Nothing
