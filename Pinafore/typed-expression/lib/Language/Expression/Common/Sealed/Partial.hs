{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Sealed.Partial where

import Language.Expression.Common.Sealed.Sealed
import Shapes

data PartialWit (w :: Type -> Type) (t :: Type) where
    MkPartialWit :: forall w f t. PurityType Maybe f -> w t -> PartialWit w (f t)

type SealedPartialExpression (varw :: Type -> Type) (tw :: Type -> Type) = SealedExpression varw (PartialWit tw)

sealedToPartialExpression :: SealedExpression varw tw -> SealedPartialExpression varw tw
sealedToPartialExpression (MkSealedExpression twt expr) =
    MkSealedExpression (MkPartialWit PureType twt) $ fmap Identity expr

partialToSealedExpression :: SealedPartialExpression varw tw -> SealedExpression varw tw
partialToSealedExpression (MkSealedExpression (MkPartialWit purity twt) expr) =
    MkSealedExpression twt $ fmap (runPurityCases purity) expr

neverSealedPartialExpression :: tw a -> SealedPartialExpression varw tw
neverSealedPartialExpression twt = MkSealedExpression (MkPartialWit ImpureType twt) $ pure Nothing
