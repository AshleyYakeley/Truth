{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Sealed.Partial where

import Shapes

import Language.Expression.Common.Sealed.Sealed

data PartialWit (w :: Type -> Type) (t :: Type) where
    MkPartialWit :: forall w f t. PurityType Maybe f -> w t -> PartialWit w (f t)

type SealedPartialExpression (varw :: Type -> Type) (tw :: Type -> Type) = SealedExpression varw (PartialWit tw)

sealedToPartialExpression :: SealedExpression varw tw -> SealedPartialExpression varw tw
sealedToPartialExpression (MkSealedExpression twt expr) =
    MkSealedExpression (MkPartialWit PureType twt) $ fmap Identity expr

partialToSealedExpression :: String -> SealedPartialExpression varw tw -> SealedExpression varw tw
partialToSealedExpression err (MkSealedExpression (MkPartialWit purity twt) expr) =
    MkSealedExpression twt $ fmap (runPurityCases err purity) expr

neverSealedPartialExpression :: tw a -> SealedPartialExpression varw tw
neverSealedPartialExpression twt = MkSealedExpression (MkPartialWit ImpureType twt) $ pure Nothing
