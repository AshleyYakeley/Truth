{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Pattern.Sealed where

import Language.Expression.Common.Pattern.Pattern
import Language.Expression.Common.WitnessTraversable
import Shapes

data SealedPattern (patwit :: Type -> Type) (negwit :: Type -> Type) =
    forall t. MkSealedPattern (negwit t)
                              (Pattern patwit t ())

instance IsPatternWitness poswit patwit => WitnessTraversable poswit negwit (SealedPattern patwit negwit) where
    traverseWitnessesM mapPos mapNeg =
        MkEndoM $ \(MkSealedPattern tt pat) -> do
            tt' <- unEndoM mapNeg tt
            pat' <- unEndoM (traverseWitnessesM mapPos mapNeg) pat
            pure $ MkSealedPattern tt' $ pat'

varSealedPattern :: negwit t -> patwit t -> SealedPattern patwit negwit
varSealedPattern twt vwt = MkSealedPattern twt $ varPattern vwt

anySealedPattern :: negwit t -> SealedPattern patwit negwit
anySealedPattern twt = MkSealedPattern twt anyPattern

sealedPatternFreeWitnesses :: (forall t. patwit t -> r) -> SealedPattern patwit negwit -> [r]
sealedPatternFreeWitnesses f (MkSealedPattern _ pat) = patternFreeWitnesses f pat
