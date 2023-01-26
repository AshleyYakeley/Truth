{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Pattern.Sealed where

import Language.Expression.Common.Pattern.Named
import Language.Expression.Common.Pattern.Pattern
import Language.Expression.Common.WitnessMappable
import Shapes

data SealedPattern (name :: Type) (vw :: Type -> Type) (tw :: Type -> Type) =
    forall t. MkSealedPattern (tw t)
                              (NamedPattern name vw t ())

instance WitnessMappable poswit negwit (SealedPattern name poswit negwit) where
    mapWitnessesM mapPos mapNeg =
        MkEndoM $ \(MkSealedPattern tt pat) -> do
            tt' <- unEndoM mapNeg tt
            pat' <- unEndoM (mapWitnessesM mapPos mapNeg) pat
            pure $ MkSealedPattern tt' $ pat'

varSealedPattern :: name -> tw t -> vw t -> SealedPattern name vw tw
varSealedPattern n twt vwt = MkSealedPattern twt $ varNamedPattern n vwt

anySealedPattern :: tw t -> SealedPattern name vw tw
anySealedPattern twt = MkSealedPattern twt anyPattern

sealedPatternNames :: SealedPattern name vw tw -> [name]
sealedPatternNames (MkSealedPattern _ pat) = patternNames pat
