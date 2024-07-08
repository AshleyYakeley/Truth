{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Pattern.Sealed where

import Language.Expression.Common.Named
import Language.Expression.Common.Pattern.Named
import Language.Expression.Common.Pattern.Pattern
import Language.Expression.Common.WitnessMappable
import Shapes

data SealedPattern (varw :: Type -> Type) (negwit :: Type -> Type) =
    forall t. MkSealedPattern (negwit t)
                              (Pattern varw t ())

instance (forall t. WitnessMappable poswit negwit (varw t), forall t. WitnessMappable poswit negwit (ww t)) =>
             WitnessMappable poswit negwit (SealedPattern varw ww) where
    mapWitnessesM mapPos mapNeg =
        MkEndoM $ \(MkSealedPattern tt pat) -> do
            tt' <- unEndoM (mapWitnessesM mapPos mapNeg) tt
            pat' <- unEndoM (mapWitnessesM mapPos mapNeg) pat
            pure $ MkSealedPattern tt' $ pat'

instance (AllConstraint Show varw, AllConstraint Show negwit) => Show (SealedPattern varw negwit) where
    show (MkSealedPattern t expr) = show expr <> " => " <> allShow t

anySealedPattern :: tw t -> SealedPattern varw tw
anySealedPattern twt = MkSealedPattern twt anyPattern

type SealedNamedPattern name vw = SealedPattern (NameWitness name vw)

varSealedPattern :: name -> tw t -> vw t -> SealedNamedPattern name vw tw
varSealedPattern n twt vwt = MkSealedPattern twt $ varNamedPattern n vwt

sealedPatternNames :: SealedNamedPattern name vw tw -> [name]
sealedPatternNames (MkSealedPattern _ pat) = patternNames pat
