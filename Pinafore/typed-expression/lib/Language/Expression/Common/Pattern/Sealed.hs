{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Pattern.Sealed where

import Language.Expression.Common.Pattern.Named
import Language.Expression.Common.Pattern.Pattern
import Language.Expression.Common.WitnessMappable
import Shapes

data SealedPattern (name :: Type) (poswit :: Type -> Type) (negwit :: Type -> Type) =
    forall t. MkSealedPattern (negwit t)
                              (NamedPattern name poswit t ())

instance WitnessMappable poswit negwit (SealedPattern name poswit negwit) where
    mapWitnessesM mapPos mapNeg =
        MkEndoM $ \(MkSealedPattern tt pat) -> do
            tt' <- unEndoM mapNeg tt
            pat' <- unEndoM (mapWitnessesM mapPos mapNeg) pat
            pure $ MkSealedPattern tt' $ pat'

instance (Show name, AllConstraint Show poswit, AllConstraint Show negwit) => Show (SealedPattern name poswit negwit) where
    show (MkSealedPattern t expr) = show expr <> " => " <> allShow t

varSealedPattern :: name -> tw t -> vw t -> SealedPattern name vw tw
varSealedPattern n twt vwt = MkSealedPattern twt $ varNamedPattern n vwt

anySealedPattern :: tw t -> SealedPattern name vw tw
anySealedPattern twt = MkSealedPattern twt anyPattern

sealedPatternNames :: SealedPattern name vw tw -> [name]
sealedPatternNames (MkSealedPattern _ pat) = patternNames pat
