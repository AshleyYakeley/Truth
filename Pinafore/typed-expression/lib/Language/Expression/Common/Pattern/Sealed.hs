{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Pattern.Sealed where

import Language.Expression.Common.Open.Named
import Language.Expression.Common.Pattern.Func
import Language.Expression.Common.Pattern.Named
import Language.Expression.Common.Pattern.Pattern
import Language.Expression.Common.WitnessMappable
import Shapes

data SealedPattern (patwit :: Type -> Type) (expwit :: Type -> Type) (funcwit :: Type -> Type) (a :: Type) =
    forall t. MkSealedPattern (funcwit t)
                              (FuncPattern patwit expwit t a)

instance ( forall t. WitnessMappable poswit negwit (patwit t)
         , forall t. WitnessMappable poswit negwit (expwit t)
         , forall t. WitnessMappable poswit negwit (funcwit t)
         ) => WitnessMappable poswit negwit (SealedPattern patwit expwit funcwit a) where
    mapWitnessesM mapPos mapNeg =
        MkEndoM $ \(MkSealedPattern tt pat) -> do
            tt' <- unEndoM (mapWitnessesM mapPos mapNeg) tt
            pat' <- unEndoM (mapWitnessesM mapPos mapNeg) pat
            pure $ MkSealedPattern tt' $ pat'

instance (AllConstraint Show patwit, AllConstraint Show funcwit) => Show (SealedPattern patwit expwit funcwit a) where
    show (MkSealedPattern t expr) = show expr <> " => " <> allShow t

anySealedPattern :: funcwit t -> SealedPattern patwit expwit funcwit ()
anySealedPattern twt = MkSealedPattern twt anyPattern

varSealedPattern :: funcwit t -> patwit t -> SealedPattern patwit expwit funcwit ()
varSealedPattern twt var = MkSealedPattern twt $ varPattern var

type NamedSealedPattern name poswit negwit = SealedPattern (NameWitness name poswit) (NameWitness name negwit) negwit

varNamedSealedPattern :: name -> negwit t -> poswit t -> NamedSealedPattern name poswit negwit ()
varNamedSealedPattern n twt vwt = varSealedPattern twt $ MkNameWitness n vwt

functionPatternNames :: NamedSealedPattern name poswit negwit a -> [name]
functionPatternNames (MkSealedPattern _ pat) = patternNames pat
