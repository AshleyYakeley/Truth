{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Pattern.Sealed where

import Shapes

import Language.Expression.Common.Open
import Language.Expression.Common.Pattern.Func
import Language.Expression.Common.Pattern.Named
import Language.Expression.Common.Pattern.Pattern

data SealedPattern (patwit :: Type -> Type) (expwit :: Type -> Type) (funcwit :: Type -> Type) (a :: Type)
    = forall t. MkSealedPattern
        (funcwit t)
        (FuncPattern patwit expwit t a)

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
