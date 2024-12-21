{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Common.Pattern.Named where

import Language.Expression.Common.Open
import Language.Expression.Common.Pattern.Pattern
import Shapes

type NameTypePattern ::
       forall kn. (kn -> Type) -> (kn -> Type -> Type) -> (Type -> Type -> Type) -> Type -> Type -> Type
type NameTypePattern nw vw = Pattern (NameTypeWitness nw vw)

varNameTypePattern :: Arrow c => nw n -> vw n t -> NameTypePattern nw vw c t ()
varNameTypePattern n t = varPattern $ MkNameTypeWitness n t

type NamedPattern :: Type -> (Type -> Type) -> (Type -> Type -> Type) -> Type -> Type -> Type
type NamedPattern name w = NameTypePattern (UnitType name) (UnitType' w)

patternNames :: NamedPattern name vw c q a -> [name]
patternNames = patternFreeWitnesses $ \(MkNameWitness name _) -> name

varNamedPattern :: Arrow c => name -> vw t -> NamedPattern name vw c t ()
varNamedPattern n t = varNameTypePattern (MkUnitType n) (MkUnitType' t)
