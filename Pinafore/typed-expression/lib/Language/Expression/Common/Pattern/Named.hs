{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Common.Pattern.Named where

import Language.Expression.Common.NameWit
import Language.Expression.Common.Named
import Language.Expression.Common.Pattern.Pattern
import Shapes

type NameTypePattern :: forall kn. (kn -> Type) -> (kn -> Type -> Type) -> Type -> Type -> Type
type NameTypePattern nw vw = Pattern (NameTypeWitness nw vw)

varNameTypePattern :: nw n -> vw n t -> NameTypePattern nw vw t ()
varNameTypePattern n t = varPattern $ MkNameTypeWitness n t

type NamedPattern :: Type -> (Type -> Type) -> Type -> Type -> Type
type NamedPattern name w = NameTypePattern (UnitType name) (UnitType' w)

patternNames :: NamedPattern name vw q a -> [name]
patternNames = patternFreeWitnesses $ \(MkNameWitness name _) -> name

varNamedPattern :: name -> vw t -> NamedPattern name vw t ()
varNamedPattern n t = varNameTypePattern (MkUnitType n) (MkUnitType' t)
