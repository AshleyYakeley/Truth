module Language.Expression.Common.NameWit where

import Language.Expression.Common.Expression
import Language.Expression.Common.Pattern
import Shapes

data NameTypeWitness nw vw t =
    forall n. MkNameTypeWitness (nw n)
                                (vw n t)

instance (AllConstraint Show nw, AllConstraint (AllConstraint Show) vw) => Show (NameTypeWitness nw vw t) where
    show (MkNameTypeWitness namewit (typewit :: vw n t)) =
        allShow namewit <>
        ": " <>
        case allConstraint @_ @_ @(AllConstraint Show) @vw @n of
            Dict -> allShow typewit

instance (AllConstraint Show nw, AllConstraint (AllConstraint Show) vw) => AllConstraint Show (NameTypeWitness nw vw) where
    allConstraint = Dict

type NameTypeExpression nw vw = Expression (NameTypeWitness nw vw)

varNameTypeExpression :: nw n -> vw n t -> NameTypeExpression nw vw t
varNameTypeExpression n t = varExpression $ MkNameTypeWitness n t

type NameTypePattern nw vw = Pattern (NameTypeWitness nw vw)

varNameTypePattern :: nw n -> vw n t -> NameTypePattern nw vw t ()
varNameTypePattern n t = varPattern $ MkNameTypeWitness n t
