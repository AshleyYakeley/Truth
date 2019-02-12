module Language.Expression.NameWit where

import Language.Expression.Expression
import Language.Expression.Pattern
import Shapes

data NameTypeWitness nw vw t =
    forall n. MkNameTypeWitness (nw n)
                                (vw n t)

instance AllWitnessConstraint Show nw => Show (NameTypeWitness nw vw t) where
    show (MkNameTypeWitness n _) = showAllWitness n

instance AllWitnessConstraint Show nw => AllWitnessConstraint Show (NameTypeWitness nw vw) where
    allWitnessConstraint = Dict

type NameTypeExpression nw vw = Expression (NameTypeWitness nw vw)

varNameTypeExpression :: nw n -> vw n t -> NameTypeExpression nw vw t
varNameTypeExpression n t = varExpression $ MkNameTypeWitness n t

type NameTypePattern nw vw = Pattern (NameTypeWitness nw vw)

varNameTypePattern :: nw n -> vw n t -> NameTypePattern nw vw t ()
varNameTypePattern n t = varPattern $ MkNameTypeWitness n t
