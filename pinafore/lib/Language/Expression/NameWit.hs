module Language.Expression.NameWit where

import Language.Expression.Expression
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
