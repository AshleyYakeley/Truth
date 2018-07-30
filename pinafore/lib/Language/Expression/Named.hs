module Language.Expression.Named where

import Language.Expression.Expression
import Shapes

data NamedWitness name w t =
    MkNamedWitness name
                   (w t)

instance Show name => Show (NamedWitness name w t) where
    show (MkNamedWitness name _) = show name

instance Show name => AllWitnessConstraint Show (NamedWitness name w) where
    allWitnessConstraint = Dict

type NamedExpression name w = Expression (NamedWitness name w)
