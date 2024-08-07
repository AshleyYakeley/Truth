module Language.Expression.Common.NameWit where

import Language.Expression.Common.Expression
import Shapes

type NameTypeWitness :: forall kn kt. (kn -> Type) -> (kn -> kt -> Type) -> kt -> Type
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

type NameTypeExpression :: (kn -> Type) -> (kn -> Type -> Type) -> Type -> Type
type NameTypeExpression nw vw = FunctionExpression (NameTypeWitness nw vw)

varNameTypeExpression :: nw n -> vw n t -> NameTypeExpression nw vw t
varNameTypeExpression n t = varExpression $ MkNameTypeWitness n t
