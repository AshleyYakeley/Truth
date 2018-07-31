module Language.Expression.Named where

import Data.List (nub)
import Language.Expression.Expression
import Language.Expression.Sealed
import Shapes

data NamedWitness name w t =
    MkNamedWitness name
                   (w t)

instance Show name => Show (NamedWitness name w t) where
    show (MkNamedWitness name _) = show name

instance Show name => AllWitnessConstraint Show (NamedWitness name w) where
    allWitnessConstraint = Dict

type NamedExpression name w = Expression (NamedWitness name w)

type SealedNamedExpression name vw tw = SealedExpression (NamedWitness name vw) tw

newtype Bindings name vw tw =
    MkBindings [(name, SealedNamedExpression name vw tw)]
    deriving (Semigroup, Monoid)

bindingsDuplicates :: Eq name => Bindings name vw tw -> [name]
bindingsDuplicates (MkBindings bb) = let
    duplicates ::
           forall a. Eq a
        => [a]
        -> [a]
    duplicates [] = []
    duplicates (a:aa)
        | elem a aa = a : duplicates aa
    duplicates (_:aa) = duplicates aa
    in nub $ duplicates $ fmap fst bb

bindExpression :: name -> SealedNamedExpression name vw tw -> Bindings name vw tw
bindExpression name vexpr = MkBindings $ pure (name, vexpr)
