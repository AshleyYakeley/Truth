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

type NamedBinder m name vw tw = Binder m (NamedWitness name vw) tw

mkNamedBinder :: Eq name => Binder m vw tw -> name -> NamedBinder m name vw tw
mkNamedBinder (MkBinder binder) name =
    MkBinder $ \twt (MkNamedWitness name' vwv) ->
        if name == name'
            then binder twt vwv
            else Nothing

newtype NamedBindings m name vw tw =
    MkNamedBindings [(name, SealedNamedExpression name vw tw)]
    deriving (Semigroup, Monoid)

namedBindingsToBindings ::
       Eq name => Binder m vw tw -> NamedBindings m name vw tw -> Bindings m (NamedWitness name vw) tw
namedBindingsToBindings binder (MkNamedBindings bb) =
    MkBindings $ fmap (\(name, expr) -> (mkNamedBinder binder name, expr)) bb

bindingsDuplicates :: Eq name => NamedBindings m name vw tw -> [name]
bindingsDuplicates (MkNamedBindings bb) = let
    duplicates ::
           forall a. Eq a
        => [a]
        -> [a]
    duplicates [] = []
    duplicates (a:aa)
        | elem a aa = a : duplicates aa
    duplicates (_:aa) = duplicates aa
    in nub $ duplicates $ fmap fst bb

bindExpression :: name -> SealedNamedExpression name vw tw -> NamedBindings m name vw tw
bindExpression name vexpr = MkNamedBindings $ pure (name, vexpr)

uncheckedBindingsLetExpression ::
       forall m name vw tw. (Eq name, Monad m)
    => Binder m vw tw
    -> NamedBindings m name vw tw
    -> SealedNamedExpression name vw tw
    -> m (SealedNamedExpression name vw tw)
uncheckedBindingsLetExpression binder nb = bindingsLetSealedExpression $ namedBindingsToBindings binder nb
