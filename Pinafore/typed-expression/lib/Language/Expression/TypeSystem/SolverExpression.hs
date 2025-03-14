module Language.Expression.TypeSystem.SolverExpression where

import Shapes

data SolverExpression typeexpr valexpr a
    = forall t. MkSolverExpression
        (typeexpr t)
        (valexpr (t -> a))

instance Functor valexpr => Functor (SolverExpression typeexpr valexpr) where
    fmap ab (MkSolverExpression uu exp) = MkSolverExpression uu $ fmap (fmap ab) exp

instance (Applicative typeexpr, Applicative valexpr) => Applicative (SolverExpression typeexpr valexpr) where
    pure a = MkSolverExpression (pure ()) $ pure $ \_ -> a
    liftA2 f (MkSolverExpression uua expa) (MkSolverExpression uub expb) =
        MkSolverExpression (liftA2 (,) uua uub) $ liftA2 (\fa fb (ta, tb) -> f (fa ta) (fb tb)) expa expb

instance Functor valexpr => Invariant (SolverExpression typeexpr valexpr) where
    invmap ab _ = fmap ab

instance (Applicative typeexpr, Applicative valexpr) => Productable (SolverExpression typeexpr valexpr)

instance (Applicative typeexpr, Applicative valexpr, Semigroup a) => Semigroup (SolverExpression typeexpr valexpr a) where
    (<>) = liftA2 (<>)

instance (Applicative typeexpr, Applicative valexpr, Monoid a) => Monoid (SolverExpression typeexpr valexpr a) where
    mempty = pure mempty

instance (AllConstraint Show typeexpr, AllConstraint Show valexpr) => Show (SolverExpression typeexpr valexpr a) where
    show (MkSolverExpression uu exp) = "{" <> allShow uu <> "; " <> allShow exp <> "}"

instance
    (AllConstraint Show typeexpr, AllConstraint Show valexpr) =>
    AllConstraint Show (SolverExpression typeexpr valexpr)
    where
    allConstraint = Dict

solverExpressionLiftValue :: (Applicative typeexpr, Functor valexpr) => valexpr a -> SolverExpression typeexpr valexpr a
solverExpressionLiftValue expr = MkSolverExpression (pure ()) $ fmap (\a _ -> a) expr

solverExpressionLiftType :: Applicative valexpr => typeexpr t -> SolverExpression typeexpr valexpr t
solverExpressionLiftType expr = MkSolverExpression expr $ pure id
