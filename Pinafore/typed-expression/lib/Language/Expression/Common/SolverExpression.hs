module Language.Expression.Common.SolverExpression where

import Shapes

data SolverExpression poswit negwit typeexpr valexpr a =
    forall t. MkSolverExpression (typeexpr t)
                                 (valexpr (t -> a))

instance Functor valexpr => Functor (SolverExpression poswit negwit typeexpr valexpr) where
    fmap ab (MkSolverExpression uu exp) = MkSolverExpression uu $ fmap (fmap ab) exp

instance (Applicative typeexpr, Applicative valexpr) => Applicative (SolverExpression poswit negwit typeexpr valexpr) where
    pure a = MkSolverExpression (pure ()) $ pure $ \_ -> a
    liftA2 f (MkSolverExpression uua expa) (MkSolverExpression uub expb) =
        MkSolverExpression (liftA2 (,) uua uub) $ liftA2 (\fa fb (ta, tb) -> f (fa ta) (fb tb)) expa expb

instance Functor valexpr => Invariant (SolverExpression poswit negwit typeexpr valexpr) where
    invmap ab _ = fmap ab

instance (Applicative typeexpr, Applicative valexpr) => Productish (SolverExpression poswit negwit typeexpr valexpr)

solverLiftValExpression ::
       (Applicative typeexpr, Functor valexpr) => valexpr a -> SolverExpression poswit negwit typeexpr valexpr a
solverLiftValExpression expr = MkSolverExpression (pure ()) $ fmap (\a _ -> a) expr

solverLiftTypeExpression :: Applicative valexpr => typeexpr t -> SolverExpression poswit negwit typeexpr valexpr t
solverLiftTypeExpression expr = MkSolverExpression expr $ pure id
