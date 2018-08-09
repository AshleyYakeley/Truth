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

data Joiner w ta tb where
    MkJoiner :: w tab -> (tab -> ta) -> (tab -> tb) -> Joiner w ta tb

data TypeJoiner m w where
    MkTypeJoiner :: w t0 -> (forall ta tb. w ta -> w tb -> m (Joiner w ta tb)) -> TypeJoiner m w

varNameTypeExpression :: nw n -> vw n t -> NameTypeExpression nw vw t
varNameTypeExpression n t = varExpression $ MkNameTypeWitness n t

abstractNTExpression ::
       (TestEquality nw, Monad m)
    => TypeJoiner m (vw n)
    -> nw n
    -> NameTypeExpression nw vw a
    -> (forall t. vw n t -> NameTypeExpression nw vw (t -> a) -> m r)
    -> m r
abstractNTExpression (MkTypeJoiner zeroWit _) _ (ClosedExpression a) cont = cont zeroWit $ ClosedExpression $ \_ -> a
abstractNTExpression joiner@(MkTypeJoiner _ joinWit) nwn (OpenExpression (MkNameTypeWitness nwn' vwt) expr) cont
    | Just Refl <- testEquality nwn nwn' =
        abstractNTExpression joiner nwn expr $ \vwt' expr' -> do
            MkJoiner vwt'' conv1 conv2 <- joinWit vwt vwt'
            cont vwt'' $ fmap (\tta tt -> tta (conv2 tt) (conv1 tt)) expr'
abstractNTExpression joiner nwn (OpenExpression wt expr) cont =
    abstractNTExpression joiner nwn expr $ \wt' expr' ->
        cont wt' $ OpenExpression wt $ fmap (\vva v1 v2 -> vva v2 v1) expr'
