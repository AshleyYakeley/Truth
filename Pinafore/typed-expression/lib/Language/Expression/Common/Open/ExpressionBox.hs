module Language.Expression.Common.Open.ExpressionBox
    ( ExpressionBox
    , singleExpressionBox
    , runExpressionBox
    )
where

import Shapes

import Language.Expression.Common.Open.Abstract
import Language.Expression.Common.Open.Error
import Language.Expression.Common.Open.Lifted

mapSomeFor :: (a -> b) -> SomeFor ((->) b) w -> SomeFor ((->) a) w
mapSomeFor ab (MkSomeFor wt ft) = MkSomeFor wt $ ft . ab

data ExpressionBox f w m
    = forall a. MkExpressionBox
        [SomeFor ((->) a) w]
        (LiftedExpression f w a)
        (f a -> m)

instance (Applicative f, Semigroup m) => Semigroup (ExpressionBox f w m) where
    MkExpressionBox wa expra posta <> MkExpressionBox wb exprb postb =
        MkExpressionBox
            (fmap (mapSomeFor fst) wa <> fmap (mapSomeFor snd) wb)
            (liftA2 (,) expra exprb)
            (\fab -> posta (fmap fst fab) <> postb (fmap snd fab))

instance (Applicative f, Monoid m) => Monoid (ExpressionBox f w m) where
    mempty = MkExpressionBox mempty (pure ()) (\_ -> mempty)

singleExpressionBox :: w t -> (a -> t) -> LiftedExpression f w a -> (f a -> m) -> ExpressionBox f w m
singleExpressionBox wt at expr fam = MkExpressionBox [MkSomeFor wt at] expr fam

runExpressionBox :: (Functor f, TestEquality w) => ExpressionBox f w m -> Result (ExpressionError w) m
runExpressionBox (MkExpressionBox ww expr fam) = do
    fa <- evalLiftedExpressionResult $ fmap fix $ abstractWitnesses ww expr
    return $ fam fa
