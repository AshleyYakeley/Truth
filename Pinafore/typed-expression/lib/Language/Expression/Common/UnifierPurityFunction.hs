{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.UnifierPurityFunction
    ( UnifierPurityFunction(..)
    , unifierUnifierPurityFunction
    , openUnifierPurityFunction
    , matchUnifierPurityFunction
    , unifierPurityFunction
    , runUnifierPurityFunction
    ) where

import Language.Expression.Common.SolverExpression
import Language.Expression.Common.TypeSystem
import Language.Expression.Common.Unifier
import Shapes

data UnifierPurityFunction ts a b =
    forall f. MkUnifierPurityFunction (PurityType Maybe f)
                                      (UnifierExpression ts (Kleisli f a b))

instance UnifyTypeSystem ts => Functor (UnifierPurityFunction ts a) where
    fmap ab (MkUnifierPurityFunction purity expr) =
        MkUnifierPurityFunction purity $ purityIs @Monad purity $ fmap (fmap ab) expr

instance UnifyTypeSystem ts => Applicative (UnifierPurityFunction ts a) where
    pure b = arr $ \_ -> b
    liftA2 f (MkUnifierPurityFunction purityA exprA) (MkUnifierPurityFunction purityB exprB) =
        matchPurityType purityA purityB $ \purityAB cA cB ->
            MkUnifierPurityFunction purityAB $
            liftA2 (\fa fb -> liftA2 f (mapKleisli cA fa) (mapKleisli cB fb)) exprA exprB

matchUnifierPurityFunction ::
       UnifyTypeSystem ts
    => (forall f. MonadInner f => Kleisli f a1 b1 -> Kleisli f a2 b2 -> Kleisli f a12 b12)
    -> UnifierPurityFunction ts a1 b1
    -> UnifierPurityFunction ts a2 b2
    -> UnifierPurityFunction ts a12 b12
matchUnifierPurityFunction f (MkUnifierPurityFunction purity1 uk1) (MkUnifierPurityFunction purity2 uk2) =
    matchPurityType purity1 purity2 $ \purity12 c1 c2 ->
        purityIs @MonadInner purity12 $
        MkUnifierPurityFunction purity12 $ liftA2 (\k1 k2 -> f (mapKleisli c1 k1) (mapKleisli c2 k2)) uk1 uk2

mii :: Maybe a -> Identity a -> Identity a
mii (Just a) _ = Identity a
mii Nothing ia = ia

instance UnifyTypeSystem ts => Alternative (UnifierPurityFunction ts a) where
    empty = MkUnifierPurityFunction ImpureType $ pure empty
    MkUnifierPurityFunction PureType expr <|> _ = MkUnifierPurityFunction PureType expr
    MkUnifierPurityFunction ImpureType expr1 <|> MkUnifierPurityFunction PureType expr2 =
        MkUnifierPurityFunction PureType $ liftA2 (\(Kleisli f1) (Kleisli f2) -> Kleisli $ liftA2 mii f1 f2) expr1 expr2
    MkUnifierPurityFunction ImpureType expr1 <|> MkUnifierPurityFunction ImpureType expr2 =
        MkUnifierPurityFunction ImpureType $ liftA2 (<|>) expr1 expr2

instance UnifyTypeSystem ts => Category (UnifierPurityFunction ts) where
    id = MkUnifierPurityFunction PureType $ pure id
    (.) = matchUnifierPurityFunction (.)

instance UnifyTypeSystem ts => Arrow (UnifierPurityFunction ts) where
    arr f = MkUnifierPurityFunction PureType $ pure $ arr f
    first (MkUnifierPurityFunction purity expr) =
        MkUnifierPurityFunction purity $ purityIs @Monad purity $ fmap first expr
    second (MkUnifierPurityFunction purity expr) =
        MkUnifierPurityFunction purity $ purityIs @Monad purity $ fmap second expr

unifierUnifierPurityFunction :: UnifyTypeSystem ts => UnifierExpression ts b -> UnifierPurityFunction ts a b
unifierUnifierPurityFunction expr = MkUnifierPurityFunction PureType $ fmap pure expr

openUnifierPurityFunction :: UnifyTypeSystem ts => TSOpenExpression ts b -> UnifierPurityFunction ts a b
openUnifierPurityFunction expr = unifierUnifierPurityFunction $ solverExpressionLiftValue expr

unifierPurityFunction :: UnifyTypeSystem ts => PurityFunction Maybe a b -> UnifierPurityFunction ts a b
unifierPurityFunction (MkPurityFunction purity kab) = MkUnifierPurityFunction purity $ pure kab

runPurity :: PurityType Maybe f -> f a -> a
runPurity PureType (Identity a) = a
runPurity ImpureType (Just a) = a
runPurity ImpureType Nothing = error "missing case"

runUnifierPurityFunction :: UnifierPurityFunction ts a b -> UnifierExpression ts (a -> b)
runUnifierPurityFunction (MkUnifierPurityFunction purity uexpr) = fmap (\kab -> runPurity purity . runKleisli kab) uexpr
