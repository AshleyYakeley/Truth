{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Sealed where

import Language.Expression.Bindings
import Language.Expression.Expression
import Shapes

data SealedExpression vw tw =
    forall t. MkSealedExpression (tw t)
                                 (Expression vw t)

letSealedExpression ::
       Applicative m
    => (forall t. tw t -> Binder m vw t)
    -> SealedExpression vw tw
    -> SealedExpression vw tw
    -> m (SealedExpression vw tw)
letSealedExpression tobinder (MkSealedExpression twv valexpr) (MkSealedExpression twt bodyexpr) =
    fmap (MkSealedExpression twt) $ letExpression (tobinder twv) valexpr bodyexpr

applySealedExpression ::
       (forall r f a. tw f -> tw a -> (forall fa. tw fa -> (f -> a -> fa) -> r) -> m r)
    -> SealedExpression vw tw
    -> SealedExpression vw tw
    -> m (SealedExpression vw tw)
applySealedExpression appf (MkSealedExpression tf exprf) (MkSealedExpression ta expra) =
    appf tf ta $ \tfa fafa -> MkSealedExpression tfa $ liftA2 fafa exprf expra

constSealedExpression :: tw t -> t -> SealedExpression vw tw
constSealedExpression twt t = MkSealedExpression twt $ pure t

evalSealedExpression :: (MonadFail m, AllWitnessConstraint Show vw) => SealedExpression vw tw -> m (Any tw)
evalSealedExpression (MkSealedExpression twa expr) = do
    a <- evalExpression expr
    return $ MkAny twa a

bindingsLetSealedExpression :: Monad m => Bindings m vw -> SealedExpression vw tw -> m (SealedExpression vw tw)
bindingsLetSealedExpression bindings (MkSealedExpression twt body) =
    fmap (MkSealedExpression twt) $ bindingsLetExpression bindings body
