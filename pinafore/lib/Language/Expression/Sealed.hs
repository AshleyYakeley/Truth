{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Sealed where

import Language.Expression.Expression
import Shapes

data SealedExpression vw tw =
    forall t. MkSealedExpression (tw t)
                                 (Expression vw t)

newtype Binder m vw tw =
    MkBinder (forall t v. tw t -> vw v -> Maybe (m (t -> v)))

binderLetExpression ::
       Applicative m => Binder m vw tw -> SealedExpression vw tw -> Expression vw a -> m (Expression vw a)
binderLetExpression (MkBinder binder) (MkSealedExpression twv val) = letExpression (binder twv) val

binderAbstractExpression :: Applicative m => tw t -> Binder m vw tw -> Expression vw a -> m (Expression vw (t -> a))
binderAbstractExpression twt (MkBinder binder) = abstractExpression $ binder twt

letSealedExpression ::
       Applicative m => Binder m vw tw -> SealedExpression vw tw -> SealedExpression vw tw -> m (SealedExpression vw tw)
letSealedExpression binder expr (MkSealedExpression twt body) =
    fmap (MkSealedExpression twt) $ binderLetExpression binder expr body

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

newtype Bindings m vw tw =
    MkBindings [(Binder m vw tw, SealedExpression vw tw)]
    deriving (Semigroup, Monoid)

data Bound m vw =
    forall vals. MkBound (forall a. Expression vw a -> m (Expression vw (vals -> a)))
                         (Expression vw vals)

mkBound :: Monad m => [(Binder m vw tw, SealedExpression vw tw)] -> Bound m vw
mkBound [] = MkBound (\e -> return $ fmap (\a _ -> a) e) (pure ())
mkBound ((binder, MkSealedExpression twt expr):bb) =
    case mkBound bb of
        MkBound abstractNames exprs ->
            MkBound
                (\e -> do
                     e' <- abstractNames e
                     e'' <- binderAbstractExpression twt binder e'
                     return $ fmap (\tva ~(t, v) -> tva t v) e'')
                ((,) <$> expr <*> exprs)

bindingsLetExpression ::
       forall m vw tw a. Monad m
    => Bindings m vw tw
    -> Expression vw a
    -> m (Expression vw a)
bindingsLetExpression (MkBindings bb) body =
    case mkBound bb of
        MkBound abstractNames exprs -> do
            exprsf <- abstractNames exprs
            bodyf <- abstractNames body
            return $ bodyf <*> fmap fix exprsf

bindingsLetSealedExpression :: Monad m => Bindings m vw tw -> SealedExpression vw tw -> m (SealedExpression vw tw)
bindingsLetSealedExpression bindings (MkSealedExpression twt body) =
    fmap (MkSealedExpression twt) $ bindingsLetExpression bindings body
