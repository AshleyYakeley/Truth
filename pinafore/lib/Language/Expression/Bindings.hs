module Language.Expression.Bindings
    ( Bindings
    , singleBinding
    , bindingsLetExpression
    ) where

import Language.Expression.Expression
import Shapes

newtype Bindings m vw =
    MkBindings [AnyF (Binder m vw) (Expression vw)]
    deriving (Semigroup, Monoid)

singleBinding :: Binder m vw t -> Expression vw t -> Bindings m vw
singleBinding binder expr = MkBindings $ pure $ MkAnyF binder expr

data Bound m vw =
    forall vals. MkBound (forall a. Expression vw a -> m (Expression vw (vals -> a)))
                         (Expression vw vals)

mkBound :: Monad m => [AnyF (Binder m vw) (Expression vw)] -> Bound m vw
mkBound [] = MkBound (\e -> return $ fmap (\a _ -> a) e) (pure ())
mkBound ((MkAnyF binder expr):bb) =
    case mkBound bb of
        MkBound abstractNames exprs ->
            MkBound
                (\e -> do
                     e' <- abstractNames e
                     e'' <- abstractExpression binder e'
                     return $ fmap (\tva ~(t, v) -> tva t v) e'')
                ((,) <$> expr <*> exprs)

bindingsLetExpression ::
       forall m vw a. Monad m
    => Bindings m vw
    -> Expression vw a
    -> m (Expression vw a)
bindingsLetExpression (MkBindings bb) body =
    case mkBound bb of
        MkBound abstractNames exprs -> do
            exprsf <- abstractNames exprs
            bodyf <- abstractNames body
            return $ bodyf <*> fmap fix exprsf
