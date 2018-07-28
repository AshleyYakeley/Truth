module Language.Expression.Expression
    ( Expression
    , ValueExpression
    , evalExpression
    , abstractExpression
    , varExpression
    , Bindings
    , bindExpression
    , letExpression
    , bindingsLetExpression
    , uncheckedBindingsLetExpression
    ) where

import Data.List (head, nub, tail)
import Shapes

data Expression name val a
    = ClosedExpression a
    | OpenExpression name
                     (Expression name val (val -> a))

instance Functor (Expression name val) where
    fmap ab (ClosedExpression a) = ClosedExpression $ ab a
    fmap ab (OpenExpression name expr) = OpenExpression name $ fmap (\va v -> ab $ va v) expr

instance Applicative (Expression name val) where
    pure = ClosedExpression
    (ClosedExpression ab) <*> expr = fmap ab expr
    (OpenExpression name exprab) <*> expr = OpenExpression name $ (\vab a v -> vab v a) <$> exprab <*> expr

abstractExpression :: Eq name => name -> Expression name val a -> Expression name val (val -> a)
abstractExpression _name (ClosedExpression a) = ClosedExpression $ \_ -> a
abstractExpression name (OpenExpression name' expr)
    | name == name' = fmap (\vva v -> vva v v) $ abstractExpression name expr
abstractExpression name (OpenExpression name' expr) =
    OpenExpression name' $ fmap (\vva v1 v2 -> vva v2 v1) $ abstractExpression name expr

evalExpression :: (MonadFail m, Show name) => Expression name val a -> m a
evalExpression (ClosedExpression a) = return a
evalExpression (OpenExpression name _) = fail $ "undefined: " ++ show name

type ValueExpression name val = Expression name val val

varExpression :: name -> ValueExpression name val
varExpression name = OpenExpression name $ ClosedExpression id

letExpression :: Eq name => name -> ValueExpression name val -> Expression name val a -> Expression name val a
letExpression name val body = abstractExpression name body <*> val

newtype Bindings name val =
    MkBindings [(name, ValueExpression name val)]
    deriving (Semigroup, Monoid)

bindExpression :: name -> ValueExpression name val -> Bindings name val
bindExpression name vexpr = MkBindings $ pure (name, vexpr)

duplicates :: Eq a => [a] -> [a]
duplicates [] = []
duplicates (a:aa)
    | elem a aa = a : duplicates aa
duplicates (_:aa) = duplicates aa

bindingsDuplicates :: Eq name => Bindings name val -> [name]
bindingsDuplicates (MkBindings bb) = nub $ duplicates $ fmap fst bb

uncheckedBindingsLetExpression ::
       forall name val a. Eq name
    => Bindings name val
    -> Expression name val a
    -> Expression name val a
uncheckedBindingsLetExpression (MkBindings bb) body = let
    appCons vva vv = vva (head vv) (tail vv)
    abstractList :: [name] -> Expression name val t -> Expression name val ([val] -> t)
    abstractList [] expr = fmap (\a _ -> a) expr
    abstractList (n:nn) expr = fmap appCons $ abstractExpression n $ abstractList nn expr
    abstractNames :: Expression name val t -> Expression name val ([val] -> t)
    abstractNames = abstractList (fmap fst bb)
    exprs :: Expression name val [val]
    exprs = fmap fix $ abstractNames $ for bb $ \(_, b) -> b
    in abstractNames body <*> exprs

bindingsLetExpression ::
       (MonadFail m, Eq name, Show name) => Bindings name val -> m (Expression name val a -> Expression name val a)
bindingsLetExpression bindings =
    case bindingsDuplicates bindings of
        [] -> return $ uncheckedBindingsLetExpression bindings
        l -> fail $ "duplicate bindings: " ++ intercalate ", " (fmap show l)
