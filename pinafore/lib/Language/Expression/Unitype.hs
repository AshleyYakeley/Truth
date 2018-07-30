module Language.Expression.Unitype
    ( UniNamedExpression
    , ValueExpression
    , evalExpression
    , abstractUniNamedExpression
    , varUniNamedExpression
    , Bindings
    , bindExpression
    , letUniNamedExpression
    , bindingsLetExpression
    , uncheckedBindingsLetExpression
    ) where

import Data.List (head, nub, tail)
import Language.Expression.Expression
import Language.Expression.Named
import Shapes

type UniNamedExpression name val = NamedExpression name ((:~:) val)

uniNamedMatch :: Eq name => name -> NamedWitness name ((:~:) t) t' -> Maybe (t -> t')
uniNamedMatch name (MkNamedWitness name' Refl) =
    if name == name'
        then Just id
        else Nothing

abstractUniNamedExpression :: Eq name => name -> UniNamedExpression name val a -> UniNamedExpression name val (val -> a)
abstractUniNamedExpression name = abstractExpression $ uniNamedMatch name

type ValueExpression name val = UniNamedExpression name val val

varUniNamedExpression :: name -> ValueExpression name val
varUniNamedExpression name = varExpression $ MkNamedWitness name Refl

letUniNamedExpression ::
       Eq name => name -> ValueExpression name val -> UniNamedExpression name val a -> UniNamedExpression name val a
letUniNamedExpression name = letExpression $ uniNamedMatch name

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
       forall name val. Eq name
    => Bindings name val
    -> ValueExpression name val
    -> ValueExpression name val
uncheckedBindingsLetExpression (MkBindings bb) body = let
    appCons vva vv = vva (head vv) (tail vv)
    abstractList :: [name] -> UniNamedExpression name val t -> UniNamedExpression name val ([val] -> t)
    abstractList [] expr = fmap (\a _ -> a) expr
    abstractList (n:nn) expr = fmap appCons $ abstractUniNamedExpression n $ abstractList nn expr
    abstractNames :: UniNamedExpression name val t -> UniNamedExpression name val ([val] -> t)
    abstractNames = abstractList (fmap fst bb)
    exprs :: UniNamedExpression name val [val]
    exprs = fmap fix $ abstractNames $ for bb $ \(_, b) -> b
    in abstractNames body <*> exprs

bindingsLetExpression ::
       (MonadFail m, Eq name, Show name)
    => Bindings name val
    -> m (ValueExpression name val -> ValueExpression name val)
bindingsLetExpression bindings =
    case bindingsDuplicates bindings of
        [] -> return $ uncheckedBindingsLetExpression bindings
        l -> fail $ "duplicate bindings: " ++ intercalate ", " (fmap show l)
