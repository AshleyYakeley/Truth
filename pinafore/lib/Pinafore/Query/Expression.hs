module Pinafore.Query.Expression where

import Data.List (head, nub, tail)
import Pinafore.Query.Value
import Shapes

data QExpr a
    = ClosedQExpr a
    | OpenQExpr String
                (QExpr (QValue -> a))

instance Functor QExpr where
    fmap ab (ClosedQExpr a) = ClosedQExpr $ ab a
    fmap ab (OpenQExpr name expr) = OpenQExpr name $ fmap (\va v -> ab $ va v) expr

instance Applicative QExpr where
    pure = ClosedQExpr
    (ClosedQExpr ab) <*> expr = fmap ab expr
    (OpenQExpr name exprab) <*> expr = OpenQExpr name $ (\vab a v -> vab v a) <$> exprab <*> expr

qabstract :: String -> QExpr a -> QExpr (QValue -> a)
qabstract _name (ClosedQExpr a) = ClosedQExpr $ \_ -> a
qabstract name (OpenQExpr name' expr)
    | name == name' = fmap (\vva v -> vva v v) $ qabstract name expr
qabstract name (OpenQExpr name' expr) = OpenQExpr name' $ fmap (\vva v1 v2 -> vva v2 v1) $ qabstract name expr

qeval :: MonadFail m => QExpr a -> m a
qeval (ClosedQExpr a) = return a
qeval (OpenQExpr name _) = fail $ "undefined: " ++ name

type QValueExpr = QExpr QValue

exprAbstract :: String -> QValueExpr -> QValueExpr
exprAbstract name expr = fmap qfunction $ qabstract name expr

exprAbstracts :: [String] -> QValueExpr -> QValueExpr
exprAbstracts [] = id
exprAbstracts (n:nn) = exprAbstract n . exprAbstracts nn

qvar :: String -> QValueExpr
qvar name = OpenQExpr name $ ClosedQExpr id

qlet :: String -> QValueExpr -> QExpr a -> QExpr a
qlet name val body = qabstract name body <*> val

newtype QBindings =
    MkQBindings [(String, QValueExpr)]
    deriving (Semigroup, Monoid)

duplicates :: Eq a => [a] -> [a]
duplicates [] = []
duplicates (a:aa)
    | elem a aa = a : duplicates aa
duplicates (_:aa) = duplicates aa

getDuplicates :: QBindings -> [String]
getDuplicates (MkQBindings bb) = nub $ duplicates $ fmap fst bb

qbind :: ToQValue t => String -> t -> QBindings
qbind name val = MkQBindings [(name, pure $ toQValue val)]

qlets :: QBindings -> QExpr a -> QExpr a
qlets (MkQBindings bb) body = let
    appCons vva vv = vva (head vv) (tail vv)
    abstractList :: [String] -> QExpr a -> QExpr ([QValue] -> a)
    abstractList [] expr = fmap (\a _ -> a) expr
    abstractList (n:nn) expr = fmap appCons $ qabstract n $ abstractList nn expr
    abstractNames :: QExpr a -> QExpr ([QValue] -> a)
    abstractNames = abstractList (fmap fst bb)
    exprs :: QExpr [QValue]
    exprs = fmap fix $ abstractNames $ for bb $ \(_, b) -> b
    in abstractNames body <*> exprs

exprApply :: QValueExpr -> QValueExpr -> QValueExpr
exprApply = liftA2 qapply

exprApplyAll :: QValueExpr -> [QValueExpr] -> QValueExpr
exprApplyAll e [] = e
exprApplyAll e (a:aa) = exprApplyAll (exprApply e a) aa
