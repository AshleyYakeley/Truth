module Pinafore.Query.Expression where

import Data.List (head, nub, tail)
import Pinafore.PredicateMorphism
import Pinafore.Query.Convert
import Pinafore.Query.Value
import Shapes

newtype Symbol =
    MkSymbol Text
    deriving (Eq)

instance Show Symbol where
    show (MkSymbol t) = unpack t

instance IsString Symbol where
    fromString s = MkSymbol $ fromString s

data QExpr baseedit a
    = ClosedQExpr a
    | OpenQExpr Symbol
                (QExpr baseedit (QValue baseedit -> a))

instance Functor (QExpr baseedit) where
    fmap ab (ClosedQExpr a) = ClosedQExpr $ ab a
    fmap ab (OpenQExpr name expr) = OpenQExpr name $ fmap (\va v -> ab $ va v) expr

instance Applicative (QExpr baseedit) where
    pure = ClosedQExpr
    (ClosedQExpr ab) <*> expr = fmap ab expr
    (OpenQExpr name exprab) <*> expr = OpenQExpr name $ (\vab a v -> vab v a) <$> exprab <*> expr

qabstract :: Symbol -> QExpr baseedit a -> QExpr baseedit (QValue baseedit -> a)
qabstract _name (ClosedQExpr a) = ClosedQExpr $ \_ -> a
qabstract name (OpenQExpr name' expr)
    | name == name' = fmap (\vva v -> vva v v) $ qabstract name expr
qabstract name (OpenQExpr name' expr) = OpenQExpr name' $ fmap (\vva v1 v2 -> vva v2 v1) $ qabstract name expr

qeval :: MonadFail m => QExpr baseedit a -> m a
qeval (ClosedQExpr a) = return a
qeval (OpenQExpr name _) = fail $ "undefined: " ++ show name

type QValueExpr baseedit = QExpr baseedit (QValue baseedit)

exprAbstract :: Symbol -> QValueExpr baseedit -> QValueExpr baseedit
exprAbstract name expr = fmap qfunction $ qabstract name expr

exprAbstracts :: [Symbol] -> QValueExpr baseedit -> QValueExpr baseedit
exprAbstracts [] = id
exprAbstracts (n:nn) = exprAbstract n . exprAbstracts nn

qvar :: Symbol -> QValueExpr baseedit
qvar name = OpenQExpr name $ ClosedQExpr id

qlet :: Symbol -> QValueExpr baseedit -> QExpr baseedit a -> QExpr baseedit a
qlet name val body = qabstract name body <*> val

newtype QBindings baseedit =
    MkQBindings [(Symbol, QValueExpr baseedit)]
    deriving (Semigroup, Monoid)

duplicates :: Eq a => [a] -> [a]
duplicates [] = []
duplicates (a:aa)
    | elem a aa = a : duplicates aa
duplicates (_:aa) = duplicates aa

getDuplicates :: QBindings baseedit -> [Symbol]
getDuplicates (MkQBindings bb) = nub $ duplicates $ fmap fst bb

qbind :: ToQValue baseedit t => Symbol -> t -> QBindings baseedit
qbind name val = MkQBindings [(name, pure $ toQValue val)]

qlets :: forall baseedit a. QBindings baseedit -> QExpr baseedit a -> QExpr baseedit a
qlets (MkQBindings bb) body = let
    appCons vva vv = vva (head vv) (tail vv)
    abstractList :: [Symbol] -> QExpr baseedit t -> QExpr baseedit ([QValue baseedit] -> t)
    abstractList [] expr = fmap (\a _ -> a) expr
    abstractList (n:nn) expr = fmap appCons $ qabstract n $ abstractList nn expr
    abstractNames :: QExpr baseedit t -> QExpr baseedit ([QValue baseedit] -> t)
    abstractNames = abstractList (fmap fst bb)
    exprs :: QExpr baseedit [QValue baseedit]
    exprs = fmap fix $ abstractNames $ for bb $ \(_, b) -> b
    in abstractNames body <*> exprs

exprApply :: HasPinaforePointEdit baseedit => QValueExpr baseedit -> QValueExpr baseedit -> QValueExpr baseedit
exprApply = liftA2 qapply

exprApplyAll :: HasPinaforePointEdit baseedit => QValueExpr baseedit -> [QValueExpr baseedit] -> QValueExpr baseedit
exprApplyAll e [] = e
exprApplyAll e (a:aa) = exprApplyAll (exprApply e a) aa
