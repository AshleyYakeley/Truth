module Pinafore.Language.Expression where

import Data.List (head, nub, tail)
import Language.Expression.Expression
import Language.Expression.Name
import Language.Expression.Type.HindleyMilner
import Language.Expression.Type.UniType
import Language.Expression.Typed
import Pinafore.Language.Convert
import Pinafore.Language.Value
import Pinafore.PredicateMorphism
import Shapes

-- MonotypedSymbol
--
data MonotypedSymbol t a where
    MkMonotypedSymbol :: Name -> MonotypedSymbol t t

instance TestEquality (MonotypedSymbol t) where
    testEquality (MkMonotypedSymbol s1) (MkMonotypedSymbol s2)
        | s1 == s2 = Just Refl
    testEquality _ _ = Nothing

instance Show (MonotypedSymbol t a) where
    show (MkMonotypedSymbol s) = show s

instance AllWitnessConstraint Show (MonotypedSymbol t) where
    allWitnessConstraint = Dict

-- QExpr
--
type QExpr baseedit = Expression (MonotypedSymbol (QValue baseedit))

qabstract :: Name -> QExpr baseedit a -> QExpr baseedit (QValue baseedit -> a)
qabstract s = exprAbstract $ MkMonotypedSymbol s

qeval :: MonadFail m => QExpr baseedit a -> m a
qeval = exprEval

type QValueExpr baseedit = QExpr baseedit (QValue baseedit)

qExprAbstract :: Name -> QValueExpr baseedit -> QValueExpr baseedit
qExprAbstract name expr = fmap qfunction $ qabstract name expr

qExprAbstracts :: [Name] -> QValueExpr baseedit -> QValueExpr baseedit
qExprAbstracts [] = id
qExprAbstracts (n:nn) = qExprAbstract n . qExprAbstracts nn

qvar :: Name -> QValueExpr baseedit
qvar s = exprVar $ MkMonotypedSymbol s

qlet :: Name -> QValueExpr baseedit -> QExpr baseedit a -> QExpr baseedit a
qlet name val body = qabstract name body <*> val

newtype QBindings baseedit =
    MkQBindings [(Name, QValueExpr baseedit)]
    deriving (Semigroup, Monoid)

duplicates :: Eq a => [a] -> [a]
duplicates [] = []
duplicates (a:aa)
    | elem a aa = a : duplicates aa
duplicates (_:aa) = duplicates aa

getDuplicates :: QBindings baseedit -> [Name]
getDuplicates (MkQBindings bb) = nub $ duplicates $ fmap fst bb

qbind :: ToQValue baseedit t => Name -> t -> QBindings baseedit
qbind name val = MkQBindings [(name, pure $ toQValue val)]

qlets :: forall baseedit a. QBindings baseedit -> QExpr baseedit a -> QExpr baseedit a
qlets (MkQBindings bb) body = let
    appCons vva vv = vva (head vv) (tail vv)
    abstractList :: [Name] -> QExpr baseedit t -> QExpr baseedit ([QValue baseedit] -> t)
    abstractList [] expr = fmap (\a _ -> a) expr
    abstractList (n:nn) expr = fmap appCons $ qabstract n $ abstractList nn expr
    abstractNames :: QExpr baseedit t -> QExpr baseedit ([QValue baseedit] -> t)
    abstractNames = abstractList (fmap fst bb)
    exprs :: QExpr baseedit [QValue baseedit]
    exprs = fmap fix $ abstractNames $ for bb $ \(_, b) -> b
    in abstractNames body <*> exprs

qExprApply :: HasPinaforeEntityEdit baseedit => QValueExpr baseedit -> QValueExpr baseedit -> QValueExpr baseedit
qExprApply = liftA2 qapply

qExprApplyAll :: HasPinaforeEntityEdit baseedit => QValueExpr baseedit -> [QValueExpr baseedit] -> QValueExpr baseedit
qExprApplyAll e [] = e
qExprApplyAll e (a:aa) = qExprApplyAll (qExprApply e a) aa
