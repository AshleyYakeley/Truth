module Pinafore.Language.Expression where

import Language.Expression.Expression
import Pinafore.Language.Convert
import Pinafore.Language.Name
import Pinafore.Language.Value
import Pinafore.PredicateMorphism
import Shapes

type QExpr baseedit = Expression Name (QValue baseedit)

type QValueExpr baseedit = ValueExpression Name (QValue baseedit)

qAbstractExpr :: Name -> QValueExpr baseedit -> QValueExpr baseedit
qAbstractExpr name expr = fmap qfunction $ abstractExpression name expr

qAbstractsExpr :: [Name] -> QValueExpr baseedit -> QValueExpr baseedit
qAbstractsExpr [] = id
qAbstractsExpr (n:nn) = qAbstractExpr n . qAbstractsExpr nn

type QBindings baseedit = Bindings Name (QValue baseedit)

qBindVal :: ToQValue baseedit t => Name -> t -> QBindings baseedit
qBindVal name val = bindExpression name $ pure $ toQValue val

qApplyExpr :: HasPinaforeEntityEdit baseedit => QValueExpr baseedit -> QValueExpr baseedit -> QValueExpr baseedit
qApplyExpr = liftA2 qapply

qApplyAllExpr :: HasPinaforeEntityEdit baseedit => QValueExpr baseedit -> [QValueExpr baseedit] -> QValueExpr baseedit
qApplyAllExpr e [] = e
qApplyAllExpr e (a:aa) = qApplyAllExpr (qApplyExpr e a) aa
