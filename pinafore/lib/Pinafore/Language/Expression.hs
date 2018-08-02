module Pinafore.Language.Expression where

import Language.Expression.Unitype
import Pinafore.Language.Convert
import Pinafore.Language.Name
import Pinafore.Language.Value
import Pinafore.PredicateMorphism
import Shapes

type QExpr baseedit = SealedUnitypeExpression Name (QValue baseedit)

qConstExpr :: ToQValue baseedit a => a -> QExpr baseedit
qConstExpr a = opoint $ toQValue a

qVarExpr :: Name -> QExpr baseedit
qVarExpr = varSealedUnitypeExpression

qAbstractExpr :: Name -> QExpr baseedit -> QExpr baseedit
qAbstractExpr = abstractSealedUnitypeExpression qfunction

qAbstractsExpr :: [Name] -> QExpr baseedit -> QExpr baseedit
qAbstractsExpr [] = id
qAbstractsExpr (n:nn) = qAbstractExpr n . qAbstractsExpr nn

type QBindings baseedit = UnitypeBindings Name (QValue baseedit)

qBindVal :: ToQValue baseedit t => Name -> t -> QBindings baseedit
qBindVal name val = bindExpression name $ opoint $ toQValue val

qApplyExpr :: HasPinaforeEntityEdit baseedit => QExpr baseedit -> QExpr baseedit -> QExpr baseedit
qApplyExpr = oliftA2 qapply

qApplyAllExpr :: HasPinaforeEntityEdit baseedit => QExpr baseedit -> [QExpr baseedit] -> QExpr baseedit
qApplyAllExpr e [] = e
qApplyAllExpr e (a:aa) = qApplyAllExpr (qApplyExpr e a) aa

qSequenceExpr :: [QExpr baseedit] -> QExpr baseedit
qSequenceExpr = osequenceA $ MkAny QTList

qBindExpr :: Name -> QExpr baseedit -> QBindings baseedit
qBindExpr = bindExpression

qLetExpr :: Name -> QExpr baseedit -> QExpr baseedit -> QExpr baseedit
qLetExpr = letSealedUnitypeExpression

qBindingsLetExpr :: MonadFail m => QBindings baseedit -> m (QExpr baseedit -> QExpr baseedit)
qBindingsLetExpr = bindingsLetUnitypeExpression

qUncheckedBindingsLetExpr :: QBindings baseedit -> QExpr baseedit -> QExpr baseedit
qUncheckedBindingsLetExpr = uncheckedBindingsLetUnitypeExpression

qEvalExpr :: MonadFail m => QExpr baseedit -> m (QValue baseedit)
qEvalExpr = evalSealedUnitypeExpression
