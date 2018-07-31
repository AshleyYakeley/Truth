module Pinafore.Language.Expression where

import Language.Expression.Unitype
import Pinafore.Language.Convert
import Pinafore.Language.Name
import Pinafore.Language.Value
import Pinafore.PredicateMorphism
import Shapes

type QExpression baseedit = SealedUnitypeExpression Name (QValue baseedit)

qConstExpr :: ToQValue baseedit a => a -> QExpression baseedit
qConstExpr a = opoint $ toQValue a

qVarExpr :: Name -> QExpression baseedit
qVarExpr = varSealedUnitypeExpression

qAbstractExpr :: Name -> QExpression baseedit -> QExpression baseedit
qAbstractExpr = abstractSealedUnitypeExpression qfunction

qAbstractsExpr :: [Name] -> QExpression baseedit -> QExpression baseedit
qAbstractsExpr [] = id
qAbstractsExpr (n:nn) = qAbstractExpr n . qAbstractsExpr nn

type QBindings baseedit = UnitypeBindings Name (QValue baseedit)

qBindVal :: ToQValue baseedit t => Name -> t -> QBindings baseedit
qBindVal name val = bindExpression name $ opoint $ toQValue val

qApplyExpr :: HasPinaforeEntityEdit baseedit => QExpression baseedit -> QExpression baseedit -> QExpression baseedit
qApplyExpr = oliftA2 qapply

qApplyAllExpr ::
       HasPinaforeEntityEdit baseedit => QExpression baseedit -> [QExpression baseedit] -> QExpression baseedit
qApplyAllExpr e [] = e
qApplyAllExpr e (a:aa) = qApplyAllExpr (qApplyExpr e a) aa

qSequenceExpr :: [QExpression baseedit] -> QExpression baseedit
qSequenceExpr = osequenceA $ MkAny QTList
