module Language.Expression.Common.ExpressionTypeSystem where

import Language.Expression.Common.Expression
import Language.Expression.Common.Partial
import Language.Expression.Common.Sealed
import Language.Expression.Common.SealedF
import Language.Expression.Common.SolverExpression
import Shapes

class ExpressionTypeSystem (ts :: Type) where
    type TSExprVar ts :: Type -> Type
    type TSExprType ts :: Type -> Type

type TSOpenExpression :: Type -> Type -> Type
type TSOpenExpression ts = Expression (TSExprVar ts)

type TSSealedExpression ts = SealedExpression (TSExprVar ts) (TSExprType ts)

type TSSealedPartialExpression ts = SealedPartialExpression (TSExprVar ts) (TSExprType ts)

type TSSealedFExpression ts = SealedFExpression (TSExprVar ts) (TSExprType ts)

type TSOpenSolverExpression ts typeexpr = SolverExpression typeexpr (TSOpenExpression ts)
