module Language.Expression.Common.ExpressionTypeSystem where

import Language.Expression.Common.Expression
import Language.Expression.Common.Partial
import Language.Expression.Common.Sealed
import Language.Expression.Common.SealedF
import Language.Expression.Common.SolverExpression
import Shapes

class ExpressionTypeSystem (ts :: Type) where
    type TSVar ts :: Type -> Type
    type TSType ts :: Type -> Type

type TSOpenExpression :: Type -> Type -> Type
type TSOpenExpression ts = Expression (TSVar ts)

type TSSealedExpression ts = SealedExpression (TSVar ts) (TSType ts)

type TSSealedPartialExpression ts = SealedPartialExpression (TSVar ts) (TSType ts)

type TSSealedFExpression ts = SealedFExpression (TSVar ts) (TSType ts)

type TSOpenSolverExpression ts typeexpr = SolverExpression typeexpr (TSOpenExpression ts)
