module Language.Expression.Common.TypeSystem where

import Data.Shim
import Language.Expression.Common.Named
import Language.Expression.Common.Pattern
import Language.Expression.Common.Sealed
import Language.Expression.Common.SolverExpression
import Language.Expression.Common.WitnessMappable
import Shapes

class (Monad (TSOuter ts), Category (TSShim ts), Category (TSShim ts), Eq (TSVarID ts), Show (TSVarID ts)) =>
          TypeSystem (ts :: Type) where
    type TSOuter ts :: Type -> Type
    type TSNegWitness ts :: Type -> Type
    type TSPosWitness ts :: Type -> Type
    type TSShim ts :: Type -> Type -> Type
    type TSVarID ts :: Type

type TSWitness :: Type -> Polarity -> Type -> Type
type family TSWitness ts polarity where
    TSWitness ts 'Negative = TSNegWitness ts
    TSWitness ts 'Positive = TSPosWitness ts

type TSShimWit ts polarity = PolarShimWit (TSShim ts) (TSWitness ts polarity) polarity

type TSNegShimWit ts = TSShimWit ts 'Negative

type TSPosShimWit ts = TSShimWit ts 'Positive

type TSMappable ts = WitnessMappable (TSPosShimWit ts) (TSNegShimWit ts)

type TSOpenExpression ts = NamedExpression (TSVarID ts) (TSNegShimWit ts)

type TSSealedExpression ts = SealedExpression (TSVarID ts) (TSNegShimWit ts) (TSPosShimWit ts)

type TSOpenPattern ts = NamedPattern (TSVarID ts) (TSPosShimWit ts)

type TSSolverExpression ts typeexpr
     = SolverExpression (TSPosShimWit ts) (TSNegShimWit ts) typeexpr (TSOpenExpression ts)

type TSExpressionWitness ts = NamedExpressionWitness (TSVarID ts) (TSNegShimWit ts)

type TSSealedPattern ts = SealedPattern (TSVarID ts) (TSPosShimWit ts) (TSNegShimWit ts)

type TSPatternConstructor ts = PatternConstructor (TSVarID ts) (TSPosShimWit ts) (TSNegShimWit ts)

type TSSealedExpressionPattern ts = SealedExpressionPattern (TSVarID ts) (TSPosShimWit ts) (TSNegShimWit ts)

type TSExpressionPatternConstructor ts = ExpressionPatternConstructor (TSVarID ts) (TSPosShimWit ts) (TSNegShimWit ts)
