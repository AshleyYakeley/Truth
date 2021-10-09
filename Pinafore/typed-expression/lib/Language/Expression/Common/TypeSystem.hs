module Language.Expression.Common.TypeSystem where

import Data.Shim
import Language.Expression.Common.Named
import Language.Expression.Common.Sealed
import Language.Expression.Common.WitnessMappable
import Shapes

class (Monad (TSOuter ts), Category (TSShim ts), InCategory (TSShim ts), Eq (TSVarID ts), Show (TSVarID ts)) =>
          TypeSystem (ts :: Type) where
    type TSOuter ts :: Type -> Type
    type TSNegWitness ts :: Type -> Type
    type TSPosWitness ts :: Type -> Type
    type TSShim ts :: Type -> Type -> Type
    type TSVarID ts :: Type

type TSNegShimWit ts = PolarShimWit (TSShim ts) (TSNegWitness ts) 'Negative

type TSPosShimWit ts = PolarShimWit (TSShim ts) (TSPosWitness ts) 'Positive

type TSMappable ts = WitnessMappable (TSPosShimWit ts) (TSNegShimWit ts)

type TSOpenExpression ts = NamedExpression (TSVarID ts) (TSNegShimWit ts)

type TSSealedExpression ts = SealedExpression (TSVarID ts) (TSNegShimWit ts) (TSPosShimWit ts)

type TSOpenPattern ts = NamedPattern (TSVarID ts) (TSPosShimWit ts)

type TSSealedPattern ts = SealedPattern (TSVarID ts) (TSPosShimWit ts) (TSNegShimWit ts)

type TSPatternConstructor ts = PatternConstructor (TSVarID ts) (TSPosShimWit ts) (TSNegShimWit ts)
