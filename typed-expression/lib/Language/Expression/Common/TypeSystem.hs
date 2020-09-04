module Language.Expression.Common.TypeSystem where

import Data.Shim
import Language.Expression.Common.Named
import Language.Expression.Common.Sealed
import Language.Expression.Common.WitnessMappable
import Shapes

class (Monad (TSOuter ts), Category (TSShim ts), InCategory (TSShim ts), Eq (TSName ts)) => TypeSystem (ts :: Type) where
    type TSOuter ts :: Type -> Type
    type TSNegWitness ts :: Type -> Type
    type TSPosWitness ts :: Type -> Type
    type TSShim ts :: Type -> Type -> Type
    type TSName ts :: Type

type TSNegShimWit ts = ShimWit (TSShim ts) (TSNegWitness ts) 'Negative

type TSPosShimWit ts = ShimWit (TSShim ts) (TSPosWitness ts) 'Positive

type TSMappable ts = WitnessMappable (TSPosShimWit ts) (TSNegShimWit ts)

type TSOpenExpression ts = NamedExpression (TSName ts) (TSNegShimWit ts)

type TSSealedExpression ts = SealedExpression (TSName ts) (TSNegShimWit ts) (TSPosShimWit ts)

type TSOpenPattern ts = NamedPattern (TSName ts) (TSPosShimWit ts)

type TSSealedPattern ts = SealedPattern (TSName ts) (TSPosShimWit ts) (TSNegShimWit ts)

type TSPatternConstructor ts = PatternConstructor (TSName ts) (TSPosShimWit ts) (TSNegShimWit ts)
