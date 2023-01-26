module Language.Expression.Common.TypeSystem where

import Data.Shim
import Language.Expression.Common.Named
import Language.Expression.Common.Partial
import Language.Expression.Common.Pattern
import Language.Expression.Common.Sealed
import Language.Expression.Common.SolverExpression
import Language.Expression.Common.WitnessMappable
import Shapes

class (Monad (TSOuter ts), Category (TSShim ts), Eq (TSVarID ts), Show (TSVarID ts)) => TypeSystem (ts :: Type) where
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

tsMapWitnessesM ::
       forall ts m a. (TSMappable ts a, Applicative m)
    => EndoM' m (TSPosShimWit ts)
    -> EndoM' m (TSNegShimWit ts)
    -> EndoM m a
tsMapWitnessesM = mapWitnessesM

tsMapWitnesses ::
       forall ts a. TSMappable ts a
    => Endo' (TSPosShimWit ts)
    -> Endo' (TSNegShimWit ts)
    -> Endo a
tsMapWitnesses = mapWitnesses

type TSOpenExpression :: Type -> Type -> Type
type TSOpenExpression ts = NamedExpression (TSVarID ts) (TSNegShimWit ts)

type TSSealedPartialExpression ts = SealedPartialExpression (TSVarID ts) (TSNegShimWit ts) (TSPosShimWit ts)

type TSSealedExpression ts = SealedExpression (TSVarID ts) (TSNegShimWit ts) (TSPosShimWit ts)

type TSOpenSolverExpression ts typeexpr
     = SolverExpression (TSPosShimWit ts) (TSNegShimWit ts) typeexpr (TSOpenExpression ts)

type TSExpressionWitness ts = NamedExpressionWitness (TSVarID ts) (TSNegShimWit ts)
