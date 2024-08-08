module Language.Expression.TypeSystem.TypeSystem where

import Data.Shim
import Language.Expression.Common.Open.Expression
import Language.Expression.Common.Open.Named
import Language.Expression.Common.Sealed.Partial
import Language.Expression.Common.Sealed.Sealed
import Language.Expression.Common.Sealed.SealedF
import Language.Expression.Common.WitnessMappable
import Language.Expression.TypeSystem.SolverExpression
import Shapes

type ShowTypeSystem ts = (Show (TSVarID ts), AllConstraint Show (TSNegWitness ts), AllConstraint Show (TSPosWitness ts))

class ( Monad (TSOuter ts)
      , Category (TSShim ts)
      , Eq (TSVarID ts)
      -- , ShowTypeSystem ts
      ) => TypeSystem (ts :: Type) where
    type TSOuter ts :: Type -> Type
    type TSNegWitness ts :: Type -> Type
    type TSPosWitness ts :: Type -> Type
    type TSShim ts :: ShimKind Type
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

type TSVarWit ts = NameWitness (TSVarID ts) (TSNegShimWit ts)

type TSOpenExpression :: Type -> Type -> Type
type TSOpenExpression ts = Expression (TSVarWit ts)

type TSSealedPartialExpression ts = SealedPartialExpression (TSVarWit ts) (TSPosShimWit ts)

type TSSealedExpression ts = SealedExpression (TSVarWit ts) (TSPosShimWit ts)

type TSSealedFExpression ts = SealedFExpression (TSVarWit ts) (TSPosShimWit ts)

type TSOpenSolverExpression ts typeexpr = SolverExpression typeexpr (TSOpenExpression ts)
