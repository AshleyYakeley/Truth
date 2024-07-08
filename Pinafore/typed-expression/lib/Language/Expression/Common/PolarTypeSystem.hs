module Language.Expression.Common.PolarTypeSystem where

import Data.Shim
import Language.Expression.Common.ExpressionTypeSystem
import Language.Expression.Common.Named
import Language.Expression.Common.Pattern
import Language.Expression.Common.Pattern.TypeSystem
import Language.Expression.Common.WitnessMappable
import Shapes

type ShowPolarTypeSystem ts
     = (Show (TSVarID ts), AllConstraint Show (TSNegWitness ts), AllConstraint Show (TSPosWitness ts))

class ( ExpressionTypeSystem ts
      , PatternTypeSystem ts
      , TSExprType ts ~ TSPosShimWit ts
      , TSExprVar ts ~ NameWitness (TSVarID ts) (TSNegShimWit ts)
      , TSPatType ts ~ TSNegShimWit ts
      , TSPatVar ts ~ NameWitness (TSVarID ts) (TSPosShimWit ts)
      , Monad (TSOuter ts)
      , Category (TSShim ts)
      , Eq (TSVarID ts)
      -- , ShowPolarTypeSystem ts
      ) => PolarTypeSystem (ts :: Type) where
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

type TSPatternConstructor ts = PatternConstructor (TSPatVar ts) (TSPosShimWit ts) (TSPatType ts)

type TSExpressionWitness ts = ExpressionWitness (TSPatType ts) (TSOpenExpression ts)

type TSExpressionPatternConstructor ts = PatternConstructor (TSPatVar ts) (TSPosShimWit ts) (TSExpressionWitness ts)

type TSMatch ts = SealedPattern (TSPatVar ts) (TSOpenExpression ts)

type TSSealedExpressionPattern ts = SealedPattern (TSPatVar ts) (TSExpressionWitness ts)

varSealedExpressionPattern ::
       forall ts t. PolarTypeSystem ts
    => TSVarID ts
    -> TSPatType ts t
    -> TSPosShimWit ts (MeetType t ())
    -> TSSealedExpressionPattern ts
varSealedExpressionPattern n twt vwt = varSealedPattern n (MkExpressionWitness twt $ pure ()) vwt

anySealedExpressionPattern :: forall ts t. TSPatType ts t -> TSSealedExpressionPattern ts
anySealedExpressionPattern twt = anySealedPattern $ MkExpressionWitness twt $ pure ()

toExpressionPatternConstructor :: forall ts. TSPatternConstructor ts -> TSExpressionPatternConstructor ts
toExpressionPatternConstructor (MkPatternConstructor twt lt pat) =
    MkPatternConstructor (MkExpressionWitness twt $ pure MkTopType) lt $ pat . arr meet1

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
