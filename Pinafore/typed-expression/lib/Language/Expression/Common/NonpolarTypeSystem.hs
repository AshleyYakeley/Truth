module Language.Expression.Common.NonpolarTypeSystem where

import Data.Shim
import Language.Expression.Common.TypeSystem
import Shapes

class TypeSystem ts => NonpolarTypeSystem (ts :: Type) where
    type TSNonpolarWitness ts :: Type -> Type
    nonpolarToPositive :: forall t. TSNonpolarWitness ts t -> TSPosShimWit ts t
    nonpolarToNegative :: forall t. TSNonpolarWitness ts t -> TSNegShimWit ts t
    positiveToNonpolar :: forall t. TSPosWitness ts t -> Maybe (TSNonpolarShimWit ts 'Positive t)
    negativeToNonpolar :: forall t. TSNegWitness ts t -> Maybe (TSNonpolarShimWit ts 'Negative t)

type TSNonpolarShimWit ts polarity = PolarShimWit (TSShim ts) (TSNonpolarWitness ts) polarity
