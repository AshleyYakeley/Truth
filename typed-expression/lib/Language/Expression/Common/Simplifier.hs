module Language.Expression.Common.Simplifier
    ( TypePart(..)
    , SimplifyTypeSystem(..)
    ) where

import Language.Expression.Common.TypeSystem
import Shapes

data TypePart
    = TPWhole
    | TPPartial
    deriving (Eq)

class TypeSystem ts => SimplifyTypeSystem ts where
    simplifyPosType :: TypePart -> TSPosWitness ts t -> TSOuter ts (TSPosShimWit ts t)
    simplify ::
           forall a. TSMappable ts a
        => a
        -> TSOuter ts a
