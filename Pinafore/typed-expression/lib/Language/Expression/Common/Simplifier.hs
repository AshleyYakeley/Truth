module Language.Expression.Common.Simplifier
    ( SimplifyTypeSystem(..)
    ) where

import Language.Expression.Common.PolarTypeSystem
import Shapes

class PolarTypeSystem ts => SimplifyTypeSystem ts where
    simplify ::
           forall a. TSMappable ts a
        => EndoM (TSOuter ts) a
