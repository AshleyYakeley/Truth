module Language.Expression.Common.Simplifier
    ( SimplifyTypeSystem(..)
    ) where

import Language.Expression.Common.TypeSystem
import Shapes

class TypeSystem ts => SimplifyTypeSystem ts where
    simplify ::
           forall a. TSMappable ts a
        => EndoM (TSOuter ts) a
