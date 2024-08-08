module Language.Expression.TypeSystem.Simplify
    ( SimplifyTypeSystem(..)
    ) where

import Language.Expression.TypeSystem.TypeSystem
import Shapes

class TypeSystem ts => SimplifyTypeSystem ts where
    simplify ::
           forall a. TSMappable ts a
        => EndoM (TSOuter ts) a
