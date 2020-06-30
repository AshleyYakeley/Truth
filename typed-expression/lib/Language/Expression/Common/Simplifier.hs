module Language.Expression.Common.Simplifier
    ( SimplifyTypeSystem(..)
    ) where

import Language.Expression.Common.TypeSystem

class TypeSystem ts => SimplifyTypeSystem ts where
    simplify ::
           forall a. TSMappable ts a
        => a
        -> TSOuter ts a
