module Language.Expression.TypeSystem.Simplify
    ( SimplifyTypeSystem (..)
    )
where

import Shapes

import Language.Expression.TypeSystem.TypeSystem

class TypeSystem ts => SimplifyTypeSystem ts where
    simplify ::
        forall a.
        TSMappable ts a =>
        EndoM (TSOuter ts) a
