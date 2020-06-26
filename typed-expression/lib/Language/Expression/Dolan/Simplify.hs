module Language.Expression.Dolan.Simplify
    ( dolanSimplifyTypes
    ) where

import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Simplify.DuplicateGroundTypes
import Language.Expression.Dolan.Simplify.DuplicateTypeVars
import Language.Expression.Dolan.Simplify.OneSidedTypeVars
import Language.Expression.Dolan.Simplify.RollUpRecursion
import Language.Expression.Dolan.Simplify.SharedTypeVars
import Language.Expression.Dolan.Simplify.UnusedRecursion
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

doMod :: Bool -> (a -> a) -> a -> a
doMod False _ = id
doMod True f = f

-- Simplification:
--
-- Some simplification happens by the way types are represented.
-- e.g. "(rec a. P) | Q" => "rec a. (P|Q)"
-- e.g. "rec a. rec b. (a,b)" => "rec a. (a,a)"
-- e.g. "(rec a. F a) | (rec a. G a)" => "rec a. (F a|G a)"
--
-- eliminateUnusedRecursion: remove unused recursion & eliminate immediate recursion
-- e.g. "rec a. Integer" => "Integer"
-- e.g. "rec a. a" => "Any"/"None"
--
-- mergeDuplicateGroundTypes: merge duplicate ground types in join/meet (on each type)
-- e.g. "[a]|[b]" => "[a|b]"
--
-- eliminateOneSidedTypeVars: eliminate one-sided type vars (on whole expression)
-- Type vars are one-sided if they appear only in positive position, or only in negative position.
-- e.g. "a -> [b]" => "Any -> [None]"
--
-- mergeSharedTypeVars: merge shared type vars (on whole expression)
-- Two type variables are shared, if they always appear together (join/meet) in the positive positions, or in the negative positions.
-- e.g. "a -> b -> a|b" => "a -> a -> a|a"
--
-- mergeDuplicateTypeVars: merge duplicate type vars in join/meet (on each type)
-- e.g. "a|a" => "a"
--
-- rollUpRecursiveTypes: roll up recursive types
-- e.g. "F (rec a. F a)" => "rec a. F a"
dolanSimplifyTypes ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanGroundType ground, PShimWitMappable (DolanPolyShim ground Type) (DolanType ground) a)
    => a
    -> a
dolanSimplifyTypes =
    doMod True $
    doMod True (rollUpRecursiveTypes @ground) .
    doMod True (mergeDuplicateTypeVars @ground) .
    doMod True (mergeSharedTypeVars @ground) .
    doMod True (eliminateOneSidedTypeVars @ground) .
    doMod True (mergeDuplicateGroundTypes @ground) . doMod True (eliminateUnusedRecursion @ground)
