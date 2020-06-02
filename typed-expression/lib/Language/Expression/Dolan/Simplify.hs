module Language.Expression.Dolan.Simplify
    ( dolanSimplifyTypes
    ) where

import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Simplify.DuplicateGroundTypes
import Language.Expression.Dolan.Simplify.DuplicateTypeVars
import Language.Expression.Dolan.Simplify.OneSidedTypeVars
import Language.Expression.Dolan.Simplify.SharedTypeVars
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

simplifyTypes :: Bool
simplifyTypes = True

-- Simplification:
-- 1. merge duplicate ground types in join/meet (on each type)
-- e.g. "[a]|[b]" => "[a|b]"
--
-- 2. eliminate one-sided type vars (on whole expression)
-- Type vars are one-sided if they appear only in positive position, or only in negative position.
-- e.g. "a -> [b]" => "Any -> [None]"
--
-- 3. merge shared type vars (on whole expression)
-- Two type variables are shared, if they always appear together (join/meet) in the positive positions, or in the negative positions.
-- e.g. "a -> b -> a|b" => "a -> a -> a|a"
--
-- 4. merge duplicate type vars in join/meet (on each type)
-- e.g. "a|a" => "a"
dolanSimplifyTypes ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanGroundType ground, PShimWitMappable (DolanPolyShim ground Type) (DolanType ground) a)
    => a
    -> a
dolanSimplifyTypes =
    if simplifyTypes
        then mergeDuplicateTypeVars @ground .
             mergeSharedTypeVars @ground . eliminateOneSidedTypeVars @ground . mergeDuplicateGroundTypes @ground
        else id
