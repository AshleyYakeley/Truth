module Pinafore.Language.TypeSystem.Simplify
    ( pinaforeSimplifyTypes
    ) where

import Language.Expression.Dolan
import Pinafore.Language.TypeSystem.Simplify.DuplicateGroundTypes
import Pinafore.Language.TypeSystem.Simplify.DuplicateTypeVars
import Pinafore.Language.TypeSystem.Simplify.OneSidedTypeVars
import Pinafore.Language.TypeSystem.Simplify.SharedTypeVars
import Pinafore.Language.TypeSystem.Type
import Shapes

simplify :: Bool
simplify = True

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
pinaforeSimplifyTypes ::
       forall baseedit a. PShimWitMappable PinaforeShim (PinaforeType baseedit) a
    => a
    -> a
pinaforeSimplifyTypes =
    if simplify
        then mergeDuplicateTypeVars @baseedit .
             mergeSharedTypeVars @baseedit . eliminateOneSidedTypeVars @baseedit . mergeDuplicateGroundTypes @baseedit
        else id
