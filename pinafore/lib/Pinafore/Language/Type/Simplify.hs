module Pinafore.Language.Type.Simplify
    ( pinaforeSimplifyExpressionType
    ) where

import Pinafore.Language.Type.Simplify.DuplicateGroundTypes
import Pinafore.Language.Type.Simplify.DuplicateTypeVars
import Pinafore.Language.Type.Simplify.FreeVars
import Pinafore.Language.Type.Simplify.OneSidedTypeVars
import Pinafore.Language.Type.Simplify.SharedTypeVars
import Pinafore.Language.Type.Type
import Pinafore.Language.Type.Unify
import Shapes

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
--
-- 5. merge free (term) variables with subtypes
-- e.g. "{v::a,v::a} => b" => "{v::a} => b"
pinaforeSimplifyExpressionType ::
       forall baseedit name. Eq name
    => PinaforeExpression baseedit name
    -> PinaforeUnifierMonad baseedit (PinaforeExpression baseedit name)
pinaforeSimplifyExpressionType =
    mergeFreeExpressionTermVars .
    mergeDuplicateTypeVarsInTypes .
    mergeSharedTypeVarsInExpression . eliminateOneSidedTypeVarsInExpression . mergeDuplicateGroundTypesInTypes
