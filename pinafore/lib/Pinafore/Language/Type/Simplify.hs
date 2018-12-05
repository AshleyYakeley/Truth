module Pinafore.Language.Type.Simplify
    ( pinaforeSimplifyExpressionType
    ) where

import Pinafore.Language.Type.Simplify.DuplicateGroundTypes
import Pinafore.Language.Type.Simplify.OneSidedTypeVars
import Pinafore.Language.Type.Type
import Shapes

-- Simplification:
-- 1. merge duplicate ground types in join/meet (on each type)
-- 2. eliminate one-sided type vars (on whole expression)
-- 3. merge shared type vars (on whole expression)
-- 4. merge duplicate type vars in join/meet (on each type)
-- 5. merge free (term) variables with subtypes
pinaforeSimplifyExpressionType :: PinaforeExpression baseedit name -> PinaforeExpression baseedit name
pinaforeSimplifyExpressionType =
    mergeFreeTermVars .
    mergeDuplicateTypeVarsExpression .
    mergeSharedTypeVars . eliminateOneSidedTypeVars . mergeDuplicateGroundTypesExpression

mergeSharedTypeVars :: PinaforeExpression baseedit name -> PinaforeExpression baseedit name
mergeSharedTypeVars expr = expr

mergeDuplicateTypeVarsExpression :: PinaforeExpression baseedit name -> PinaforeExpression baseedit name
mergeDuplicateTypeVarsExpression expr = expr

mergeFreeTermVars :: PinaforeExpression baseedit name -> PinaforeExpression baseedit name
mergeFreeTermVars expr = expr
