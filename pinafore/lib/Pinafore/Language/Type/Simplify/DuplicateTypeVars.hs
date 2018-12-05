module Pinafore.Language.Type.Simplify.DuplicateTypeVars
    ( mergeDuplicateTypeVarsInTypes
    ) where

import Pinafore.Language.Type.Type

--import Shapes
mergeDuplicateTypeVarsInTypes :: PinaforeExpression baseedit name -> PinaforeExpression baseedit name
mergeDuplicateTypeVarsInTypes expr = expr
