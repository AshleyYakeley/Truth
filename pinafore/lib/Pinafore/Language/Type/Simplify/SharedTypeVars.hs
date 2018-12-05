module Pinafore.Language.Type.Simplify.SharedTypeVars
    ( mergeSharedTypeVarsInExpression
    ) where

import Pinafore.Language.Type.Type

--import Shapes
mergeSharedTypeVarsInExpression :: PinaforeExpression baseedit name -> PinaforeExpression baseedit name
mergeSharedTypeVarsInExpression expr = expr
