module Pinafore.Language.Type.Subsume
    ( subsumeExpression
    ) where

import Pinafore.Language.Type
import Pinafore.Language.Type.Simplify
import Shapes

-- the user's declared type WILL be simplified first
subsumeExpression ::
       AnyW (PinaforeType baseedit 'PositivePolarity)
    -> PinaforeExpression baseedit
    -> PinaforeSourceScoped baseedit (PinaforeExpression baseedit)
subsumeExpression (MkAnyW rawdecltype) expr =
    case pinaforeSimplifyType rawdecltype of
        MkTypeF _decltype _ -> return expr
