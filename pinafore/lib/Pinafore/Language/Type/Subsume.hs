module Pinafore.Language.Type.Subsume
    ( subsumeExpression
    ) where

import Pinafore.Language.Type
import Shapes

subsumeExpression ::
       AnyW (PinaforeType baseedit 'PositivePolarity)
    -> PinaforeExpression baseedit
    -> PinaforeSourceScoped baseedit (PinaforeExpression baseedit)
subsumeExpression _ expr = return expr
