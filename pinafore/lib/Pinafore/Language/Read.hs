module Pinafore.Language.Read
    ( parseTopExpression
    , parseType
    , InteractiveCommand(..)
    , parseInteractiveCommand
    ) where

import Pinafore.Base
import Pinafore.Language.Expression
import Pinafore.Language.Read.Expression
import Pinafore.Language.Read.Interactive
import Pinafore.Language.Read.Parser
import Pinafore.Language.Read.Type
import Pinafore.Language.Type
import Shapes hiding (try)

parseTopExpression :: HasPinaforeEntityEdit baseedit => Text -> PinaforeSourceScoped baseedit (QExpr baseedit)
parseTopExpression = parseScopedReader readTopExpression
