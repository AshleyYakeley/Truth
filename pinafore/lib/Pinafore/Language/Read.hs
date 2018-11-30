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
import Shapes hiding (try)
import Text.Parsec hiding ((<|>), many, optional)

parseTopExpression ::
       HasPinaforeEntityEdit baseedit => SourceName -> Text -> Result Text (PinaforeTypeCheck (QExpr baseedit))
parseTopExpression = parseReader readTopExpression
