module Pinafore.Language.Read
    ( parseTopExpression
    , parseType
    , InteractiveCommand(..)
    , parseInteractiveCommand
    ) where

import Pinafore.Base
import Pinafore.Language.Expression
import Pinafore.Language.Interpret
import Pinafore.Language.Interpret.Type
import Pinafore.Language.Read.Expression
import Pinafore.Language.Read.Interactive
import Pinafore.Language.Read.Parser
import Pinafore.Language.Read.Type
import Pinafore.Language.TypeSystem
import Shapes hiding (try)

parseTopExpression :: HasPinaforeEntityUpdate baseupdate => Text -> PinaforeSourceScoped baseupdate (QExpr baseupdate)
parseTopExpression = parseScopedReader $ fmap interpretTopExpression readTopExpression

parseType ::
       forall baseupdate polarity. Is PolarityType polarity
    => Text
    -> PinaforeSourceScoped baseupdate (AnyW (PinaforeType baseupdate polarity))
parseType text = do
    st <- parseScopedReader (fmap return readType) text
    interpretType st
