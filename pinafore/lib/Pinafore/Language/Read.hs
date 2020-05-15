module Pinafore.Language.Read
    ( parseTopExpression
    , parseType
    , InteractiveCommand(..)
    , parseInteractiveCommand
    ) where

import Pinafore.Language.Expression
import Pinafore.Language.Interpret
import Pinafore.Language.Interpret.Type
import Pinafore.Language.Read.Expression
import Pinafore.Language.Read.Interactive
import Pinafore.Language.Read.Parser
import Pinafore.Language.Read.Type
import Pinafore.Language.TypeSystem
import Shapes hiding (try)

parseTopExpression :: Text -> PinaforeSourceScoped baseupdate (QExpr baseupdate)
parseTopExpression = parseScopedReaderWhole $ fmap interpretTopExpression readExpression

parseType ::
       forall baseupdate polarity. Is PolarityType polarity
    => Text
    -> PinaforeSourceScoped baseupdate (AnyW (PinaforeType baseupdate polarity))
parseType text = do
    st <- parseScopedReaderWhole (fmap return readType) text
    interpretType st
