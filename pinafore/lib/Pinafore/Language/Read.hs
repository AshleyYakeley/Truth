module Pinafore.Language.Read
    ( parseTopExpression
    , parseModule
    , parseType
    , InteractiveCommand(..)
    , parseInteractiveCommand
    ) where

import Pinafore.Language.Expression
import Pinafore.Language.Interpret
import Pinafore.Language.Read.Expression
import Pinafore.Language.Read.Interactive
import Pinafore.Language.Read.Parser
import Pinafore.Language.Read.Type
import Pinafore.Language.Type
import Shapes hiding (try)

parseTopExpression :: Text -> PinaforeSourceInterpreter QExpr
parseTopExpression = parseScopedReaderWhole $ fmap interpretTopExpression readExpression

parseModule :: Text -> PinaforeSourceInterpreter PinaforeScope
parseModule = parseScopedReaderWhole $ fmap interpretModule readModule

parseType ::
       forall polarity. Is PolarityType polarity
    => Text
    -> PinaforeSourceInterpreter (AnyW (PinaforeType polarity))
parseType text = do
    st <- parseScopedReaderWhole (fmap return readType) text
    interpretType st
