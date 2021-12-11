module Pinafore.Language.Grammar.Read
    ( parseTopExpression
    , parseModule
    , parseType
    , InteractiveCommand(..)
    , parseInteractiveCommand
    , initialPos
    ) where

import Pinafore.Language.Expression
import Pinafore.Language.Grammar.Interpret
import Pinafore.Language.Grammar.Read.Expression
import Pinafore.Language.Grammar.Read.Interactive
import Pinafore.Language.Grammar.Read.Parser
import Pinafore.Language.Grammar.Read.Type
import Pinafore.Language.Name
import Pinafore.Language.Type
import Shapes hiding (try)

parseTopExpression :: Text -> PinaforeInterpreter QExpr
parseTopExpression = parseScopedReaderWhole $ fmap interpretTopExpression readExpression

parseModule :: ModuleName -> Text -> PinaforeInterpreter PinaforeModule
parseModule modname =
    parseScopedReaderWhole $ do
        smod <- readModule
        return $ interpretModule modname smod

parseType ::
       forall polarity. Is PolarityType polarity
    => Text
    -> PinaforeInterpreter (AnyW (PinaforeType polarity))
parseType text = do
    st <- parseScopedReaderWhole (fmap return readType) text
    interpretType st
