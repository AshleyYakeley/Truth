module Pinafore.Language.Grammar
    ( parseTopExpression
    , parseModule
    , parseType
    , interpretImportDeclaration
    ) where

import Import
import Pinafore.Language.Error
import Pinafore.Language.Grammar.Interpret
import Pinafore.Language.Interpreter
import Pinafore.Language.Type

parseScopedReaderWhole :: Parser (QInterpreter t) -> Text -> QInterpreter t
parseScopedReaderWhole parser text = do
    spos <- paramAsk sourcePosParam
    ma <- fromParseResult $ evalStateT (runParser parser text) spos
    ma

parseTopExpression :: Text -> QInterpreter QExpression
parseTopExpression = parseScopedReaderWhole $ fmap interpretExpression readExpression

parseModule :: Text -> QInterpreter QModule
parseModule =
    parseScopedReaderWhole $ do
        smod <- readModule
        return $ interpretModule smod

parseType ::
       forall polarity. Is PolarityType polarity
    => Text
    -> QInterpreter (Some (QType polarity))
parseType text = do
    st <- parseScopedReaderWhole (fmap return readType) text
    interpretType st
