module Pinafore.Language.Interpret
    ( parseTopExpression
    , parseModule
    , parseType
    , parseNonpolarType
    , parseToValue
    , parseToValueUnify
    , parseToValueSubsume
    , interpretImportDeclaration
    , runPinaforeScoped
    , runInteract
    , showQValue
    )
where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Interpret.Expression
import Pinafore.Language.Interpret.Interact
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
    forall polarity.
    Is PolarityType polarity =>
    Text ->
    QInterpreter (Some (QType polarity))
parseType text = do
    st <- parseScopedReaderWhole (fmap return readType) text
    interpretType st

parseNonpolarType ::
    Text ->
    QInterpreter (Some QNonpolarType)
parseNonpolarType text = do
    st <- parseScopedReaderWhole (fmap return readType) text
    interpretNonpolarType st

parseToValue :: Text -> [(ImplicitName, QValue)] -> QInterpreter QValue
parseToValue text args = do
    expr <- parseTopExpression text
    let argExprs = fmap (fmap qConstValue) args
    expr' <- qImply argExprs expr
    qEvalExpr expr'

parseToValueUnify ::
    forall t.
    HasQType QPolyShim 'Negative t =>
    Text ->
    [(ImplicitName, QValue)] ->
    QInterpreter t
parseToValueUnify text args = do
    val <- parseToValue text args
    qUnifyValue val

parseToValueSubsume :: forall t. QType 'Positive t -> Text -> QInterpreter t
parseToValueSubsume t text = do
    val <- parseToValue text []
    qSubsumeValue t val

runPinaforeScoped :: (?library :: LibraryContext) => String -> QInterpreter a -> InterpretResult a
runPinaforeScoped sourcename ma =
    runInterpreter (initialPos sourcename) ?library $ do
        sd <- interpretImportDeclaration builtInModuleName
        withDeclarations sd ma
