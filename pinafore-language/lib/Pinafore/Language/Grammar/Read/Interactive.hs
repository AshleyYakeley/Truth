module Pinafore.Language.Grammar.Read.Interactive
    ( InteractiveCommand(..)
    , parseInteractiveCommand
    ) where

import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Grammar.Interpret
import Pinafore.Language.Grammar.Read.Expression
import Pinafore.Language.Grammar.Read.Parser
import Pinafore.Language.Grammar.Read.Token
import Pinafore.Language.Grammar.Read.Type
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Shapes hiding (try)

data InteractiveCommand
    = LetInteractiveCommand (MFunction PinaforeInterpreter PinaforeInterpreter)
    | ExpressionInteractiveCommand (PinaforeInterpreter QExpr)
    | ShowTypeInteractiveCommand Bool
                                 (PinaforeInterpreter QExpr)
    | forall polarity. SimplifyTypeInteractiveCommand (PolarityType polarity)
                                                      (PinaforeInterpreter (AnyW (PinaforeType polarity)))
    | ErrorInteractiveCommand Text

showTypeInteractiveCommand :: Bool -> Parser InteractiveCommand
showTypeInteractiveCommand showinfo = do
    expr <- readExpression
    return $ ShowTypeInteractiveCommand showinfo $ interpretTopExpression expr

simplifyPolarTypeInteractiveCommand :: forall polarity. PolarityType polarity -> Parser InteractiveCommand
simplifyPolarTypeInteractiveCommand polarity =
    case getRepWitness polarity of
        Dict -> do
            spos <- getPosition
            stype <- readType
            return $ SimplifyTypeInteractiveCommand polarity $ runSourcePos spos $ interpretType stype

readPolarity :: (forall polarity. PolarityType polarity -> Parser r) -> Parser r
readPolarity cont =
    (readExactlyThis TokOperator "+" >> cont PositiveType) <|> (readExactlyThis TokOperator "-" >> cont NegativeType)

simplifyTypeInteractiveCommand :: Parser InteractiveCommand
simplifyTypeInteractiveCommand = readPolarity simplifyPolarTypeInteractiveCommand

readSpecialCommand :: Text -> Parser InteractiveCommand
readSpecialCommand "t" = showTypeInteractiveCommand False
readSpecialCommand "type" = showTypeInteractiveCommand False
readSpecialCommand "info" = showTypeInteractiveCommand True
readSpecialCommand "simplify" = simplifyTypeInteractiveCommand
readSpecialCommand "simplify-" = simplifyPolarTypeInteractiveCommand NegativeType
readSpecialCommand cmd = return $ ErrorInteractiveCommand $ "unknown interactive command: " <> cmd

readInteractiveCommand :: Parser InteractiveCommand
readInteractiveCommand =
    (do
         readThis TokTypeJudge
         MkName cmd <- readThis TokLName
         readSpecialCommand cmd) <|>
    (readEnd >> return (LetInteractiveCommand id)) <|>
    (try $ do
         sexpr <- readExpression
         return $ ExpressionInteractiveCommand $ interpretTopExpression sexpr) <|>
    (do
         stdecls <- readTopDeclarations
         return $ LetInteractiveCommand $ interpretTopDeclarations stdecls)

parseInteractiveCommand :: Text -> StateT SourcePos InterpretResult InteractiveCommand
parseInteractiveCommand = parseReader readInteractiveCommand
