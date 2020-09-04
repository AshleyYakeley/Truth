module Pinafore.Language.Read.Interactive
    ( InteractiveCommand(..)
    , parseInteractiveCommand
    ) where

import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Interpret
import Pinafore.Language.Interpret.Type
import Pinafore.Language.Name
import Pinafore.Language.Read.Expression
import Pinafore.Language.Read.Parser
import Pinafore.Language.Read.Token
import Pinafore.Language.Read.Type
import Pinafore.Language.Scope
import Pinafore.Language.Type
import Shapes hiding (try)

data InteractiveCommand
    = LetInteractiveCommand (PinaforeScoped (WMFunction PinaforeScoped PinaforeScoped))
    | ExpressionInteractiveCommand (PinaforeScoped QExpr)
    | ShowTypeInteractiveCommand Bool
                                 (PinaforeScoped QExpr)
    | forall polarity. SimplifyTypeInteractiveCommand (PolarityType polarity)
                                                      (PinaforeScoped (AnyW (PinaforeType polarity)))
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
    (readEnd >> return (LetInteractiveCommand $ return id)) <|>
    (try $ do
         sexpr <- readExpression
         return $ ExpressionInteractiveCommand $ interpretTopExpression sexpr) <|>
    (do
         stdecls <- readTopDeclarations
         return $ LetInteractiveCommand $ interpretTopDeclarations stdecls)

parseInteractiveCommand :: Text -> StateT SourcePos InterpretResult InteractiveCommand
parseInteractiveCommand = parseReader readInteractiveCommand
