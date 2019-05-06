module Pinafore.Language.Read.Interactive
    ( InteractiveCommand(..)
    , parseInteractiveCommand
    ) where

import Language.Expression.Polarity
import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Interpret
import Pinafore.Language.Interpret.Type
import Pinafore.Language.Name
import Pinafore.Language.Read.Expression
import Pinafore.Language.Read.Parser
import Pinafore.Language.Read.Token
import Pinafore.Language.Read.Type
import Pinafore.Language.Type
import Shapes hiding (try)

data InteractiveCommand baseedit
    = LetInteractiveCommand (PinaforeScoped baseedit (Transform (PinaforeScoped baseedit) (PinaforeScoped baseedit)))
    | ExpressionInteractiveCommand (PinaforeScoped baseedit (QExpr baseedit))
    | ShowTypeInteractiveCommand (PinaforeScoped baseedit (QExpr baseedit))
    | forall polarity. SimplifyTypeInteractiveCommand (PolarityType polarity)
                                                      (PinaforeScoped baseedit (AnyW (PinaforeType baseedit polarity)))
    | ErrorInteractiveCommand Text

showTypeInteractiveCommand ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (InteractiveCommand baseedit)
showTypeInteractiveCommand = do
    expr <- readTopExpression
    return $ ShowTypeInteractiveCommand $ interpretTopExpression expr

simplifyPolarTypeInteractiveCommand ::
       forall baseedit polarity. HasPinaforeEntityEdit baseedit
    => PolarityType polarity
    -> Parser (InteractiveCommand baseedit)
simplifyPolarTypeInteractiveCommand polarity =
    case getRepWitness polarity of
        Dict -> do
            spos <- getPosition
            stype <- readType
            return $ SimplifyTypeInteractiveCommand polarity $ runSourcePos spos $ interpretType stype

readPolarity :: (forall polarity. PolarityType polarity -> Parser r) -> Parser r
readPolarity cont =
    (readExactlyThis TokOperator "+" >> cont PositiveType) <|> (readExactlyThis TokOperator "-" >> cont NegativeType)

simplifyTypeInteractiveCommand ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (InteractiveCommand baseedit)
simplifyTypeInteractiveCommand = readPolarity simplifyPolarTypeInteractiveCommand

readSpecialCommand :: HasPinaforeEntityEdit baseedit => Text -> Parser (InteractiveCommand baseedit)
readSpecialCommand "t" = showTypeInteractiveCommand
readSpecialCommand "type" = showTypeInteractiveCommand
readSpecialCommand "simplify" = simplifyTypeInteractiveCommand
readSpecialCommand "simplify-" = simplifyPolarTypeInteractiveCommand NegativeType
readSpecialCommand cmd = return $ ErrorInteractiveCommand $ "unknown interactive command: " <> cmd

readInteractiveCommand ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (InteractiveCommand baseedit)
readInteractiveCommand =
    (do
         readExactlyThis TokOperator ":"
         MkName cmd <- readThis TokLName
         readSpecialCommand cmd) <|>
    (parseEnd >> return (LetInteractiveCommand $ return id)) <|>
    (try $ do
         sexpr <- readTopExpression
         return $ ExpressionInteractiveCommand $ interpretTopExpression sexpr) <|>
    (do
         stdecls <- readTopDeclarations
         return $ LetInteractiveCommand $ interpretTopDeclarations stdecls)

parseInteractiveCommand ::
       HasPinaforeEntityEdit baseedit => Text -> StateT SourcePos InterpretResult (InteractiveCommand baseedit)
parseInteractiveCommand = parseReader readInteractiveCommand
