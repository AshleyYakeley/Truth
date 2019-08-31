module Pinafore.Language.Read.Interactive
    ( InteractiveCommand(..)
    , parseInteractiveCommand
    ) where

import Data.Shim
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
import Pinafore.Language.TypeSystem
import Shapes hiding (try)

data InteractiveCommand baseupdate
    = LetInteractiveCommand (PinaforeScoped baseupdate (Transform (PinaforeScoped baseupdate) (PinaforeScoped baseupdate)))
    | ExpressionInteractiveCommand (PinaforeScoped baseupdate (QExpr baseupdate))
    | ShowTypeInteractiveCommand Bool
                                 (PinaforeScoped baseupdate (QExpr baseupdate))
    | forall polarity. SimplifyTypeInteractiveCommand (PolarityType polarity)
                                                      (PinaforeScoped baseupdate (AnyW (PinaforeType baseupdate polarity)))
    | ErrorInteractiveCommand Text

showTypeInteractiveCommand ::
       forall baseupdate. HasPinaforeEntityUpdate baseupdate
    => Bool
    -> Parser (InteractiveCommand baseupdate)
showTypeInteractiveCommand showinfo = do
    expr <- readTopExpression
    return $ ShowTypeInteractiveCommand showinfo $ interpretTopExpression expr

simplifyPolarTypeInteractiveCommand ::
       forall baseupdate polarity. HasPinaforeEntityUpdate baseupdate
    => PolarityType polarity
    -> Parser (InteractiveCommand baseupdate)
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
       forall baseupdate. HasPinaforeEntityUpdate baseupdate
    => Parser (InteractiveCommand baseupdate)
simplifyTypeInteractiveCommand = readPolarity simplifyPolarTypeInteractiveCommand

readSpecialCommand :: HasPinaforeEntityUpdate baseupdate => Text -> Parser (InteractiveCommand baseupdate)
readSpecialCommand "t" = showTypeInteractiveCommand False
readSpecialCommand "type" = showTypeInteractiveCommand False
readSpecialCommand "info" = showTypeInteractiveCommand True
readSpecialCommand "simplify" = simplifyTypeInteractiveCommand
readSpecialCommand "simplify-" = simplifyPolarTypeInteractiveCommand NegativeType
readSpecialCommand cmd = return $ ErrorInteractiveCommand $ "unknown interactive command: " <> cmd

readInteractiveCommand ::
       forall baseupdate. HasPinaforeEntityUpdate baseupdate
    => Parser (InteractiveCommand baseupdate)
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
       HasPinaforeEntityUpdate baseupdate => Text -> StateT SourcePos InterpretResult (InteractiveCommand baseupdate)
parseInteractiveCommand = parseReader readInteractiveCommand
