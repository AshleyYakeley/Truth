module Pinafore.Language.Read.Interactive
    ( InteractiveCommand(..)
    , parseInteractiveCommand
    ) where

import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Interpret
import Pinafore.Language.Name
import Pinafore.Language.Read.Expression
import Pinafore.Language.Read.Parser
import Pinafore.Language.Read.Token
import Shapes hiding (try)

data InteractiveCommand baseedit
    = LetInteractiveCommand (PinaforeScoped baseedit (Transform (PinaforeScoped baseedit) (PinaforeScoped baseedit)))
    | ExpressionInteractiveCommand (PinaforeScoped baseedit (QExpr baseedit))
    | ShowTypeInteractiveCommand (PinaforeScoped baseedit (QExpr baseedit))
    | ErrorInteractiveCommand Text

showTypeInteractiveCommand ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (InteractiveCommand baseedit)
showTypeInteractiveCommand = do
    expr <- readTopExpression
    return $ ShowTypeInteractiveCommand $ interpretTopExpression expr

readSpecialCommand :: HasPinaforeEntityEdit baseedit => Text -> Parser (InteractiveCommand baseedit)
readSpecialCommand "t" = showTypeInteractiveCommand
readSpecialCommand "type" = showTypeInteractiveCommand
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
