module Pinafore.Language.Read.Interactive
    ( InteractiveCommand(..)
    , parseInteractiveCommand
    ) where

import Pinafore.Base
import Pinafore.Language.Expression
import Pinafore.Language.Interpret
import Pinafore.Language.Name
import Pinafore.Language.Read.Expression
import Pinafore.Language.Read.Parser
import Pinafore.Language.Read.Token
import Shapes hiding (try)
import Text.Parsec hiding ((<|>), many, optional)

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

readInteractiveCommand ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (InteractiveCommand baseedit)
readInteractiveCommand =
    (do
         readExactlyThis TokOperator ":"
         MkName cmd <- readThis TokName
         case cmd of
             "t" -> showTypeInteractiveCommand
             "type" -> showTypeInteractiveCommand
             _ -> return $ ErrorInteractiveCommand $ "unknown interactive command: " <> cmd) <|>
    (eof >> return (LetInteractiveCommand $ return id)) <|>
    (try $ do
         sexpr <- readTopExpression
         return $ ExpressionInteractiveCommand $ interpretTopExpression sexpr) <|>
    (do
         stdecls <- readTopDeclarations
         return $ LetInteractiveCommand $ interpretTopDeclarations stdecls)

parseInteractiveCommand ::
       HasPinaforeEntityEdit baseedit => Text -> StateT SourcePos (Result Text) (InteractiveCommand baseedit)
parseInteractiveCommand = parseReader readInteractiveCommand
