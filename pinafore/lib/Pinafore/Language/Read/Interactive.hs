module Pinafore.Language.Read.Interactive
    ( InteractiveCommand(..)
    , parseInteractiveCommand
    ) where

import Pinafore.Base
import Pinafore.Language.Expression
import Pinafore.Language.Name
import Pinafore.Language.Read.Expression
import Pinafore.Language.Read.Parser
import Pinafore.Language.Token
import Shapes hiding (try)
import Text.Parsec hiding ((<|>), many, optional)

data InteractiveCommand baseedit
    = LetInteractiveCommand (QExpr baseedit -> PinaforeTypeCheck (QExpr baseedit))
    | ExpressionInteractiveCommand (PinaforeTypeCheck (QExpr baseedit))
    | ShowTypeInteractiveCommand (PinaforeTypeCheck (QExpr baseedit))
    | ErrorInteractiveCommand Text

showTypeInteractiveCommand ::
       forall baseedit. HasPinaforePointEdit baseedit
    => Parser (InteractiveCommand baseedit)
showTypeInteractiveCommand = do
    expr <- readTopExpression
    return $ ShowTypeInteractiveCommand expr

readInteractiveCommand ::
       forall baseedit. HasPinaforePointEdit baseedit
    => Parser (InteractiveCommand baseedit)
readInteractiveCommand =
    (do
         readExactlyThis TokOperator ":"
         MkName cmd <- readThis TokName
         case cmd of
             "t" -> showTypeInteractiveCommand
             "type" -> showTypeInteractiveCommand
             _ -> return $ ErrorInteractiveCommand $ "unknown interactive command: " <> cmd) <|>
    (eof >> return (LetInteractiveCommand return)) <|>
    (try $ fmap ExpressionInteractiveCommand readTopExpression) <|>
    (fmap LetInteractiveCommand readTopLetBindings)

parseInteractiveCommand ::
       HasPinaforePointEdit baseedit => SourceName -> Text -> Result Text (InteractiveCommand baseedit)
parseInteractiveCommand = parseReader readInteractiveCommand
