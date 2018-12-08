module Pinafore.Language.Read.Interactive
    ( InteractiveCommand(..)
    , parseInteractiveCommand
    ) where

import Pinafore.Base
import Pinafore.Language.Expression
import Pinafore.Language.Name
import Pinafore.Language.Read.Expression
import Pinafore.Language.Read.Parser
import Pinafore.Language.Read.Token
import Shapes hiding (try)
import Text.Parsec hiding ((<|>), many, optional)

data InteractiveCommand baseedit
    = LetInteractiveCommand (PinaforeScoped baseedit (QExpr baseedit) -> PinaforeScoped baseedit (QExpr baseedit))
    | ExpressionInteractiveCommand (PinaforeScoped baseedit (QExpr baseedit))
    | ShowTypeInteractiveCommand (PinaforeScoped baseedit (QExpr baseedit))
    | ErrorInteractiveCommand Text

showTypeInteractiveCommand ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (InteractiveCommand baseedit)
showTypeInteractiveCommand = do
    expr <- readTopExpression
    return $ ShowTypeInteractiveCommand expr

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
    (eof >> return (LetInteractiveCommand id)) <|>
    (try $ fmap ExpressionInteractiveCommand readTopExpression) <|>
    (fmap LetInteractiveCommand readTopLetBindings)

parseInteractiveCommand ::
       HasPinaforeEntityEdit baseedit => SourcePos -> Text -> Result Text (SourcePos, InteractiveCommand baseedit)
parseInteractiveCommand =
    parseReader $ do
        spos <- getPosition
        ic <- readInteractiveCommand
        return (spos, ic)
