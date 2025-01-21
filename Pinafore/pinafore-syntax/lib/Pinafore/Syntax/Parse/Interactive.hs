module Pinafore.Syntax.Parse.Interactive
    ( InteractiveCommand (..)
    , parseInteractiveCommand
    )
where

import Data.Shim
import Shapes hiding (try)

import Pinafore.Syntax.Name
import Pinafore.Syntax.Parse.Basic
import Pinafore.Syntax.Parse.Error
import Pinafore.Syntax.Parse.Expression
import Pinafore.Syntax.Parse.Parser
import Pinafore.Syntax.Parse.Token
import Pinafore.Syntax.Parse.Type
import Pinafore.Syntax.Syntax

data InteractiveCommand
    = NullInteractiveCommand
    | LetInteractiveCommand SyntaxDeclaration
    | ExpressionInteractiveCommand SyntaxExpression
    | BindActionInteractiveCommand
        SyntaxPattern
        SyntaxExpression
    | ShowDocInteractiveCommand FullNameRef
    | ShowTypeInteractiveCommand
        Bool
        SyntaxExpression
    | SimplifyTypeInteractiveCommand
        Polarity
        SyntaxType
    | ErrorInteractiveCommand Text

showDocInteractiveCommand :: Parser InteractiveCommand
showDocInteractiveCommand = do
    name <- readFullNameRef
    return $ ShowDocInteractiveCommand name

showTypeInteractiveCommand :: Bool -> Parser InteractiveCommand
showTypeInteractiveCommand showinfo = do
    expr <- readExpression
    return $ ShowTypeInteractiveCommand showinfo expr

simplifyPolarTypeInteractiveCommand :: Polarity -> Parser InteractiveCommand
simplifyPolarTypeInteractiveCommand polarity = do
    stype <- readType
    return $ SimplifyTypeInteractiveCommand polarity stype

readPolarity :: Parser Polarity
readPolarity =
    (readExactlyThis TokOperator "+" >> return Positive) <|> (readExactlyThis TokOperator "-" >> return Negative)

simplifyTypeInteractiveCommand :: Parser InteractiveCommand
simplifyTypeInteractiveCommand = do
    polarity <- readPolarity
    simplifyPolarTypeInteractiveCommand polarity

readSpecialCommand :: Text -> Parser InteractiveCommand
readSpecialCommand "doc" = showDocInteractiveCommand
readSpecialCommand "t" = showTypeInteractiveCommand False
readSpecialCommand "type" = showTypeInteractiveCommand False
readSpecialCommand "info" = showTypeInteractiveCommand True
readSpecialCommand "simplify" = simplifyTypeInteractiveCommand
readSpecialCommand "simplify-" = simplifyPolarTypeInteractiveCommand Negative
readSpecialCommand cmd = return $ ErrorInteractiveCommand $ "unknown interactive command: " <> cmd

readInteractiveCommand :: Parser InteractiveCommand
readInteractiveCommand =
    ( do
        readThis TokTypeJudge
        cmd <- (readLName >>= \(MkName cmd) -> return cmd) <|> (readThis TokType >> return "type")
        readSpecialCommand cmd
    )
        <|> (try $ readEnd >> return NullInteractiveCommand)
        <|> ( do
                dl <- readDoLine
                return
                    $ case dl of
                        ExpressionDoLine sexpr -> ExpressionInteractiveCommand sexpr
                        BindDoLine spat sexpr -> BindActionInteractiveCommand spat sexpr
                        DeclarationDoLine sdecl -> LetInteractiveCommand sdecl
            )

parseInteractiveCommand :: Text -> StateT SourcePos ParseResult InteractiveCommand
parseInteractiveCommand = runParser readInteractiveCommand
