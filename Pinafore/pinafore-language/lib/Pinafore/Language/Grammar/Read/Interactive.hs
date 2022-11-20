module Pinafore.Language.Grammar.Read.Interactive
    ( InteractiveCommand(..)
    , parseInteractiveCommand
    ) where

import Pinafore.Language.Error
import Pinafore.Language.Grammar.Read.Expression
import Pinafore.Language.Grammar.Read.Parser
import Pinafore.Language.Grammar.Read.Token
import Pinafore.Language.Grammar.Read.Type
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Name
import Pinafore.Language.Type
import Shapes hiding (try)

data InteractiveCommand
    = NullInteractiveCommand
    | LetInteractiveCommand SyntaxTopDeclarations
    | ExpressionInteractiveCommand SyntaxExpression
    | BindActionInteractiveCommand SyntaxPattern
                                   SyntaxExpression
    | ShowDocInteractiveCommand FullNameRef
    | ShowTypeInteractiveCommand Bool
                                 SyntaxExpression
    | SimplifyTypeInteractiveCommand Polarity
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
    (do
         readThis TokTypeJudge
         MkName cmd <- readLName
         readSpecialCommand cmd) <|>
    (readEnd >> return NullInteractiveCommand) <|>
    (try $ do
         dl <- readDoLine
         return $
             case dl of
                 ExpressionDoLine sexpr -> ExpressionInteractiveCommand sexpr
                 BindDoLine spat sexpr -> BindActionInteractiveCommand spat sexpr) <|>
    (do
         stdecls <- readTopDeclarations
         return $ LetInteractiveCommand stdecls)

parseInteractiveCommand :: Text -> StateT SourcePos InterpretResult InteractiveCommand
parseInteractiveCommand = parseReader readInteractiveCommand
