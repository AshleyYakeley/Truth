module Pinafore.Syntax.Parse.Constructor
    ( readRecordValue
    , readConstructor
    ) where

import Pinafore.Syntax.Name
import Pinafore.Syntax.Parse.Basic
import Pinafore.Syntax.Parse.Parser
import Pinafore.Syntax.Parse.Token
import Pinafore.Syntax.Syntax
import Shapes hiding (try)

readRecordValue :: Parser SyntaxExpression -> Parser [(Name, SyntaxExpression)]
readRecordValue p =
    readOf $ do
        n <- readLName
        readThis TokAssign
        v <- p
        return (n, v)

readConstructor :: Maybe (Parser SyntaxExpression) -> Parser SyntaxConstructor
readConstructor mp =
    (do
         name <- readFullUName
         mvals <-
             case mp of
                 Just p -> optional $ readRecordValue p
                 Nothing -> return Nothing
         return $ SLNamedConstructor name mvals) <|>
    (do
         n <- readThis TokNumber
         return $ SLNumber n) <|>
    (do
         str <- readThis TokString
         return $ SLString str)
