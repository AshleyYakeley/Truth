module Pinafore.Language.Grammar.Read.Constructor
    ( readConstructor
    ) where

import Pinafore.Language.Grammar.Read.Parser
import Pinafore.Language.Grammar.Read.Token
import Pinafore.Language.Grammar.Syntax
import Shapes hiding (try)

readConstructor :: Maybe (Parser SyntaxExpression) -> Parser SyntaxConstructor
readConstructor mp =
    (do
         name <- readFullUName
         mvals <-
             case mp of
                 Just p ->
                     optional $
                     readOf $ do
                         n <- readLName
                         readThis TokAssign
                         v <- p
                         return (n, v)
                 Nothing -> return Nothing
         return $ SLNamedConstructor name mvals) <|>
    (do
         n <- readThis TokNumber
         return $ SLNumber n) <|>
    (do
         str <- readThis TokString
         return $ SLString str)
