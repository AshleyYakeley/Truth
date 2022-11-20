module Pinafore.Language.Grammar.Read.Constructor
    ( readConstructor
    ) where

import Pinafore.Language.Grammar.Read.Parser
import Pinafore.Language.Grammar.Read.Token
import Pinafore.Language.Grammar.Syntax
import Shapes hiding (try)

readConstructor :: Parser SyntaxConstructor
readConstructor =
    (do
         name <- readFullUName
         return $ SLNamedConstructor name) <|>
    (do
         n <- readThis TokNumber
         return $ SLNumber n) <|>
    (do
         str <- readThis TokString
         return $ SLString str)
