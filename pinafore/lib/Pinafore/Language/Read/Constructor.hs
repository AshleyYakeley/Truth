module Pinafore.Language.Read.Constructor
    ( readConstructor
    ) where

import Pinafore.Language.Read.Parser
import Pinafore.Language.Read.Token
import Pinafore.Language.Syntax
import Shapes hiding (try)

readConstructor :: Parser SyntaxConstructor
readConstructor =
    (do
         name <- readThis TokUName
         return $ SLNamedConstructor name) <|>
    (do
         n <- readThis TokNumber
         return $ SLNumber n) <|>
    (do
         str <- readThis TokString
         return $ SLString str)
