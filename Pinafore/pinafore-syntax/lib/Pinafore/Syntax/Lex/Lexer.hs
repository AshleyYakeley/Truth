module Pinafore.Syntax.Lex.Lexer where

import Shapes
import Text.Parsec

type Lexer = Parsec String ()

lexChar :: Char -> Lexer ()
lexChar c = void $ char c

lexString :: String -> Lexer ()
lexString s = void $ string s

runLexerWhole :: forall a. Lexer a -> String -> Maybe a
runLexerWhole lexer s = let
    lexerWhole :: Lexer a
    lexerWhole = do
        r <- lexer
        eof
        pure r
    in mToMaybe $ parse lexerWhole "" s
