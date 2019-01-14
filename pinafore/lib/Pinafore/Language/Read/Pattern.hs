module Pinafore.Language.Read.Pattern
    ( readPatterns
    , readPattern1
    ) where

import Pinafore.Language.Read.Constructor
import Pinafore.Language.Read.Parser
import Pinafore.Language.Read.Token
import Pinafore.Language.Syntax
import Shapes hiding (try)

readSourcePosPattern :: Parser SyntaxPattern' -> Parser SyntaxPattern
readSourcePosPattern p = do
    spos <- getPosition
    expr' <- p
    return $ MkSyntaxPattern spos expr'

readPatterns :: Parser [SyntaxPattern]
readPatterns = many readPattern2

readPattern1 :: Parser SyntaxPattern
readPattern1 = do
    f <- readPattern2
    args <- readPatterns
    case args of
        [] -> return f
        _ ->
            case f of
                MkSyntaxPattern spos (ConstructorSyntaxPattern c cargs) ->
                    return $ MkSyntaxPattern spos $ ConstructorSyntaxPattern c $ cargs <> args
                _ -> fail "cannot apply pattern"

readPattern2 :: Parser SyntaxPattern
readPattern2 = do
    spos <- getPosition
    pat1 <- readPattern3
    mpat2 <-
        optional $ do
            readThis TokAt
            readPattern2
    case mpat2 of
        Nothing -> return pat1
        Just pat2 -> return $ MkSyntaxPattern spos $ BothSyntaxPattern pat1 pat2

readPattern3 :: Parser SyntaxPattern
readPattern3 =
    readSourcePosPattern
        (do
             c <- readConstructor
             return $ ConstructorSyntaxPattern c []) <|>
    readSourcePosPattern
        (do
             name <- readThis TokLName
             return $ VarSyntaxPattern name) <|>
    readSourcePosPattern
        (do
             readThis TokUnderscore
             return AnySyntaxPattern) <|>
    readParen readPattern1
