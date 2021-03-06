module Pinafore.Language.Grammar.Read.Infix
    ( readExpressionInfixed
    , operatorFixity
    , Fixity(..)
    , FixAssoc(..)
    ) where

import Pinafore.Language.Grammar.Read.Parser
import Pinafore.Language.Grammar.Read.Token
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Name
import Shapes hiding (try)

data FixAssoc
    = AssocNone
    | AssocLeft
    | AssocRight
    deriving (Eq)

data Fixity =
    MkFixity FixAssoc
             Int
    deriving (Eq)

-- following Haskell
-- https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-820061
operatorFixity :: Name -> Fixity
operatorFixity "." = MkFixity AssocRight 10
operatorFixity "!." = MkFixity AssocRight 10
operatorFixity "^" = MkFixity AssocRight 9
operatorFixity "^^" = MkFixity AssocRight 9
operatorFixity "**" = MkFixity AssocRight 9
operatorFixity "!**" = MkFixity AssocLeft 9
operatorFixity "!++" = MkFixity AssocLeft 9
operatorFixity "*" = MkFixity AssocLeft 8
operatorFixity ".*" = MkFixity AssocLeft 8
operatorFixity "~*" = MkFixity AssocLeft 8
operatorFixity "/" = MkFixity AssocLeft 8
operatorFixity "~/" = MkFixity AssocLeft 8
operatorFixity "<*>" = MkFixity AssocLeft 8
operatorFixity "<:*:>" = MkFixity AssocLeft 8
operatorFixity "!$" = MkFixity AssocRight 8
operatorFixity "!$%" = MkFixity AssocRight 8
operatorFixity "!$$" = MkFixity AssocRight 8
operatorFixity "!@" = MkFixity AssocRight 8
operatorFixity "!@%" = MkFixity AssocRight 8
operatorFixity "!@@" = MkFixity AssocRight 8
operatorFixity "+" = MkFixity AssocLeft 7
operatorFixity ".+" = MkFixity AssocLeft 7
operatorFixity "~+" = MkFixity AssocLeft 7
operatorFixity "-" = MkFixity AssocLeft 7
operatorFixity ".-" = MkFixity AssocLeft 7
operatorFixity "~-" = MkFixity AssocLeft 7
operatorFixity "??" = MkFixity AssocLeft 7
operatorFixity "<+>" = MkFixity AssocLeft 7
operatorFixity "<:+:>" = MkFixity AssocLeft 7
operatorFixity "::" = MkFixity AssocRight 6
operatorFixity "++" = MkFixity AssocRight 6
operatorFixity "<>" = MkFixity AssocRight 6
operatorFixity "==" = MkFixity AssocNone 5
operatorFixity "/=" = MkFixity AssocNone 5
operatorFixity "~==" = MkFixity AssocNone 5
operatorFixity "~/=" = MkFixity AssocNone 5
operatorFixity "<=" = MkFixity AssocNone 5
operatorFixity "<" = MkFixity AssocNone 5
operatorFixity ">=" = MkFixity AssocNone 5
operatorFixity ">" = MkFixity AssocNone 5
operatorFixity "<&>" = MkFixity AssocLeft 4
operatorFixity "<:&:>" = MkFixity AssocLeft 4
operatorFixity "<:&>" = MkFixity AssocLeft 4
operatorFixity "<\\>" = MkFixity AssocLeft 4
operatorFixity "<:\\>" = MkFixity AssocLeft 4
operatorFixity "<^>" = MkFixity AssocLeft 4
operatorFixity "&&" = MkFixity AssocRight 4
operatorFixity "<|>" = MkFixity AssocLeft 3
operatorFixity "<:|:>" = MkFixity AssocLeft 3
operatorFixity "||" = MkFixity AssocRight 3
operatorFixity ":=" = MkFixity AssocNone 2
operatorFixity "+=" = MkFixity AssocNone 2
operatorFixity "-=" = MkFixity AssocNone 2
operatorFixity ">>=" = MkFixity AssocLeft 1
operatorFixity ">>" = MkFixity AssocLeft 1
operatorFixity "$" = MkFixity AssocRight 0
operatorFixity _ = MkFixity AssocLeft 10

readInfix :: Int -> Parser (Name, FixAssoc, SyntaxExpression)
readInfix prec =
    try $ do
        spos <- getPosition
        name <- readThis TokOperator
        let MkFixity assoc fprec = operatorFixity name
        if prec == fprec
            then return (name, assoc, MkWithSourcePos spos $ SEVar $ UnqualifiedReferenceName name)
            else empty

leftApply :: SyntaxExpression -> [(SourcePos, SyntaxExpression, SyntaxExpression)] -> SyntaxExpression
leftApply e1 [] = e1
leftApply e1 ((spos, f, e2):rest) = leftApply (seApplys spos f [e1, e2]) rest

rightApply :: SyntaxExpression -> [(SourcePos, SyntaxExpression, SyntaxExpression)] -> SyntaxExpression
rightApply e1 [] = e1
rightApply e1 ((spos, f, e2):rest) = seApplys spos f [e1, rightApply e2 rest]

readInfixedExpression :: Parser SyntaxExpression -> Int -> Parser SyntaxExpression
readInfixedExpression pe 11 = pe
readInfixedExpression pe prec = do
    spos <- getPosition
    se1 <- readInfixedExpression pe (succ prec)
    rest <-
        many $ do
            (name, assoc, seop) <- readInfix prec
            se2 <- readInfixedExpression pe (succ prec)
            return (name, assoc, seop, se2)
    case rest of
        [] -> return se1
        [(_, AssocNone, seop, se2)] -> return $ seApplys spos seop [se1, se2]
        _
            | all (\(_, assoc, _, _) -> assoc == AssocLeft) rest ->
                return $ leftApply se1 $ fmap (\(_, _, seop, se2) -> (spos, seop, se2)) rest
        _
            | all (\(_, assoc, _, _) -> assoc == AssocRight) rest ->
                return $ rightApply se1 $ fmap (\(_, _, seop, se2) -> (spos, seop, se2)) rest
        _ -> fail $ "incompatible infix operators: " ++ intercalate " " (fmap (\(name, _, _, _) -> show name) rest)

readExpressionInfixed :: Parser SyntaxExpression -> Parser SyntaxExpression
readExpressionInfixed pe = readInfixedExpression pe 0
