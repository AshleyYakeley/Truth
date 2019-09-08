module Pinafore.Language.Read.Infix
    ( readExpressionInfixed
    , operatorFixity
    , Fixity(..)
    , FixAssoc(..)
    ) where

import Pinafore.Base
import Pinafore.Language.Name
import Pinafore.Language.Read.Parser
import Pinafore.Language.Read.Token
import Pinafore.Language.Syntax
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
operatorFixity "!$$" = MkFixity AssocRight 8
operatorFixity "!@" = MkFixity AssocRight 8
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
operatorFixity ":" = MkFixity AssocRight 6
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

readInfix :: Int -> Parser (Name, FixAssoc, SyntaxExpression baseupdate)
readInfix prec =
    try $ do
        spos <- getPosition
        name <- readThis TokOperator
        let MkFixity assoc fprec = operatorFixity name
        if prec == fprec
            then return (name, assoc, MkSyntaxExpression spos $ SEVar name)
            else empty

leftApply ::
       HasPinaforeEntityUpdate baseupdate
    => SyntaxExpression baseupdate
    -> [(SourcePos, SyntaxExpression baseupdate, SyntaxExpression baseupdate)]
    -> SyntaxExpression baseupdate
leftApply e1 [] = e1
leftApply e1 ((spos, f, e2):rest) = leftApply (seApplys spos f [e1, e2]) rest

rightApply ::
       HasPinaforeEntityUpdate baseupdate
    => SyntaxExpression baseupdate
    -> [(SourcePos, SyntaxExpression baseupdate, SyntaxExpression baseupdate)]
    -> SyntaxExpression baseupdate
rightApply e1 [] = e1
rightApply e1 ((spos, f, e2):rest) = seApplys spos f [e1, rightApply e2 rest]

readInfixedExpression ::
       forall baseupdate. HasPinaforeEntityUpdate baseupdate
    => Parser (SyntaxExpression baseupdate)
    -> Int
    -> Parser (SyntaxExpression baseupdate)
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

readExpressionInfixed ::
       forall baseupdate. HasPinaforeEntityUpdate baseupdate
    => Parser (SyntaxExpression baseupdate)
    -> Parser (SyntaxExpression baseupdate)
readExpressionInfixed pe = readInfixedExpression pe 0
