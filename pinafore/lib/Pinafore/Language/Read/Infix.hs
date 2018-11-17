module Pinafore.Language.Read.Infix
    ( readExpressionInfixed
    ) where

import Pinafore.Base
import Pinafore.Language.Expression
import Pinafore.Language.Name
import Pinafore.Language.Read.Parser
import Pinafore.Language.Read.RefNotation
import Pinafore.Language.Token
import Shapes hiding (try)
import Text.Parsec hiding ((<|>), many, optional)

data FixAssoc
    = AssocNone
    | AssocLeft
    | AssocRight
    deriving (Eq)

data Fixity =
    MkFixity FixAssoc
             Int

-- following Haskell
-- https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-820061
operatorFixity :: Name -> Fixity
operatorFixity "." = MkFixity AssocRight 9
operatorFixity "<.>" = MkFixity AssocRight 9
operatorFixity "*" = MkFixity AssocLeft 8
operatorFixity "/" = MkFixity AssocLeft 8
operatorFixity "/\\" = MkFixity AssocLeft 8
operatorFixity "+" = MkFixity AssocLeft 7
operatorFixity "-" = MkFixity AssocLeft 7
operatorFixity "\\/" = MkFixity AssocLeft 7
operatorFixity "++" = MkFixity AssocRight 6
operatorFixity "==" = MkFixity AssocNone 5
operatorFixity "/=" = MkFixity AssocNone 5
operatorFixity "~==" = MkFixity AssocNone 5
operatorFixity "~/=" = MkFixity AssocNone 5
operatorFixity "<=" = MkFixity AssocNone 5
operatorFixity "<" = MkFixity AssocNone 5
operatorFixity ">=" = MkFixity AssocNone 5
operatorFixity ">" = MkFixity AssocNone 5
operatorFixity "&&" = MkFixity AssocRight 4
operatorFixity "||" = MkFixity AssocRight 3
operatorFixity ":=" = MkFixity AssocNone 2
operatorFixity "+=" = MkFixity AssocNone 2
operatorFixity "-=" = MkFixity AssocNone 2
operatorFixity ">>" = MkFixity AssocLeft 1
operatorFixity "??" = MkFixity AssocLeft 1
operatorFixity "$" = MkFixity AssocRight 0
operatorFixity _ = MkFixity AssocLeft 9

readInfix :: Int -> Parser (FixAssoc, Name)
readInfix prec =
    Text.Parsec.try $ do
        name <- readThis TokOperator
        let MkFixity assoc fprec = operatorFixity name
        if prec == fprec
            then return (assoc, name)
            else empty

leftApply ::
       HasPinaforePointEdit baseedit => QExpr baseedit -> [(QExpr baseedit, QExpr baseedit)] -> RefExpression baseedit
leftApply e1 [] = return e1
leftApply e1 ((f, e2):rest) = do
    ee <- liftRefNotation $ qApplyAllExpr f [e1, e2]
    leftApply ee rest

rightApply ::
       HasPinaforePointEdit baseedit => QExpr baseedit -> [(QExpr baseedit, QExpr baseedit)] -> RefExpression baseedit
rightApply e1 [] = return e1
rightApply e1 ((f, e2):rest) = do
    ee <- rightApply e2 rest
    liftRefNotation $ qApplyAllExpr f [e1, ee]

readInfixedExpression ::
       forall baseedit. HasPinaforePointEdit baseedit
    => Parser (RefExpression baseedit)
    -> Int
    -> Parser (RefExpression baseedit)
readInfixedExpression pe 10 = pe
readInfixedExpression pe prec = do
    te1 <- readInfixedExpression pe (succ prec)
    rest <-
        many $ do
            (assoc, name) <- readInfix prec
            te2 <- readInfixedExpression pe (succ prec)
            return (assoc, name, te2)
    case rest of
        [] -> return te1
        [(AssocNone, name, te2)] ->
            return $ do
                e1 <- te1
                e2 <- te2
                liftRefNotation $ qApplyAllExpr (qVarExpr name) [e1, e2]
        _
            | all (\(assoc, _, _) -> assoc == AssocLeft) rest ->
                return $ do
                    e1 <- te1
                    pairs <-
                        for rest $ \(_, name, te2) -> do
                            e2 <- te2
                            return (qVarExpr name, e2)
                    leftApply e1 pairs
        _
            | all (\(assoc, _, _) -> assoc == AssocRight) rest ->
                return $ do
                    e1 <- te1
                    pairs <-
                        for rest $ \(_, name, te2) -> do
                            e2 <- te2
                            return (qVarExpr name, e2)
                    rightApply e1 pairs
        _ -> parserFail $ "incompatible infix operators: " ++ intercalate " " (fmap (\(_, name, _) -> show name) rest)

readExpressionInfixed ::
       forall baseedit. HasPinaforePointEdit baseedit
    => Parser (RefExpression baseedit)
    -> Parser (RefExpression baseedit)
readExpressionInfixed pe = readInfixedExpression pe 0
