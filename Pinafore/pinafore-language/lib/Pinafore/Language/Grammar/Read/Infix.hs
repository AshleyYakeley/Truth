module Pinafore.Language.Grammar.Read.Infix
    ( FixityReader(..)
    , readInfixed
    ) where

import Pinafore.Language.ExprShow
import Pinafore.Language.Grammar.Read.Parser
import Pinafore.Language.Name
import Shapes hiding (try)

data FixityReader e = MkFixityReader
    { efrReadInfix :: Parser (Name, Fixity, e -> e -> e)
    , efrMaxPrecedence :: Int
    }

leftApply :: e -> [(e -> e -> e, e)] -> e
leftApply e1 [] = e1
leftApply e1 ((f, e2):rest) = leftApply (f e1 e2) rest

rightApply :: e -> [(e -> e -> e, e)] -> e
rightApply e1 [] = e1
rightApply e1 ((f, e2):rest) = f e1 $ rightApply e2 rest

readInfixedPrec :: Int -> FixityReader e -> Parser e -> Parser e
readInfixedPrec prec fr pe
    | prec == succ (efrMaxPrecedence fr) = pe
readInfixedPrec prec fr pe = do
    se1 <- readInfixedPrec (succ prec) fr pe
    rest <-
        many $ do
            (name, op, assoc) <-
                try $ do
                    (name, MkFixity assoc fprec, op) <- efrReadInfix fr
                    if prec == fprec
                        then return (name, op, assoc)
                        else empty
            se2 <- readInfixedPrec (succ prec) fr pe
            return (name, op, assoc, se2)
    case rest of
        [] -> return se1
        [(_, seop, AssocNone, se2)] -> return $ seop se1 se2
        _
            | all (\(_, _, assoc, _) -> assoc == AssocLeft) rest ->
                return $ leftApply se1 $ fmap (\(_, seop, _, se2) -> (seop, se2)) rest
        _
            | all (\(_, _, assoc, _) -> assoc == AssocRight) rest ->
                return $ rightApply se1 $ fmap (\(_, seop, _, se2) -> (seop, se2)) rest
        _ -> fail $ "incompatible infix operators: " ++ intercalate " " (fmap (\(name, _, _, _) -> show name) rest)

readInfixed :: FixityReader e -> Parser e -> Parser e
readInfixed = readInfixedPrec 0
