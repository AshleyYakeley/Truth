module Pinafore.Syntax.Parse.Infix
    ( FixityReader (..)
    , readInfixed
    )
where

import Pinafore.Base
import Shapes hiding (try)

import Pinafore.Syntax.Name
import Pinafore.Syntax.Parse.Parser

data FixityReader e = MkFixityReader
    { efrReadInfix :: Parser (FullNameRef, Fixity, e -> e -> e)
    , efrMaxPrecedence :: Word
    }

leftApply :: e -> [(e -> e -> e, e)] -> e
leftApply e1 [] = e1
leftApply e1 ((f, e2) : rest) = leftApply (f e1 e2) rest

rightApply :: e -> [(e -> e -> e, e)] -> e
rightApply e1 [] = e1
rightApply e1 ((f, e2) : rest) = f e1 $ rightApply e2 rest

readInfixedPrec :: Word -> Parser (FullNameRef, Fixity, e -> e -> e) -> Parser e -> Parser e
readInfixedPrec 0 _ pe = pe
readInfixedPrec prec pi pe = do
    se1 <- readInfixedPrec (pred prec) pi pe
    rest <-
        many $ do
            (name, op, assoc) <-
                try $ do
                    (name, MkFixity assoc fprec, op) <- pi
                    if prec == fprec
                        then return (name, op, assoc)
                        else empty
            se2 <- readInfixedPrec (pred prec) pi pe
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
readInfixed fr = readInfixedPrec (efrMaxPrecedence fr) (efrReadInfix fr)
