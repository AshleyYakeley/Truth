module Pinafore.Language.Read
    ( parseExpression
    , InteractiveCommand(..)
    , parseInteractiveCommand
    ) where

import Pinafore.Language.Convert
import Pinafore.Language.Expression
import Pinafore.Language.If
import Pinafore.Language.Name
import Pinafore.Language.Token
import Pinafore.Language.Value
import Pinafore.PredicateMorphism
import Shapes hiding (try)
import Text.Parsec hiding ((<|>), many, optional)

type Parser = Parsec [(SourcePos, Any Token)] ()

readThis :: Token t -> Parser t
readThis tok =
    token (\(_, MkAny tok' _) -> show tok') fst $ \(_, MkAny tok' t) ->
        case testEquality tok tok' of
            Just Refl -> Just t
            Nothing -> Nothing

readPattern :: Parser Name
readPattern = readThis TokName

readBinding :: HasPinaforeEntityEdit baseedit => Parser (QBindList baseedit)
readBinding = do
    name <- readThis TokName
    args <- many readPattern
    readThis TokAssign
    tval <- readExpression
    return $
        qBindExpr name $ do
            val <- tval
            qAbstractsExpr args val

readBindings :: HasPinaforeEntityEdit baseedit => Parser (QBindList baseedit)
readBindings =
    (do
         b <- readBinding
         mbl <-
             optional $ do
                 readThis TokSemicolon
                 readBindings
         return $ b <> fromMaybe mempty mbl) <|>
    (do return mempty)

readLetBindings :: HasPinaforeEntityEdit baseedit => Parser (QExpr baseedit -> QTypeCheck (QExpr baseedit))
readLetBindings = do
    readThis TokLet
    bl <- readBindings
    qBindingsLetExpr bl

readExpression ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (QTypeCheck (QExpr baseedit))
readExpression = readInfixedExpression 0

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
        name <- readThis TokInfix
        let MkFixity assoc fprec = operatorFixity name
        if prec == fprec
            then return (assoc, name)
            else empty

leftApply ::
       HasPinaforeEntityEdit baseedit
    => QExpr baseedit
    -> [(QExpr baseedit, QExpr baseedit)]
    -> QTypeCheck (QExpr baseedit)
leftApply e1 [] = return e1
leftApply e1 ((f, e2):rest) = do
    ee <- qApplyAllExpr f [e1, e2]
    leftApply ee rest

rightApply ::
       HasPinaforeEntityEdit baseedit
    => QExpr baseedit
    -> [(QExpr baseedit, QExpr baseedit)]
    -> QTypeCheck (QExpr baseedit)
rightApply e1 [] = return e1
rightApply e1 ((f, e2):rest) = do
    ee <- rightApply e2 rest
    qApplyAllExpr f [e1, ee]

readInfixedExpression ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Int
    -> Parser (QTypeCheck (QExpr baseedit))
readInfixedExpression 10 = readExpression1
readInfixedExpression prec = do
    te1 <- readInfixedExpression (succ prec)
    rest <-
        many $ do
            (assoc, name) <- readInfix prec
            te2 <- readInfixedExpression (succ prec)
            return (assoc, name, te2)
    case rest of
        [] -> return te1
        [(AssocNone, name, te2)] ->
            return $ do
                e1 <- te1
                e2 <- te2
                ev <- qVarExpr name
                qApplyAllExpr ev [e1, e2]
        _
            | all (\(assoc, _, _) -> assoc == AssocLeft) rest ->
                return $ do
                    e1 <- te1
                    pairs <-
                        for rest $ \(_, name, te2) -> do
                            e2 <- te2
                            ev <- qVarExpr name
                            return (ev, e2)
                    leftApply e1 pairs
        _
            | all (\(assoc, _, _) -> assoc == AssocRight) rest ->
                return $ do
                    e1 <- te1
                    pairs <-
                        for rest $ \(_, name, te2) -> do
                            e2 <- te2
                            ev <- qVarExpr name
                            return (ev, e2)
                    rightApply e1 pairs
        _ -> parserFail $ "incompatible infix operators: " ++ intercalate " " (fmap (\(_, name, _) -> show name) rest)

readExpression1 ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (QTypeCheck (QExpr baseedit))
readExpression1 =
    (do
         readThis TokLambda
         args <- many readPattern
         readThis TokMap
         mval <- readExpression
         return $ do
             val <- mval
             qAbstractsExpr args val) <|>
    (do
         bmap <- readLetBindings
         readThis TokIn
         mbody <- readExpression
         return $ do
             body <- mbody
             bmap body) <|>
    (do
         readThis TokIf
         metest <- readExpression
         readThis TokThen
         methen <- readExpression
         readThis TokElse
         meelse <- readExpression
         return $ do
             etest <- metest
             ethen <- methen
             eelse <- meelse
             qApplyAllExpr (opoint $ toQValue $ qifthenelse @baseedit) [etest, ethen, eelse]) <|>
    readExpression2

readExpression2 :: HasPinaforeEntityEdit baseedit => Parser (QTypeCheck (QExpr baseedit))
readExpression2 = do
    te1 <- readExpression3
    targs <- many readExpression3
    return $ do
        e1 <- te1
        args <- sequence targs
        qApplyAllExpr e1 args

readExpression3 ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (QTypeCheck (QExpr baseedit))
readExpression3 =
    (do
         b <- readThis TokBool
         return $ qConstExpr b) <|>
    (do
         name <- readThis TokName
         return $ qVarExpr name) <|>
    (do
         n <- readThis TokNumber
         return $ qConstExpr n) <|>
    (do
         str <- readThis TokString
         return $ qConstExpr str) <|>
    (do
         readThis TokOpenParen
         expr <- readExpression
         readThis TokCloseParen
         return $ expr) <|>
    (do
         readThis TokOpenBracket
         mexprs <-
             (do
                  expr1 <- readExpression
                  exprs <-
                      many $ do
                          readThis TokComma
                          readExpression
                  return $ expr1 : exprs) <|>
             return []
         readThis TokCloseBracket
         return $ do
             exprs <- sequence mexprs
             return $ qSequenceExpr exprs) <|>
    (do
         readThis TokInvert
         return $ qConstExpr $ qinvert @baseedit) <|>
    (do
         p <- readThis TokPredicate
         return $ qConstExpr p) <|>
    (do
         p <- readThis TokPoint
         return $ qConstExpr p) <?>
    "expression"

data InteractiveCommand baseedit
    = LetInteractiveCommand (QExpr baseedit -> QTypeCheck (QExpr baseedit))
    | ExpressionInteractiveCommand (QTypeCheck (QExpr baseedit))

readInteractiveCommand ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (InteractiveCommand baseedit)
readInteractiveCommand =
    (eof >> return (LetInteractiveCommand return)) <|> (try $ fmap ExpressionInteractiveCommand readExpression) <|>
    (fmap LetInteractiveCommand readLetBindings)

parseReader :: Parser t -> SourceName -> Text -> Result Text t
parseReader parser name text = do
    toks <- parseTokens name text
    case parse parser name toks of
        Right a -> return a
        Left e -> fail $ show e

parseExpression :: HasPinaforeEntityEdit baseedit => SourceName -> Text -> Result Text (QTypeCheck (QExpr baseedit))
parseExpression = parseReader readExpression

parseInteractiveCommand ::
       HasPinaforeEntityEdit baseedit => SourceName -> Text -> Result Text (InteractiveCommand baseedit)
parseInteractiveCommand = parseReader readInteractiveCommand
