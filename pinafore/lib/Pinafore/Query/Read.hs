module Pinafore.Query.Read
    ( parseExpression
    , parseInteractiveCommand
    ) where

import Pinafore.PredicateMorphism
import Pinafore.Query.Convert
import Pinafore.Query.Expression
import Pinafore.Query.Token
import Pinafore.Query.Value
import Shapes hiding (try)
import Text.Parsec hiding ((<|>), many, optional)

type Parser = Parsec [(SourcePos, Any Token)] ()

readThis :: Token t -> Parser t
readThis tok =
    token (\(_, MkAny tok' _) -> show tok') fst $ \(_, MkAny tok' t) ->
        case testEquality tok tok' of
            Just Refl -> Just t
            Nothing -> Nothing

readPattern :: Parser Symbol
readPattern = readThis TokSymbol

readBinding :: HasPinaforeTableEdit baseedit => Parser (Symbol, QValueExpr baseedit)
readBinding = do
    name <- readThis TokSymbol
    args <- many readPattern
    readThis TokAssign
    val <- readExpression
    return (name, exprAbstracts args val)

readBindings :: HasPinaforeTableEdit baseedit => Parser (QBindings baseedit)
readBindings =
    (do
         b <- readBinding
         mbb <-
             optional $ do
                 readThis TokSemicolon
                 MkQBindings bb <- readBindings
                 return bb
         return $ MkQBindings $ b : (fromMaybe [] mbb)) <|>
    (do return $ MkQBindings [])

readLetBindings :: HasPinaforeTableEdit baseedit => Parser (QValueExpr baseedit -> QValueExpr baseedit)
readLetBindings = do
    readThis TokLet
    bindings <- readBindings
    case getDuplicates bindings of
        [] -> return $ qlets bindings
        l -> parserFail $ "duplicate bindings: " ++ intercalate ", " (fmap show l)

readExpression ::
       forall baseedit. HasPinaforeTableEdit baseedit
    => Parser (QValueExpr baseedit)
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
operatorFixity :: Symbol -> Fixity
operatorFixity "." = MkFixity AssocRight 9
operatorFixity "*" = MkFixity AssocLeft 7
operatorFixity "/" = MkFixity AssocLeft 7
operatorFixity "+" = MkFixity AssocLeft 6
operatorFixity "-" = MkFixity AssocLeft 6
operatorFixity "++" = MkFixity AssocRight 5
operatorFixity "==" = MkFixity AssocNone 4
operatorFixity "/=" = MkFixity AssocNone 4
operatorFixity "~==" = MkFixity AssocNone 4
operatorFixity "~/=" = MkFixity AssocNone 4
operatorFixity "<=" = MkFixity AssocNone 4
operatorFixity "<" = MkFixity AssocNone 4
operatorFixity ">=" = MkFixity AssocNone 4
operatorFixity ">" = MkFixity AssocNone 4
operatorFixity "&" = MkFixity AssocRight 3
operatorFixity "|" = MkFixity AssocRight 2
operatorFixity ">>" = MkFixity AssocLeft 1
operatorFixity "$" = MkFixity AssocRight 0
operatorFixity _ = MkFixity AssocLeft 9

readInfix :: Int -> Parser (FixAssoc, Symbol)
readInfix prec =
    Text.Parsec.try $ do
        name <- readThis TokInfix
        let MkFixity assoc fprec = operatorFixity name
        if prec == fprec
            then return (assoc, name)
            else empty

leftApply ::
       HasPinaforeTableEdit baseedit
    => QValueExpr baseedit
    -> [(QValueExpr baseedit, QValueExpr baseedit)]
    -> QValueExpr baseedit
leftApply e1 [] = e1
leftApply e1 ((f, e2):rest) = leftApply (exprApplyAll f [e1, e2]) rest

rightApply ::
       HasPinaforeTableEdit baseedit
    => QValueExpr baseedit
    -> [(QValueExpr baseedit, QValueExpr baseedit)]
    -> QValueExpr baseedit
rightApply e1 [] = e1
rightApply e1 ((f, e2):rest) = exprApplyAll f [e1, rightApply e2 rest]

readInfixedExpression ::
       forall baseedit. HasPinaforeTableEdit baseedit
    => Int
    -> Parser (QValueExpr baseedit)
readInfixedExpression 10 = readExpression1
readInfixedExpression prec = do
    e1 <- readInfixedExpression (succ prec)
    rest <-
        many $ do
            (assoc, name) <- readInfix prec
            e2 <- readInfixedExpression (succ prec)
            return (assoc, name, e2)
    case rest of
        [] -> return e1
        [(AssocNone, name, e2)] -> return $ exprApplyAll (qvar name) [e1, e2]
        _
            | all (\(assoc, _, _) -> assoc == AssocLeft) rest ->
                return $ leftApply e1 $ fmap (\(_, name, e2) -> (qvar name, e2)) rest
        _
            | all (\(assoc, _, _) -> assoc == AssocRight) rest ->
                return $ rightApply e1 $ fmap (\(_, name, e2) -> (qvar name, e2)) rest
        _ -> parserFail $ "incompatible infix operators: " ++ intercalate " " (fmap (\(_, name, _) -> show name) rest)

readExpression1 ::
       forall baseedit. HasPinaforeTableEdit baseedit
    => Parser (QValueExpr baseedit)
readExpression1 =
    (do
         readThis TokLambda
         args <- many readPattern
         readThis TokMap
         val <- readExpression
         return $ exprAbstracts args val) <|>
    (do
         bmap <- readLetBindings
         readThis TokIn
         body <- readExpression
         return $ bmap body) <|>
    (do
         readThis TokIf
         etest <- readExpression
         readThis TokThen
         ethen <- readExpression
         readThis TokElse
         eelse <- readExpression
         return $ exprApplyAll (pure $ toQValue $ qifthenelse @baseedit) [etest, ethen, eelse]) <|>
    readExpression2

readExpression2 :: HasPinaforeTableEdit baseedit => Parser (QValueExpr baseedit)
readExpression2 = do
    e1 <- readExpression3
    args <- many readExpression3
    return $ exprApplyAll e1 args

readExpression3 ::
       forall baseedit. HasPinaforeTableEdit baseedit
    => Parser (QValueExpr baseedit)
readExpression3 =
    (do
         b <- readThis TokBool
         return $ pure $ toQValue b) <|>
    (do
         name <- readThis TokSymbol
         return $ qvar name) <|>
    (do
         n <- readThis TokNumber
         return $ pure $ toQValue n) <|>
    (do
         str <- readThis TokString
         return $ pure $ toQValue str) <|>
    (do
         readThis TokOpenParen
         expr <- readExpression
         readThis TokCloseParen
         return $ expr) <|>
    (do
         readThis TokOpenBracket
         exprs <-
             (do
                  expr1 <- readExpression
                  exprs <-
                      many $ do
                          readThis TokComma
                          readExpression
                  return $ expr1 : exprs) <|>
             return []
         readThis TokCloseBracket
         return $ fmap (MkAny QTList) $ sequenceA exprs) <|>
    (do
         readThis TokInvert
         return $ pure $ toQValue $ qinvert @baseedit) <|>
    (do
         p <- readThis TokPredicate
         return $ pure $ toQValue p) <|>
    (do
         p <- readThis TokPoint
         return $ pure $ toQValue p) <?>
    "expression"

readInteractiveCommand ::
       forall baseedit. HasPinaforeTableEdit baseedit
    => Parser (Either (QValueExpr baseedit -> QValueExpr baseedit) (QValueExpr baseedit))
readInteractiveCommand = (eof >> return (Left id)) <|> (try $ fmap Right readExpression) <|> (fmap Left readLetBindings)

parseReader :: Parser t -> SourceName -> Text -> Result Text t
parseReader parser name text = do
    toks <- parseTokens name text
    case parse parser name toks of
        Right a -> return a
        Left e -> fail $ show e

parseExpression :: HasPinaforeTableEdit baseedit => SourceName -> Text -> Result Text (QValueExpr baseedit)
parseExpression = parseReader readExpression

parseInteractiveCommand ::
       HasPinaforeTableEdit baseedit
    => SourceName
    -> Text
    -> Result Text (Either (QValueExpr baseedit -> QValueExpr baseedit) (QValueExpr baseedit))
parseInteractiveCommand = parseReader readInteractiveCommand
