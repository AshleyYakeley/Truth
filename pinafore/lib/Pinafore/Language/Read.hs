module Pinafore.Language.Read
    ( parseExpression
    , parseInteractiveCommand
    ) where

import Language.Expression.Unitype
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

readBinding :: HasPinaforeEntityEdit baseedit => Parser (QBindings baseedit)
readBinding = do
    name <- readThis TokName
    args <- many readPattern
    readThis TokAssign
    val <- readExpression
    return $ bindExpression name $ qAbstractsExpr args val

readBindings :: HasPinaforeEntityEdit baseedit => Parser (QBindings baseedit)
readBindings =
    (do
         b <- readBinding
         mbb <-
             optional $ do
                 readThis TokSemicolon
                 readBindings
         return $ b <> fromMaybe mempty mbb) <|>
    (do return mempty)

readLetBindings :: HasPinaforeEntityEdit baseedit => Parser (QExpression baseedit -> QExpression baseedit)
readLetBindings = do
    readThis TokLet
    bindings <- readBindings
    bindingsLetExpression bindings

readExpression ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (QExpression baseedit)
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
    => QExpression baseedit
    -> [(QExpression baseedit, QExpression baseedit)]
    -> QExpression baseedit
leftApply e1 [] = e1
leftApply e1 ((f, e2):rest) = leftApply (qApplyAllExpr f [e1, e2]) rest

rightApply ::
       HasPinaforeEntityEdit baseedit
    => QExpression baseedit
    -> [(QExpression baseedit, QExpression baseedit)]
    -> QExpression baseedit
rightApply e1 [] = e1
rightApply e1 ((f, e2):rest) = qApplyAllExpr f [e1, rightApply e2 rest]

readInfixedExpression ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Int
    -> Parser (QExpression baseedit)
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
        [(AssocNone, name, e2)] -> return $ qApplyAllExpr (varSealedUnitypeExpression name) [e1, e2]
        _
            | all (\(assoc, _, _) -> assoc == AssocLeft) rest ->
                return $ leftApply e1 $ fmap (\(_, name, e2) -> (varSealedUnitypeExpression name, e2)) rest
        _
            | all (\(assoc, _, _) -> assoc == AssocRight) rest ->
                return $ rightApply e1 $ fmap (\(_, name, e2) -> (varSealedUnitypeExpression name, e2)) rest
        _ -> parserFail $ "incompatible infix operators: " ++ intercalate " " (fmap (\(_, name, _) -> show name) rest)

readExpression1 ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (QExpression baseedit)
readExpression1 =
    (do
         readThis TokLambda
         args <- many readPattern
         readThis TokMap
         val <- readExpression
         return $ qAbstractsExpr args val) <|>
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
         return $ qApplyAllExpr (opoint $ toQValue $ qifthenelse @baseedit) [etest, ethen, eelse]) <|>
    readExpression2

readExpression2 :: HasPinaforeEntityEdit baseedit => Parser (QExpression baseedit)
readExpression2 = do
    e1 <- readExpression3
    args <- many readExpression3
    return $ qApplyAllExpr e1 args

readExpression3 ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (QExpression baseedit)
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

readInteractiveCommand ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (Either (QExpression baseedit -> QExpression baseedit) (QExpression baseedit))
readInteractiveCommand = (eof >> return (Left id)) <|> (try $ fmap Right readExpression) <|> (fmap Left readLetBindings)

parseReader :: Parser t -> SourceName -> Text -> Result Text t
parseReader parser name text = do
    toks <- parseTokens name text
    case parse parser name toks of
        Right a -> return a
        Left e -> fail $ show e

parseExpression :: HasPinaforeEntityEdit baseedit => SourceName -> Text -> Result Text (QExpression baseedit)
parseExpression = parseReader readExpression

parseInteractiveCommand ::
       HasPinaforeEntityEdit baseedit
    => SourceName
    -> Text
    -> Result Text (Either (QExpression baseedit -> QExpression baseedit) (QExpression baseedit))
parseInteractiveCommand = parseReader readInteractiveCommand
