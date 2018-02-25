module Pinafore.Query.Read
    ( parseExpression
    ) where

import Data.UUID

import Pinafore.Query.Expression
import Pinafore.Query.Value
import Pinafore.Table
import Shapes
import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.String

readWS :: Parser ()
readWS = do
    spaces
    _ <-
        optional
            (do
                 readComment
                 readWS)
    return ()
    <?> "white space"
  where
    isLineBreak :: Char -> Bool
    isLineBreak '\n' = True
    isLineBreak '\r' = True
    isLineBreak _ = False
    readComment :: Parser ()
    readComment = do
        _ <- char '#'
        _ <- many (satisfy (\c -> not (isLineBreak c)))
        _ <- satisfy isLineBreak
        return ()

readCharAndWS :: Char -> Parser ()
readCharAndWS c = do
    _ <- char c
    readWS

readStringAndWS :: String -> Parser ()
readStringAndWS s = do
    _ <- Text.Parsec.try $ string s
    readWS

readEscapedChar :: Parser Char
readEscapedChar = do
    _ <- char '\\'
    c <- anyToken
    case c of
        'n' -> return '\n'
        't' -> return '\t'
        'r' -> return '\r'
        'f' -> return '\f'
        _ -> return c

readQuotedString :: Parser String
readQuotedString = do
    _ <- char '"'
    s <- many readQuotedChar
    readCharAndWS '"'
    return s
    <?> "string"
  where
    readQuotedChar :: Parser Char
    readQuotedChar = readEscapedChar <|> (satisfy ('"' /=))

identifierChar :: Char -> Bool
identifierChar '-' = True
identifierChar '_' = True
identifierChar c = isAlphaNum c

data Keyword t where
    KWLet :: Keyword ()
    KWIn :: Keyword ()
    KWBool :: Keyword Bool
    KWSymbol :: Keyword Symbol

instance Show (Keyword t) where
    show KWLet = show ("let" :: String)
    show KWIn = show ("in" :: String)
    show KWBool = "boolean constant"
    show KWSymbol = "symbol"

readKeyword :: Keyword t -> Parser t
readKeyword kw =
    Text.Parsec.try $
    ((do
          firstC <- satisfy isAlpha
          rest <- many $ satisfy identifierChar
          readWS
          case (kw, firstC : rest)
            -- keywords
                of
              (KWLet, "let") -> return ()
              (_, "let") -> empty
              (KWIn, "in") -> return ()
              (_, "in") -> empty
              (KWBool, "true") -> return True
              (_, "true") -> empty
              (KWBool, "false") -> return False
              (_, "false") -> empty
              (KWSymbol, name) -> return $ MkSymbol $ pack name
              (_, _) -> empty) <?>
     show kw)

readSymbol :: Parser Symbol
readSymbol = readKeyword KWSymbol

readPattern :: Parser Symbol
readPattern = readSymbol

readBinding :: HasPinaforeTableEdit baseedit => Parser (Symbol, QValueExpr baseedit)
readBinding = do
    name <- readSymbol
    args <- many readPattern
    readCharAndWS '='
    val <- readExpression
    return (name, exprAbstracts args val)

readBindings :: HasPinaforeTableEdit baseedit => Parser (QBindings baseedit)
readBindings =
    (do
         b <- readBinding
         mbb <-
             optional $ do
                 readCharAndWS ';'
                 MkQBindings bb <- readBindings
                 return bb
         return $ MkQBindings $ b : (fromMaybe [] mbb)) <|>
    (do
         readWS
         return $ MkQBindings [])

readInfix ::
       forall baseedit. HasPinaforeTableEdit baseedit
    => Parser (QValueExpr baseedit)
readInfix =
    (do
         readCharAndWS '$'
         return $ pure $ toQValue $ qapply @baseedit) <|>
    (do
         readCharAndWS '.'
         return $ pure $ toQValue $ qcombine @baseedit) <|>
    (do
         readCharAndWS '&'
         return $ pure $ toQValue $ qmeet @baseedit) <|>
    (do
         readCharAndWS '|'
         return $ pure $ toQValue $ qjoin @baseedit) <|>
    (do
         readStringAndWS "++"
         return $ pure $ toQValue $ qappend @baseedit) <?>
    "infix operator"

readExpression :: HasPinaforeTableEdit baseedit => Parser (QValueExpr baseedit)
readExpression =
    (do
         readCharAndWS '\\'
         args <- many readPattern
         readStringAndWS "->"
         val <- readExpression
         return $ exprAbstracts args val) <|>
    (do
         readKeyword KWLet
         bindings <- readBindings
         readKeyword KWIn
         body <- readExpression
         case getDuplicates bindings of
             [] -> return ()
             l -> parserFail $ "duplicate bindings: " ++ intercalate ", " (fmap show l)
         return $ qlets bindings body) <|>
    (do
         e1 <- readExpression1
         mfe2 <-
             optional $ do
                 f <- readInfix
                 e2 <- readExpression
                 return (f, e2)
         case mfe2 of
             Just (f, e2) -> return $ exprApplyAll f [e1, e2]
             Nothing -> return e1)

readExpression1 :: HasPinaforeTableEdit baseedit => Parser (QValueExpr baseedit)
readExpression1 = do
    e1 <- readSingleExpression
    args <- many readSingleExpression
    return $ exprApplyAll e1 args

mpure :: Alternative m => Maybe a -> m a
mpure (Just a) = pure a
mpure Nothing = empty

uuidChar :: Char -> Bool
uuidChar '-' = True
uuidChar c = isHexDigit c

readUUID :: Parser UUID
readUUID = do
    uuid <- some $ satisfy uuidChar
    readWS
    mpure $ Data.UUID.fromString uuid

readNumber :: Parser Integer
readNumber = let
    readDigit :: Parser Int
    readDigit = do
        c <- satisfy isDigit
        return $ digitToInt c
    assembleDigits :: [Int] -> Integer -> Integer
    assembleDigits [] i = i
    assembleDigits (d:dd) i = assembleDigits dd $ i * 10 + fromIntegral d
    readDigits :: Parser Integer
    readDigits = do
        digits <- some readDigit
        readWS
        return $ assembleDigits digits 0
    in do
           _ <- char '-'
           n <- readDigits
           return $ negate n
    <|> readDigits <?> "number"

readSingleExpression ::
       forall baseedit. HasPinaforeTableEdit baseedit
    => Parser (QValueExpr baseedit)
readSingleExpression =
    (do
         b <- readKeyword KWBool
         return $ pure $ toQValue b) <|>
    (do
         name <- readSymbol
         return $ qvar name) <|>
    (do
         n <- readNumber
         return $ pure $ toQValue n) <|>
    (do
         str <- readQuotedString
         return $ pure $ toQValue $ (pack str :: Text)) <|>
    (do
         readCharAndWS '('
         expr <- readExpression
         readCharAndWS ')'
         return $ expr) <|>
    (do
         readCharAndWS '['
         exprs <-
             (do
                  expr1 <- readExpression
                  exprs <-
                      many $ do
                          readCharAndWS ','
                          readExpression
                  return $ expr1 : exprs) <|>
             return []
         readCharAndWS ']'
         return $ fmap (MkAny QList) $ sequenceA exprs) <|>
    (do
         readCharAndWS '@'
         return $ pure $ toQValue $ qinvert @baseedit) <|>
    (do
         readCharAndWS '%'
         uuid <- readUUID
         return $ pure $ toQValue $ MkPredicate uuid) <|>
    (do
         readCharAndWS '!'
         uuid <- readUUID
         return $ pure $ toQValue $ MkPoint uuid) <?>
    "expression"

readExpressionText :: HasPinaforeTableEdit baseedit => Parser (QValueExpr baseedit)
readExpressionText = do
    readWS
    readExpression

parseExpression :: HasPinaforeTableEdit baseedit => SourceName -> Text -> Result Text (QValueExpr baseedit)
parseExpression name text =
    case parse readExpressionText name (unpack text) of
        Right a -> return a
        Left e -> fail $ show e
