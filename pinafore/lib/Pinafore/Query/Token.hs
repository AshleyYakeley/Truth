module Pinafore.Query.Token
    ( Token(..)
    , parseTokens
    ) where

import Data.UUID
import Pinafore.Number
import Pinafore.Query.Expression
import Pinafore.Table
import Shapes hiding (try)
import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.String

data Token t where
    TokSemicolon :: Token ()
    TokComma :: Token ()
    TokOpenParen :: Token ()
    TokCloseParen :: Token ()
    TokOpenBracket :: Token ()
    TokCloseBracket :: Token ()
    TokString :: Token Text
    TokLet :: Token ()
    TokIn :: Token ()
    TokIf :: Token ()
    TokThen :: Token ()
    TokElse :: Token ()
    TokBool :: Token Bool
    TokSymbol :: Token Symbol
    TokLambda :: Token ()
    TokAssign :: Token ()
    TokMap :: Token ()
    TokPoint :: Token Point
    TokPredicate :: Token Predicate
    TokInvert :: Token ()
    TokInfix :: Token Symbol
    TokNumber :: Token Number

instance TestEquality Token where
    testEquality TokSemicolon TokSemicolon = Just Refl
    testEquality TokComma TokComma = Just Refl
    testEquality TokOpenParen TokOpenParen = Just Refl
    testEquality TokCloseParen TokCloseParen = Just Refl
    testEquality TokOpenBracket TokOpenBracket = Just Refl
    testEquality TokCloseBracket TokCloseBracket = Just Refl
    testEquality TokString TokString = Just Refl
    testEquality TokLet TokLet = Just Refl
    testEquality TokIn TokIn = Just Refl
    testEquality TokIf TokIf = Just Refl
    testEquality TokThen TokThen = Just Refl
    testEquality TokElse TokElse = Just Refl
    testEquality TokBool TokBool = Just Refl
    testEquality TokSymbol TokSymbol = Just Refl
    testEquality TokLambda TokLambda = Just Refl
    testEquality TokAssign TokAssign = Just Refl
    testEquality TokMap TokMap = Just Refl
    testEquality TokPoint TokPoint = Just Refl
    testEquality TokPredicate TokPredicate = Just Refl
    testEquality TokInvert TokInvert = Just Refl
    testEquality TokInfix TokInfix = Just Refl
    testEquality TokNumber TokNumber = Just Refl
    testEquality _ _ = Nothing

instance Show (Token t) where
    show TokSemicolon = ";"
    show TokComma = ","
    show TokOpenParen = "("
    show TokCloseParen = ")"
    show TokOpenBracket = "["
    show TokCloseBracket = "]"
    show TokString = "quoted string"
    show TokLet = show ("let" :: String)
    show TokIn = show ("in" :: String)
    show TokIf = show ("if" :: String)
    show TokThen = show ("then" :: String)
    show TokElse = show ("else" :: String)
    show TokBool = "boolean constant"
    show TokSymbol = "symbol"
    show TokLambda = "\\"
    show TokAssign = "="
    show TokMap = "->"
    show TokPoint = "!"
    show TokPredicate = "%"
    show TokInvert = "@"
    show TokInfix = "infix"
    show TokNumber = "number"

instance Show (Any Token) where
    show (MkAny t _) = show t

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

readQuotedString :: Parser (Any Token)
readQuotedString = do
    _ <- char '"'
    s <- many readQuotedChar
    _ <- char '"'
    return $ MkAny TokString $ pack s
  where
    readQuotedChar :: Parser Char
    readQuotedChar = readEscapedChar <|> (satisfy ('"' /=))

identifierChar :: Char -> Bool
identifierChar '-' = True
identifierChar '_' = True
identifierChar c = isAlphaNum c

readNumber :: Parser (Any Token)
readNumber =
    fmap (MkAny TokNumber) $
    (try $ do
         _ <- string "NaN"
         return $ InexactNumber $ 0 / 0) <|>
    (try $ do
         _ <- string "~Infinity"
         return $ InexactNumber $ 1 / 0) <|>
    (try $ do
         _ <- string "~-Infinity"
         return $ InexactNumber $ -1 / 0) <|>
    (try $ do
         text <- many1 $ satisfy $ \c -> elem c ("0123456789-.e_~" :: String)
         case readMaybe text of
             Just n -> return $ n
             Nothing -> empty)

readTextToken :: Parser (Any Token)
readTextToken = do
    firstC <- satisfy isAlpha
    rest <- many $ satisfy identifierChar
    case firstC : rest of
        -- keywords
        "let" -> return $ MkAny TokLet ()
        "in" -> return $ MkAny TokIn ()
        "if" -> return $ MkAny TokIf ()
        "then" -> return $ MkAny TokThen ()
        "else" -> return $ MkAny TokElse ()
        "true" -> return $ MkAny TokBool True
        "false" -> return $ MkAny TokBool False
        name -> return $ MkAny TokSymbol $ MkSymbol $ pack name

uuidChar :: Char -> Bool
uuidChar '-' = True
uuidChar c = isHexDigit c

mpure :: Alternative m => Maybe a -> m a
mpure (Just a) = pure a
mpure Nothing = empty

readUUID :: Parser UUID
readUUID = do
    uuid <- some $ satisfy uuidChar
    mpure $ Data.UUID.fromString uuid

readOpToken :: Parser (Any Token)
readOpToken = do
    name <-
        many1 $
        satisfy $ \c ->
            elem c ("!$%&*+./<=>?@\\^|-~:" :: String) || (not (isAscii c) && (isSymbol c || isPunctuation c))
    case name of
        "\\" -> return $ MkAny TokLambda ()
        "=" -> return $ MkAny TokAssign ()
        "->" -> return $ MkAny TokMap ()
        "!" -> do
            uuid <- readUUID
            return $ MkAny TokPoint $ MkPoint uuid
        "%" -> do
            uuid <- readUUID
            return $ MkAny TokPredicate $ MkPredicate uuid
        "@" -> return $ MkAny TokInvert ()
        _ -> return $ MkAny TokInfix $ MkSymbol $ pack name

readChar :: Char -> Token () -> Parser (Any Token)
readChar c tok = do
    _ <- char c
    return $ MkAny tok ()

readToken :: Parser ((SourcePos, Any Token))
readToken = do
    pos <- getPosition
    t <-
        readChar ';' TokSemicolon <|> readChar ',' TokComma <|> readChar '(' TokOpenParen <|> readChar ')' TokCloseParen <|>
        readChar '[' TokOpenBracket <|>
        readChar ']' TokCloseBracket <|>
        try readNumber <|>
        readQuotedString <|>
        readTextToken <|>
        readOpToken
    readWS
    return (pos, t)

readTokens :: Parser [(SourcePos, Any Token)]
readTokens = do
    readWS
    many readToken

parseTokens :: SourceName -> Text -> Result Text [(SourcePos, Any Token)]
parseTokens name text =
    case parse readTokens name (unpack text) of
        Right a -> return a
        Left e -> fail $ show e
