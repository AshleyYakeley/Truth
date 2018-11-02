module Pinafore.Language.Token
    ( Token(..)
    , parseTokens
    ) where

import Data.UUID
import Pinafore.Base
import Pinafore.Language.Name

--import Pinafore.Storage.Table
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
    TokOpenBrace :: Token ()
    TokCloseBrace :: Token ()
    TokString :: Token Text
    TokLet :: Token ()
    TokIn :: Token ()
    TokIf :: Token ()
    TokThen :: Token ()
    TokElse :: Token ()
    TokEntity :: Token ()
    TokSubtype :: Token ()
    TokBool :: Token Bool
    TokName :: Token Name
    TokLambda :: Token ()
    TokAssign :: Token ()
    TokMap :: Token ()
    TokPropMap :: Token ()
    TokProperty :: Token ()
    TokPoint :: Token ()
    TokUUID :: Token UUID
    TokAt :: Token ()
    TokOperator :: Token Name
    TokNumber :: Token Number

instance TestEquality Token where
    testEquality TokSemicolon TokSemicolon = Just Refl
    testEquality TokComma TokComma = Just Refl
    testEquality TokOpenParen TokOpenParen = Just Refl
    testEquality TokCloseParen TokCloseParen = Just Refl
    testEquality TokOpenBracket TokOpenBracket = Just Refl
    testEquality TokCloseBracket TokCloseBracket = Just Refl
    testEquality TokOpenBrace TokOpenBrace = Just Refl
    testEquality TokCloseBrace TokCloseBrace = Just Refl
    testEquality TokString TokString = Just Refl
    testEquality TokLet TokLet = Just Refl
    testEquality TokIn TokIn = Just Refl
    testEquality TokIf TokIf = Just Refl
    testEquality TokThen TokThen = Just Refl
    testEquality TokElse TokElse = Just Refl
    testEquality TokEntity TokEntity = Just Refl
    testEquality TokSubtype TokSubtype = Just Refl
    testEquality TokBool TokBool = Just Refl
    testEquality TokName TokName = Just Refl
    testEquality TokLambda TokLambda = Just Refl
    testEquality TokAssign TokAssign = Just Refl
    testEquality TokMap TokMap = Just Refl
    testEquality TokPropMap TokPropMap = Just Refl
    testEquality TokProperty TokProperty = Just Refl
    testEquality TokPoint TokPoint = Just Refl
    testEquality TokUUID TokUUID = Just Refl
    testEquality TokAt TokAt = Just Refl
    testEquality TokOperator TokOperator = Just Refl
    testEquality TokNumber TokNumber = Just Refl
    testEquality _ _ = Nothing

instance Show (Token t) where
    show TokSemicolon = ";"
    show TokComma = ","
    show TokOpenParen = "("
    show TokCloseParen = ")"
    show TokOpenBracket = "["
    show TokCloseBracket = "]"
    show TokOpenBrace = "{"
    show TokCloseBrace = "}"
    show TokString = "quoted string"
    show TokLet = show ("let" :: String)
    show TokIn = show ("in" :: String)
    show TokIf = show ("if" :: String)
    show TokThen = show ("then" :: String)
    show TokElse = show ("else" :: String)
    show TokEntity = show ("entity" :: String)
    show TokSubtype = show ("subtype" :: String)
    show TokBool = "boolean constant"
    show TokName = "name"
    show TokLambda = "\\"
    show TokAssign = "="
    show TokMap = "->"
    show TokPropMap = "~>"
    show TokProperty = "property"
    show TokPoint = "point"
    show TokUUID = "%"
    show TokAt = "@"
    show TokOperator = "infix"
    show TokNumber = "number"

instance Show (AnyValue Token) where
    show (MkAnyValue t _) = show t

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

readQuotedString :: Parser (AnyValue Token)
readQuotedString = do
    _ <- char '"'
    s <- many readQuotedChar
    _ <- char '"'
    return $ MkAnyValue TokString $ pack s
  where
    readQuotedChar :: Parser Char
    readQuotedChar = readEscapedChar <|> (satisfy ('"' /=))

identifierChar :: Char -> Bool
identifierChar '-' = True
identifierChar '_' = True
identifierChar c = isAlphaNum c

readNumber :: Parser (AnyValue Token)
readNumber =
    fmap (MkAnyValue TokNumber) $
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

readTextToken :: Parser (AnyValue Token)
readTextToken = do
    firstC <- satisfy isAlpha
    rest <- many $ satisfy identifierChar
    case firstC : rest of
        -- keywords
        "let" -> return $ MkAnyValue TokLet ()
        "in" -> return $ MkAnyValue TokIn ()
        "property" -> return $ MkAnyValue TokProperty ()
        "point" -> return $ MkAnyValue TokPoint ()
        "if" -> return $ MkAnyValue TokIf ()
        "then" -> return $ MkAnyValue TokThen ()
        "else" -> return $ MkAnyValue TokElse ()
        "entity" -> return $ MkAnyValue TokEntity ()
        "subtype" -> return $ MkAnyValue TokSubtype ()
        "true" -> return $ MkAnyValue TokBool True
        "false" -> return $ MkAnyValue TokBool False
        name -> return $ MkAnyValue TokName $ MkName $ pack name

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

readOpToken :: Parser (AnyValue Token)
readOpToken = do
    name <-
        many1 $
        satisfy $ \c ->
            elem c ("!$%&*+./<=>?@\\^|-~:" :: String) || (not (isAscii c) && (isSymbol c || isPunctuation c))
    case name of
        "\\" -> return $ MkAnyValue TokLambda ()
        "=" -> return $ MkAnyValue TokAssign ()
        "->" -> return $ MkAnyValue TokMap ()
        "~>" -> return $ MkAnyValue TokPropMap ()
        "%" -> do
            uuid <- readUUID
            return $ MkAnyValue TokUUID uuid
        "@" -> return $ MkAnyValue TokAt ()
        _ -> return $ MkAnyValue TokOperator $ MkName $ pack name

readChar :: Char -> Token () -> Parser (AnyValue Token)
readChar c tok = do
    _ <- char c
    return $ MkAnyValue tok ()

readToken :: Parser ((SourcePos, AnyValue Token))
readToken = do
    pos <- getPosition
    t <-
        readChar ';' TokSemicolon <|> readChar ',' TokComma <|> readChar '(' TokOpenParen <|> readChar ')' TokCloseParen <|>
        readChar '[' TokOpenBracket <|>
        readChar ']' TokCloseBracket <|>
        readChar '{' TokOpenBrace <|>
        readChar '}' TokCloseBrace <|>
        try readNumber <|>
        readQuotedString <|>
        readTextToken <|>
        readOpToken
    readWS
    return (pos, t)

readTokens :: Parser [(SourcePos, AnyValue Token)]
readTokens = do
    readWS
    many readToken

parseTokens :: SourceName -> Text -> Result Text [(SourcePos, AnyValue Token)]
parseTokens name text =
    case parse readTokens name (unpack text) of
        Right a -> return a
        Left e -> fail $ show e
