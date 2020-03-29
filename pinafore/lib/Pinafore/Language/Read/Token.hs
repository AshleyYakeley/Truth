module Pinafore.Language.Read.Token
    ( Token(..)
    , parseTokens
    ) where

import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.Name
import Shapes hiding (try)
import Shapes.Numeric
import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.String

data Token t where
    TokSemicolon :: Token ()
    TokComma :: Token ()
    TokTypeJudge :: Token ()
    TokOpenParen :: Token ()
    TokCloseParen :: Token ()
    TokOpenBracket :: Token ()
    TokCloseBracket :: Token ()
    TokOpenBrace :: Token ()
    TokCloseBrace :: Token ()
    TokString :: Token Text
    TokUnref :: Token ()
    TokLet :: Token ()
    TokIn :: Token ()
    TokDo :: Token ()
    TokCase :: Token ()
    TokOf :: Token ()
    TokEnd :: Token ()
    TokIf :: Token ()
    TokThen :: Token ()
    TokElse :: Token ()
    TokOpenType :: Token ()
    TokSubtype :: Token ()
    TokClosedType :: Token ()
    TokUName :: Token Name
    TokLName :: Token Name
    TokUnderscore :: Token ()
    TokLambda :: Token ()
    TokAssign :: Token ()
    TokMap :: Token ()
    TokBackMap :: Token ()
    TokPropMap :: Token ()
    TokProperty :: Token ()
    TokEntity :: Token ()
    TokAnchor :: Token Anchor
    TokAt :: Token ()
    TokOperator :: Token Name
    TokNumber :: Token Number

instance TestEquality Token where
    testEquality TokSemicolon TokSemicolon = Just Refl
    testEquality TokComma TokComma = Just Refl
    testEquality TokTypeJudge TokTypeJudge = Just Refl
    testEquality TokOpenParen TokOpenParen = Just Refl
    testEquality TokCloseParen TokCloseParen = Just Refl
    testEquality TokOpenBracket TokOpenBracket = Just Refl
    testEquality TokCloseBracket TokCloseBracket = Just Refl
    testEquality TokOpenBrace TokOpenBrace = Just Refl
    testEquality TokCloseBrace TokCloseBrace = Just Refl
    testEquality TokString TokString = Just Refl
    testEquality TokUnref TokUnref = Just Refl
    testEquality TokLet TokLet = Just Refl
    testEquality TokIn TokIn = Just Refl
    testEquality TokDo TokDo = Just Refl
    testEquality TokCase TokCase = Just Refl
    testEquality TokOf TokOf = Just Refl
    testEquality TokEnd TokEnd = Just Refl
    testEquality TokIf TokIf = Just Refl
    testEquality TokThen TokThen = Just Refl
    testEquality TokElse TokElse = Just Refl
    testEquality TokOpenType TokOpenType = Just Refl
    testEquality TokSubtype TokSubtype = Just Refl
    testEquality TokClosedType TokClosedType = Just Refl
    testEquality TokUName TokUName = Just Refl
    testEquality TokLName TokLName = Just Refl
    testEquality TokUnderscore TokUnderscore = Just Refl
    testEquality TokLambda TokLambda = Just Refl
    testEquality TokAssign TokAssign = Just Refl
    testEquality TokMap TokMap = Just Refl
    testEquality TokBackMap TokBackMap = Just Refl
    testEquality TokPropMap TokPropMap = Just Refl
    testEquality TokProperty TokProperty = Just Refl
    testEquality TokEntity TokEntity = Just Refl
    testEquality TokAnchor TokAnchor = Just Refl
    testEquality TokAt TokAt = Just Refl
    testEquality TokOperator TokOperator = Just Refl
    testEquality TokNumber TokNumber = Just Refl
    testEquality _ _ = Nothing

instance Show (Token t) where
    show TokSemicolon = show (";" :: String)
    show TokComma = show ("," :: String)
    show TokTypeJudge = show ("::" :: String)
    show TokOpenParen = show ("(" :: String)
    show TokCloseParen = show (")" :: String)
    show TokOpenBracket = show ("[" :: String)
    show TokCloseBracket = show ("]" :: String)
    show TokOpenBrace = show ("{" :: String)
    show TokCloseBrace = show ("}" :: String)
    show TokString = "quoted string"
    show TokUnref = "unreference"
    show TokLet = show ("let" :: String)
    show TokIn = show ("in" :: String)
    show TokDo = show ("do" :: String)
    show TokCase = show ("case" :: String)
    show TokOf = show ("of" :: String)
    show TokEnd = show ("end" :: String)
    show TokIf = show ("if" :: String)
    show TokThen = show ("then" :: String)
    show TokElse = show ("else" :: String)
    show TokOpenType = show ("opentype" :: String)
    show TokSubtype = show ("subtype" :: String)
    show TokClosedType = show ("closedtype" :: String)
    show TokUName = "uname"
    show TokLName = "lname"
    show TokUnderscore = show ("_" :: String)
    show TokLambda = show ("\\" :: String)
    show TokAssign = show ("=" :: String)
    show TokMap = show ("->" :: String)
    show TokBackMap = show ("<-" :: String)
    show TokPropMap = show ("~>" :: String)
    show TokProperty = show ("property" :: String)
    show TokEntity = show ("entity" :: String)
    show TokAnchor = "anchor"
    show TokAt = show ("@" :: String)
    show TokOperator = "infix"
    show TokNumber = "number"

instance Show (AnyValue Token) where
    show (MkAnyValue t _) = show t

readWS :: Parser ()
readWS = do
    spaces
    void $
        optional
            (do
                 comment
                 readWS)
    return ()
    <?> "white space"
  where
    char1 :: Char -> Parser ()
    char1 c = do
        void $ char c
        return ()
    isLineBreak :: Char -> Bool
    isLineBreak '\n' = True
    isLineBreak '\r' = True
    isLineBreak _ = False
    blockCommentOpen :: Parser ()
    blockCommentOpen =
        try $ do
            char1 '{'
            char1 '#'
    blockCommentClose :: Parser ()
    blockCommentClose =
        try $ do
            char1 '#'
            char1 '}'
    lineComment :: Parser ()
    lineComment = do
        char1 '#'
        void $ many (satisfy (\c -> not (isLineBreak c)))
        void endOfLine
    blockComment :: Parser ()
    blockComment = do
        blockCommentOpen
        void $ manyTill (blockComment <|> void endOfLine <|> void anyToken) blockCommentClose
    comment :: Parser ()
    comment = blockComment <|> lineComment

readEscapedChar :: Parser Char
readEscapedChar = do
    void $ char '\\'
    c <- anyToken
    case c of
        'n' -> return '\n'
        't' -> return '\t'
        'r' -> return '\r'
        'f' -> return '\f'
        _ -> return c

readQuotedString :: Parser Text
readQuotedString = do
    void $ char '"'
    s <- many readQuotedChar
    void $ char '"'
    return $ pack s
  where
    readQuotedChar :: Parser Char
    readQuotedChar = readEscapedChar <|> (satisfy ('"' /=))

identifierFirstChar :: Char -> Bool
identifierFirstChar '_' = True
identifierFirstChar c = isAlpha c

identifierChar :: Char -> Bool
identifierChar '-' = True
identifierChar '_' = True
identifierChar c = isAlphaNum c

readNumber :: Parser (AnyValue Token)
readNumber =
    fmap (MkAnyValue TokNumber) $
    (try $ do
         void $ string "NaN"
         return $ InexactNumber $ 0 / 0) <|>
    (try $ do
         void $ string "~Infinity"
         return $ InexactNumber $ 1 / 0) <|>
    (try $ do
         void $ string "~-Infinity"
         return $ InexactNumber $ -1 / 0) <|>
    (try $ do
         text <- many1 $ satisfy $ \c -> elem c ("0123456789-.e_~" :: String)
         mpure $ readNumberLiteral text)

readTextToken :: Parser (AnyValue Token)
readTextToken = do
    firstC <- satisfy identifierFirstChar
    rest <- many $ satisfy identifierChar
    case firstC : rest of
        -- keywords
        "_" -> return $ MkAnyValue TokUnderscore ()
        "let" -> return $ MkAnyValue TokLet ()
        "in" -> return $ MkAnyValue TokIn ()
        "do" -> return $ MkAnyValue TokDo ()
        "case" -> return $ MkAnyValue TokCase ()
        "of" -> return $ MkAnyValue TokOf ()
        "end" -> return $ MkAnyValue TokEnd ()
        "property" -> return $ MkAnyValue TokProperty ()
        "entity" -> return $ MkAnyValue TokEntity ()
        "if" -> return $ MkAnyValue TokIf ()
        "then" -> return $ MkAnyValue TokThen ()
        "else" -> return $ MkAnyValue TokElse ()
        "opentype" -> return $ MkAnyValue TokOpenType ()
        "subtype" -> return $ MkAnyValue TokSubtype ()
        "closedtype" -> return $ MkAnyValue TokClosedType ()
        name
            | isUpper firstC -> return $ MkAnyValue TokUName $ MkName $ pack name
        name -> return $ MkAnyValue TokLName $ MkName $ pack name

toHexDigit :: Char -> Maybe Word8
toHexDigit c =
    if isHexDigit c
        then Just $ fromIntegral $ digitToInt c
        else Nothing

fromHex :: [Char] -> Maybe [Word8]
fromHex [] = Just []
fromHex (chi:clo:cc) = do
    whi <- toHexDigit chi
    wlo <- toHexDigit clo
    ww <- fromHex cc
    return $ (whi * 16 + wlo) : ww
fromHex [_] = Nothing

hexAnchorChar :: Char -> Bool
hexAnchorChar '-' = True
hexAnchorChar c = isHexDigit c

readHexAnchor :: Parser Anchor
readHexAnchor = do
    cs <- some $ satisfy hexAnchorChar
    mpure $ do
        octets <- fromHex $ filter isHexDigit cs
        decode anchorCodec $ fromList octets

readOpToken :: Parser (AnyValue Token)
readOpToken = do
    name <-
        many1 $
        satisfy $ \c ->
            elem c ("!$%&*+./<=>?@\\^|-~:" :: String) || (not (isAscii c) && (isSymbol c || isPunctuation c))
    case name of
        "::" -> return $ MkAnyValue TokTypeJudge ()
        "\\" -> return $ MkAnyValue TokLambda ()
        "=" -> return $ MkAnyValue TokAssign ()
        "->" -> return $ MkAnyValue TokMap ()
        "<-" -> return $ MkAnyValue TokBackMap ()
        "~>" -> return $ MkAnyValue TokPropMap ()
        "%" -> return $ MkAnyValue TokUnref ()
        "!" ->
            (do
                 anchor <- readHexAnchor
                 return $ MkAnyValue TokAnchor anchor) <|>
            (do
                 s <- readQuotedString
                 return $ MkAnyValue TokAnchor $ codeAnchor s)
        "@" -> return $ MkAnyValue TokAt ()
        _ -> return $ MkAnyValue TokOperator $ MkName $ pack name

readChar :: Char -> Token () -> Parser (AnyValue Token)
readChar c tok = do
    void $ char c
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
        fmap (MkAnyValue TokString) readQuotedString <|>
        readTextToken <|>
        readOpToken
    readWS
    return (pos, t)

readTokens :: SourcePos -> Parser (SourcePos, [(SourcePos, AnyValue Token)])
readTokens oldpos = do
    setPosition oldpos
    readWS
    toks <- many readToken
    eof
    newpos <- getPosition
    return (newpos, toks)

parseTokens :: Text -> StateT SourcePos InterpretResult [(SourcePos, AnyValue Token)]
parseTokens text = do
    oldpos <- get
    case parse (readTokens oldpos) (sourceName oldpos) (unpack text) of
        Right (newpos, a) -> do
            put newpos
            return a
        Left e -> throwError [parseErrorMessage e]
