module Pinafore.Language.Grammar.Read.Token
    ( Comment(..)
    , Token(..)
    , parseTokens
    ) where

import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.Name
import Shapes hiding (try)
import Shapes.Numeric
import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.String

data Comment
    = BlockComment String
    | LineComment String

commentWrite :: Comment -> String
commentWrite (BlockComment t) = "{#|" <> t <> "#}"
commentWrite (LineComment t) = "#|" <> t

data Token t where
    TokComment :: Token Comment
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
    TokRec :: Token ()
    TokLet :: Token ()
    TokIn :: Token ()
    TokDo :: Token ()
    TokCase :: Token ()
    TokOf :: Token ()
    TokEnd :: Token ()
    TokIf :: Token ()
    TokThen :: Token ()
    TokElse :: Token ()
    TokDataType :: Token ()
    TokOpenType :: Token ()
    TokSubtype :: Token ()
    TokClosedType :: Token ()
    TokDynamicType :: Token ()
    TokExpose :: Token ()
    TokImport :: Token ()
    TokUName :: Token Name
    TokQUName :: Token (NonEmpty Name, Name)
    TokLName :: Token Name
    TokQLName :: Token (ModuleName, Name)
    TokUnderscore :: Token ()
    TokLambda :: Token ()
    TokAssign :: Token ()
    TokMap :: Token ()
    TokBackMap :: Token ()
    TokAnchor :: Token Anchor
    TokAt :: Token ()
    TokOperator :: Token Name
    TokSubtypeOf :: Token ()
    TokOr :: Token ()
    TokAnd :: Token ()
    TokNumber :: Token Number

instance TestEquality Token where
    testEquality TokComment TokComment = Just Refl
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
    testEquality TokRec TokRec = Just Refl
    testEquality TokLet TokLet = Just Refl
    testEquality TokIn TokIn = Just Refl
    testEquality TokDo TokDo = Just Refl
    testEquality TokCase TokCase = Just Refl
    testEquality TokOf TokOf = Just Refl
    testEquality TokEnd TokEnd = Just Refl
    testEquality TokIf TokIf = Just Refl
    testEquality TokThen TokThen = Just Refl
    testEquality TokElse TokElse = Just Refl
    testEquality TokDataType TokDataType = Just Refl
    testEquality TokOpenType TokOpenType = Just Refl
    testEquality TokSubtype TokSubtype = Just Refl
    testEquality TokClosedType TokClosedType = Just Refl
    testEquality TokDynamicType TokDynamicType = Just Refl
    testEquality TokExpose TokExpose = Just Refl
    testEquality TokImport TokImport = Just Refl
    testEquality TokUName TokUName = Just Refl
    testEquality TokQUName TokQUName = Just Refl
    testEquality TokLName TokLName = Just Refl
    testEquality TokQLName TokQLName = Just Refl
    testEquality TokUnderscore TokUnderscore = Just Refl
    testEquality TokLambda TokLambda = Just Refl
    testEquality TokAssign TokAssign = Just Refl
    testEquality TokMap TokMap = Just Refl
    testEquality TokBackMap TokBackMap = Just Refl
    testEquality TokAnchor TokAnchor = Just Refl
    testEquality TokAt TokAt = Just Refl
    testEquality TokOperator TokOperator = Just Refl
    testEquality TokSubtypeOf TokSubtypeOf = Just Refl
    testEquality TokOr TokOr = Just Refl
    testEquality TokAnd TokAnd = Just Refl
    testEquality TokNumber TokNumber = Just Refl
    testEquality _ _ = Nothing

instance Show (Token t) where
    show TokComment = show ("comment" :: String)
    show TokSemicolon = show (";" :: String)
    show TokComma = show ("," :: String)
    show TokTypeJudge = show (":" :: String)
    show TokOpenParen = show ("(" :: String)
    show TokCloseParen = show (")" :: String)
    show TokOpenBracket = show ("[" :: String)
    show TokCloseBracket = show ("]" :: String)
    show TokOpenBrace = show ("{" :: String)
    show TokCloseBrace = show ("}" :: String)
    show TokString = "quoted string"
    show TokUnref = "unreference"
    show TokRec = show ("rec" :: String)
    show TokLet = show ("let" :: String)
    show TokIn = show ("in" :: String)
    show TokDo = show ("do" :: String)
    show TokCase = show ("case" :: String)
    show TokOf = show ("of" :: String)
    show TokEnd = show ("end" :: String)
    show TokIf = show ("if" :: String)
    show TokThen = show ("then" :: String)
    show TokElse = show ("else" :: String)
    show TokDataType = show ("datatype" :: String)
    show TokOpenType = show ("opentype" :: String)
    show TokSubtype = show ("subtype" :: String)
    show TokClosedType = show ("closedtype" :: String)
    show TokDynamicType = show ("dynamictype" :: String)
    show TokExpose = show ("expose" :: String)
    show TokImport = show ("import" :: String)
    show TokUName = "uname"
    show TokQUName = "qualified uname"
    show TokLName = "lname"
    show TokQLName = "qualified lname"
    show TokUnderscore = show ("_" :: String)
    show TokLambda = show ("\\" :: String)
    show TokAssign = show ("=" :: String)
    show TokMap = show ("=>" :: String)
    show TokBackMap = show ("<-" :: String)
    show TokAnchor = "anchor"
    show TokAt = show ("@" :: String)
    show TokOperator = "infix"
    show TokSubtypeOf = show ("<:" :: String)
    show TokOr = show ("|" :: String)
    show TokAnd = show ("&" :: String)
    show TokNumber = "number"

instance Show (AnyValue Token) where
    show (MkAnyValue t _) = show t

readChar :: Char -> Parser ()
readChar c = void $ char c

readWS :: Parser ()
readWS = do spaces <?> "white space"

readBlockComment :: Parser Comment
readBlockComment = let
    blockCommentOpen :: Parser ()
    blockCommentOpen =
        try $ do
            readChar '{'
            readChar '#'
    blockCommentInterior :: Parser String
    blockCommentInterior = (fmap commentWrite readBlockComment) <|> (endOfLine >> return "") <|> fmap pure anyToken
    blockCommentClose :: Parser ()
    blockCommentClose =
        try $ do
            readChar '#'
            readChar '}'
    in do
           blockCommentOpen
           s <- manyTill blockCommentInterior blockCommentClose
           return $ BlockComment $ mconcat s

readLineComment :: Parser Comment
readLineComment = let
    isLineBreak :: Char -> Bool
    isLineBreak '\n' = True
    isLineBreak '\r' = True
    isLineBreak _ = False
    in do
           readChar '#'
           s <- many (satisfy (\c -> not (isLineBreak c)))
           void endOfLine
           return $ LineComment s

readComment :: Parser Comment
readComment = readBlockComment <|> readLineComment

readEscapedChar :: Parser Char
readEscapedChar = do
    readChar '\\'
    c <- anyToken
    case c of
        'n' -> return '\n'
        't' -> return '\t'
        'r' -> return '\r'
        'f' -> return '\f'
        _ -> return c

readQuotedString :: Parser Text
readQuotedString = do
    readChar '"'
    s <- many readQuotedChar
    readChar '"'
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

readQName :: Parser ([Name], Bool, String)
readQName = do
    firstC <- satisfy identifierFirstChar
    rest <- many $ satisfy identifierChar
    let name = firstC : rest
    if isUpper firstC
        then (do
                  readChar '.'
                  (qual, u, n) <- readQName
                  return (MkName (pack name) : qual, u, n)) <|>
             return ([], True, name)
        else return ([], False, name)

checkKeyword :: String -> Maybe (AnyValue Token)
checkKeyword "_" = return $ MkAnyValue TokUnderscore ()
checkKeyword "rec" = return $ MkAnyValue TokRec ()
checkKeyword "let" = return $ MkAnyValue TokLet ()
checkKeyword "in" = return $ MkAnyValue TokIn ()
checkKeyword "do" = return $ MkAnyValue TokDo ()
checkKeyword "case" = return $ MkAnyValue TokCase ()
checkKeyword "of" = return $ MkAnyValue TokOf ()
checkKeyword "end" = return $ MkAnyValue TokEnd ()
checkKeyword "if" = return $ MkAnyValue TokIf ()
checkKeyword "then" = return $ MkAnyValue TokThen ()
checkKeyword "else" = return $ MkAnyValue TokElse ()
checkKeyword "datatype" = return $ MkAnyValue TokDataType ()
checkKeyword "opentype" = return $ MkAnyValue TokOpenType ()
checkKeyword "subtype" = return $ MkAnyValue TokSubtype ()
checkKeyword "closedtype" = return $ MkAnyValue TokClosedType ()
checkKeyword "dynamictype" = return $ MkAnyValue TokDynamicType ()
checkKeyword "expose" = return $ MkAnyValue TokExpose ()
checkKeyword "import" = return $ MkAnyValue TokImport ()
checkKeyword _ = Nothing

readTextToken :: Parser (AnyValue Token)
readTextToken = do
    (qual, u, name) <- readQName
    case nonEmpty qual of
        Nothing ->
            return $
            case checkKeyword name of
                Just tok -> tok
                Nothing ->
                    if u
                        then MkAnyValue TokUName $ MkName $ pack name
                        else MkAnyValue TokLName $ MkName $ pack name
        Just qualnames ->
            case checkKeyword name of
                Just _ -> empty
                Nothing ->
                    return $
                    if u
                        then MkAnyValue TokQUName (qualnames, MkName $ pack name)
                        else MkAnyValue TokQLName (MkModuleName qualnames, MkName $ pack name)

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
        ":" -> return $ MkAnyValue TokTypeJudge ()
        "\\" -> return $ MkAnyValue TokLambda ()
        "=" -> return $ MkAnyValue TokAssign ()
        "=>" -> return $ MkAnyValue TokMap ()
        "<-" -> return $ MkAnyValue TokBackMap ()
        "%" -> return $ MkAnyValue TokUnref ()
        "!" ->
            (do
                 anchor <- readHexAnchor
                 return $ MkAnyValue TokAnchor anchor) <|>
            (do
                 s <- readQuotedString
                 return $ MkAnyValue TokAnchor $ codeAnchor s)
        "@" -> return $ MkAnyValue TokAt ()
        "|" -> return $ MkAnyValue TokOr ()
        "&" -> return $ MkAnyValue TokAnd ()
        "<:" -> return $ MkAnyValue TokSubtypeOf ()
        _ -> return $ MkAnyValue TokOperator $ MkName $ pack name

readCharTok :: Char -> Token () -> Parser (AnyValue Token)
readCharTok c tok = do
    readChar c
    return $ MkAnyValue tok ()

readToken :: Parser ((SourcePos, AnyValue Token))
readToken = do
    pos <- getPosition
    t <-
        fmap (MkAnyValue TokComment) readComment <|> readCharTok ';' TokSemicolon <|> readCharTok ',' TokComma <|>
        readCharTok '(' TokOpenParen <|>
        readCharTok ')' TokCloseParen <|>
        readCharTok '[' TokOpenBracket <|>
        readCharTok ']' TokCloseBracket <|>
        readCharTok '{' TokOpenBrace <|>
        readCharTok '}' TokCloseBrace <|>
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
        Left e -> throwErrorMessage $ parseErrorMessage e
