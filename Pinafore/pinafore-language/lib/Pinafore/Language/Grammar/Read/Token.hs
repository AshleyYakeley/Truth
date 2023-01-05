module Pinafore.Language.Grammar.Read.Token
    ( Comment(..)
    , TokenNames(..)
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

instance Show Comment where
    show (BlockComment t) = "{#" <> t <> "#}"
    show (LineComment t) = "#" <> t

data TokenNames = MkTokenNames
    { tnAbsolute :: Bool
    , tnName :: Name
    , tnSpace :: [Name] -- always upper
    }

data Token t where
    TokComment :: Token Comment
    TokSemicolon :: Token ()
    TokComma :: Token ()
    TokTypeJudge :: Token ()
    TokTypeDynamic :: Token ()
    TokOpenParen :: Token ()
    TokCloseParen :: Token ()
    TokOpenBracket :: Token ()
    TokCloseBracket :: Token ()
    TokOpenBrace :: Token ()
    TokCloseBrace :: Token ()
    TokString :: Token Text
    TokUnquote :: Token ()
    TokRec :: Token ()
    TokLet :: Token ()
    TokIn :: Token ()
    TokDo :: Token ()
    TokOf :: Token ()
    TokEnd :: Token ()
    TokIf :: Token ()
    TokThen :: Token ()
    TokElse :: Token ()
    TokDataType :: Token ()
    TokOpenType :: Token ()
    TokSubtype :: Token ()
    TokTrustMe :: Token ()
    TokStorable :: Token ()
    TokDynamicType :: Token ()
    TokExpose :: Token ()
    TokImport :: Token ()
    TokAs :: Token ()
    TokNamespace :: Token ()
    TokUsing :: Token ()
    TokNamesUpper :: Token TokenNames
    TokNamesLower :: Token TokenNames
    TokUnderscore :: Token ()
    TokFn :: Token ()
    TokFns :: Token ()
    TokMatch :: Token ()
    TokMatches :: Token ()
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
    testEquality TokTypeDynamic TokTypeDynamic = Just Refl
    testEquality TokOpenParen TokOpenParen = Just Refl
    testEquality TokCloseParen TokCloseParen = Just Refl
    testEquality TokOpenBracket TokOpenBracket = Just Refl
    testEquality TokCloseBracket TokCloseBracket = Just Refl
    testEquality TokOpenBrace TokOpenBrace = Just Refl
    testEquality TokCloseBrace TokCloseBrace = Just Refl
    testEquality TokString TokString = Just Refl
    testEquality TokUnquote TokUnquote = Just Refl
    testEquality TokRec TokRec = Just Refl
    testEquality TokLet TokLet = Just Refl
    testEquality TokIn TokIn = Just Refl
    testEquality TokDo TokDo = Just Refl
    testEquality TokOf TokOf = Just Refl
    testEquality TokEnd TokEnd = Just Refl
    testEquality TokIf TokIf = Just Refl
    testEquality TokThen TokThen = Just Refl
    testEquality TokElse TokElse = Just Refl
    testEquality TokDataType TokDataType = Just Refl
    testEquality TokOpenType TokOpenType = Just Refl
    testEquality TokSubtype TokSubtype = Just Refl
    testEquality TokTrustMe TokTrustMe = Just Refl
    testEquality TokStorable TokStorable = Just Refl
    testEquality TokDynamicType TokDynamicType = Just Refl
    testEquality TokExpose TokExpose = Just Refl
    testEquality TokImport TokImport = Just Refl
    testEquality TokAs TokAs = Just Refl
    testEquality TokNamespace TokNamespace = Just Refl
    testEquality TokUsing TokUsing = Just Refl
    testEquality TokNamesUpper TokNamesUpper = Just Refl
    testEquality TokNamesLower TokNamesLower = Just Refl
    testEquality TokUnderscore TokUnderscore = Just Refl
    testEquality TokFn TokFn = Just Refl
    testEquality TokFns TokFns = Just Refl
    testEquality TokMatch TokMatch = Just Refl
    testEquality TokMatches TokMatches = Just Refl
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
    show TokTypeDynamic = show (":?" :: String)
    show TokOpenParen = show ("(" :: String)
    show TokCloseParen = show (")" :: String)
    show TokOpenBracket = show ("[" :: String)
    show TokCloseBracket = show ("]" :: String)
    show TokOpenBrace = show ("{" :: String)
    show TokCloseBrace = show ("}" :: String)
    show TokString = "quoted string"
    show TokUnquote = "unquote"
    show TokRec = show ("rec" :: String)
    show TokLet = show ("let" :: String)
    show TokIn = show ("in" :: String)
    show TokDo = show ("do" :: String)
    show TokOf = show ("of" :: String)
    show TokEnd = show ("end" :: String)
    show TokIf = show ("if" :: String)
    show TokThen = show ("then" :: String)
    show TokElse = show ("else" :: String)
    show TokDataType = show ("datatype" :: String)
    show TokOpenType = show ("opentype" :: String)
    show TokSubtype = show ("subtype" :: String)
    show TokTrustMe = show ("trustme" :: String)
    show TokStorable = show ("storable" :: String)
    show TokDynamicType = show ("dynamictype" :: String)
    show TokExpose = show ("expose" :: String)
    show TokImport = show ("import" :: String)
    show TokAs = show ("as" :: String)
    show TokNamespace = show ("namespace" :: String)
    show TokUsing = show ("using" :: String)
    show TokNamesUpper = "unames"
    show TokNamesLower = "lnames"
    show TokUnderscore = show ("_" :: String)
    show TokFn = show ("fn" :: String)
    show TokFns = show ("fns" :: String)
    show TokMatch = show ("match" :: String)
    show TokMatches = show ("matches" :: String)
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

instance Show (SomeOf Token) where
    show (MkSomeOf t _) = show t

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
    blockCommentInterior = (fmap show readBlockComment) <|> (endOfLine >> return "") <|> fmap pure anyToken
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

readNumber :: Parser (SomeOf Token)
readNumber =
    fmap (MkSomeOf TokNumber) $
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

readName :: Parser (Bool, Name)
readName = do
    firstC <- satisfy identifierFirstChar
    rest <- many $ satisfy identifierChar
    return (isUpper firstC, MkName $ pack $ firstC : rest)

readTokenNames :: Parser (Bool, TokenNames)
readTokenNames =
    try $ do
        (u, name) <- readName
        ns <-
            many $
            try $ do
                readChar '.'
                readName
        nspace <-
            for ns $ \(b, nsn) -> do
                altIf b
                return nsn
        mabs <- optional $ readChar '.'
        return (u, MkTokenNames {tnAbsolute = isJust mabs, tnSpace = nspace, tnName = name})

checkKeyword :: Text -> Maybe (SomeOf Token)
checkKeyword "_" = return $ MkSomeOf TokUnderscore ()
checkKeyword "fn" = return $ MkSomeOf TokFn ()
checkKeyword "fns" = return $ MkSomeOf TokFns ()
checkKeyword "match" = return $ MkSomeOf TokMatch ()
checkKeyword "matches" = return $ MkSomeOf TokMatches ()
checkKeyword "rec" = return $ MkSomeOf TokRec ()
checkKeyword "let" = return $ MkSomeOf TokLet ()
checkKeyword "in" = return $ MkSomeOf TokIn ()
checkKeyword "do" = return $ MkSomeOf TokDo ()
checkKeyword "of" = return $ MkSomeOf TokOf ()
checkKeyword "end" = return $ MkSomeOf TokEnd ()
checkKeyword "if" = return $ MkSomeOf TokIf ()
checkKeyword "then" = return $ MkSomeOf TokThen ()
checkKeyword "else" = return $ MkSomeOf TokElse ()
checkKeyword "datatype" = return $ MkSomeOf TokDataType ()
checkKeyword "opentype" = return $ MkSomeOf TokOpenType ()
checkKeyword "subtype" = return $ MkSomeOf TokSubtype ()
checkKeyword "trustme" = return $ MkSomeOf TokTrustMe ()
checkKeyword "storable" = return $ MkSomeOf TokStorable ()
checkKeyword "dynamictype" = return $ MkSomeOf TokDynamicType ()
checkKeyword "expose" = return $ MkSomeOf TokExpose ()
checkKeyword "import" = return $ MkSomeOf TokImport ()
checkKeyword "as" = return $ MkSomeOf TokAs ()
checkKeyword "namespace" = return $ MkSomeOf TokNamespace ()
checkKeyword "using" = return $ MkSomeOf TokUsing ()
checkKeyword _ = Nothing

readTextToken :: Parser (SomeOf Token)
readTextToken = do
    (u, tns) <- readTokenNames
    case checkKeyword $ toText $ tnName tns of
        Just stok ->
            case tns of
                MkTokenNames False _ [] -> return stok
                _ -> empty
        Nothing ->
            return $
            MkSomeOf
                (if u
                     then TokNamesUpper
                     else TokNamesLower)
                tns

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

readOpToken :: Parser (SomeOf Token)
readOpToken = do
    name <-
        many1 $
        satisfy $ \c ->
            elem c ("!$%&*+./<=>?@\\^|-~:" :: String) || (not (isAscii c) && (isSymbol c || isPunctuation c))
    case name of
        ":" -> return $ MkSomeOf TokTypeJudge ()
        ":?" -> return $ MkSomeOf TokTypeDynamic ()
        "=" -> return $ MkSomeOf TokAssign ()
        "=>" -> return $ MkSomeOf TokMap ()
        "<-" -> return $ MkSomeOf TokBackMap ()
        "%" -> return $ MkSomeOf TokUnquote ()
        "!" ->
            (do
                 anchor <- readHexAnchor
                 return $ MkSomeOf TokAnchor anchor) <|>
            (do
                 s <- readQuotedString
                 return $ MkSomeOf TokAnchor $ codeAnchor s)
        "@" -> return $ MkSomeOf TokAt ()
        "|" -> return $ MkSomeOf TokOr ()
        "&" -> return $ MkSomeOf TokAnd ()
        "<:" -> return $ MkSomeOf TokSubtypeOf ()
        _ -> return $ MkSomeOf TokOperator $ MkName $ pack name

readCharTok :: Char -> Token () -> Parser (SomeOf Token)
readCharTok c tok = do
    readChar c
    return $ MkSomeOf tok ()

readToken :: Parser ((SourcePos, SomeOf Token))
readToken = do
    pos <- getPosition
    t <-
        fmap (MkSomeOf TokComment) readComment <|> readCharTok ';' TokSemicolon <|> readCharTok ',' TokComma <|>
        readCharTok '(' TokOpenParen <|>
        readCharTok ')' TokCloseParen <|>
        readCharTok '[' TokOpenBracket <|>
        readCharTok ']' TokCloseBracket <|>
        readCharTok '{' TokOpenBrace <|>
        readCharTok '}' TokCloseBrace <|>
        try readNumber <|>
        fmap (MkSomeOf TokString) readQuotedString <|>
        readTextToken <|>
        readOpToken
    readWS
    return (pos, t)

readTokens :: SourcePos -> Parser (SourcePos, [(SourcePos, SomeOf Token)])
readTokens oldpos = do
    setPosition oldpos
    readWS
    toks <- many readToken
    eof
    newpos <- getPosition
    return (newpos, toks)

parseTokens :: Text -> StateT SourcePos InterpretResult [(SourcePos, SomeOf Token)]
parseTokens text = do
    oldpos <- get
    case parse (readTokens oldpos) (sourceName oldpos) (unpack text) of
        Right (newpos, a) -> do
            put newpos
            return a
        Left e -> throw $ parseErrorMessage e
