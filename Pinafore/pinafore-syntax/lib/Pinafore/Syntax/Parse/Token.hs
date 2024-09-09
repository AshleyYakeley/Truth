module Pinafore.Syntax.Parse.Token
    ( Comment(..)
    , TokenNames(..)
    , tokenNamesToFullNameRef
    , tokenNamesToSingleName
    , tokenNamesToNamespaceRef
    , Token(..)
    , parseTokens
    , allKeywords
    ) where

import Pinafore.Base
import Pinafore.Syntax.Name
import Shapes hiding (try)
import Shapes.Numeric
import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.String

debugSyntaxINTERNAL :: Bool
debugSyntaxINTERNAL = False

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
    } deriving (Eq)

instance Show TokenNames where
    show MkTokenNames {..} =
        show tnName <>
        concatmap (\n -> "." <> show n) tnSpace <>
        if tnAbsolute
            then "."
            else ""

instance IsString TokenNames where
    fromString s = let
        tnAbsolute = False
        tnName = fromString s
        tnSpace = []
        in MkTokenNames {..}

tokenNamesToFullNameRef :: TokenNames -> FullNameRef
tokenNamesToFullNameRef MkTokenNames {..} =
    MkFullNameRef tnName $
    (if tnAbsolute
         then AbsoluteNamespaceRef . MkNamespace
         else RelativeNamespaceRef)
        tnSpace

tokenNamesToSingleName :: TokenNames -> Maybe Name
tokenNamesToSingleName MkTokenNames {..} = do
    altIf $ not tnAbsolute
    altIf $ null tnSpace
    return tnName

tokenNamesToNamespaceRef :: TokenNames -> NamespaceRef
tokenNamesToNamespaceRef MkTokenNames {..} =
    (if tnAbsolute
         then AbsoluteNamespaceRef . MkNamespace
         else RelativeNamespaceRef) $
    [tnName] <> tnSpace

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
    TokImply :: Token ()
    TokAp :: Token ()
    TokDo :: Token ()
    TokIf :: Token ()
    TokThen :: Token ()
    TokElse :: Token ()
    TokType :: Token ()
    TokDataType :: Token ()
    TokEntityType :: Token ()
    TokPredicateType :: Token ()
    TokSubtype :: Token ()
    TokTrustMe :: Token ()
    TokStorable :: Token ()
    TokExpose :: Token ()
    TokImport :: Token ()
    TokAs :: Token ()
    TokExcept :: Token ()
    TokNamespace :: Token ()
    TokDocSec :: Token ()
    TokWith :: Token ()
    TokNamesUpper :: Token TokenNames
    TokNamesLower :: Token TokenNames
    TokImplicitName :: Token ImplicitName
    TokUnderscore :: Token ()
    TokFn :: Token ()
    TokAssign :: Token ()
    TokMap :: Token ()
    TokBackMap :: Token ()
    TokAnchor :: Token Anchor
    TokAt :: Token ()
    TokOperator :: Token TokenNames
    TokSubtypeOf :: Token ()
    TokOr :: Token ()
    TokAnd :: Token ()
    TokNumber :: Token Number
    TokDebug :: Token ()

showTokenContents :: Token a -> Maybe (a -> String)
showTokenContents TokComment = Just show
showTokenContents TokNamesUpper = Just show
showTokenContents TokNamesLower = Just show
showTokenContents TokAnchor = Just show
showTokenContents TokOperator = Just show
showTokenContents TokNumber = Just show
showTokenContents _ = Nothing

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
    testEquality TokImply TokImply = Just Refl
    testEquality TokAp TokAp = Just Refl
    testEquality TokDo TokDo = Just Refl
    testEquality TokIf TokIf = Just Refl
    testEquality TokThen TokThen = Just Refl
    testEquality TokElse TokElse = Just Refl
    testEquality TokType TokType = Just Refl
    testEquality TokDataType TokDataType = Just Refl
    testEquality TokEntityType TokEntityType = Just Refl
    testEquality TokPredicateType TokPredicateType = Just Refl
    testEquality TokSubtype TokSubtype = Just Refl
    testEquality TokTrustMe TokTrustMe = Just Refl
    testEquality TokStorable TokStorable = Just Refl
    testEquality TokExpose TokExpose = Just Refl
    testEquality TokImport TokImport = Just Refl
    testEquality TokAs TokAs = Just Refl
    testEquality TokExcept TokExcept = Just Refl
    testEquality TokNamespace TokNamespace = Just Refl
    testEquality TokDocSec TokDocSec = Just Refl
    testEquality TokWith TokWith = Just Refl
    testEquality TokNamesUpper TokNamesUpper = Just Refl
    testEquality TokNamesLower TokNamesLower = Just Refl
    testEquality TokImplicitName TokImplicitName = Just Refl
    testEquality TokUnderscore TokUnderscore = Just Refl
    testEquality TokFn TokFn = Just Refl
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
    testEquality TokDebug TokDebug = Just Refl
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
    show TokImply = show ("imply" :: String)
    show TokAp = show ("ap" :: String)
    show TokDo = show ("do" :: String)
    show TokIf = show ("if" :: String)
    show TokThen = show ("then" :: String)
    show TokElse = show ("else" :: String)
    show TokType = show ("type" :: String)
    show TokDataType = show ("datatype" :: String)
    show TokEntityType = show ("entitytype" :: String)
    show TokPredicateType = show ("predicatetype" :: String)
    show TokSubtype = show ("subtype" :: String)
    show TokTrustMe = show ("trustme" :: String)
    show TokStorable = show ("storable" :: String)
    show TokExpose = show ("expose" :: String)
    show TokImport = show ("import" :: String)
    show TokAs = show ("as" :: String)
    show TokExcept = show ("except" :: String)
    show TokNamespace = show ("namespace" :: String)
    show TokDocSec = show ("docsec" :: String)
    show TokWith = show ("with" :: String)
    show TokNamesUpper = "unames"
    show TokNamesLower = "lnames"
    show TokImplicitName = "implicit name"
    show TokUnderscore = show ("_" :: String)
    show TokFn = show ("fn" :: String)
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
    show TokDebug = "debug"

instance Show (SomeOf Token) where
    show (MkSomeOf t x) =
        show t <>
        case showTokenContents t of
            Just f -> "(" <> f x <> ")"
            Nothing -> ""

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
    firstC <- satisfy allowedAlphaNameFirstChar
    rest <- many $ satisfy allowedAlphaNameChar
    return (isUpper firstC, MkName $ pack $ firstC : rest)

checkKeyword :: Text -> Maybe (SomeOf Token)
checkKeyword "_" = return $ MkSomeOf TokUnderscore ()
checkKeyword "fn" = return $ MkSomeOf TokFn ()
checkKeyword "rec" = return $ MkSomeOf TokRec ()
checkKeyword "let" = return $ MkSomeOf TokLet ()
checkKeyword "imply" = return $ MkSomeOf TokImply ()
checkKeyword "ap" = return $ MkSomeOf TokAp ()
checkKeyword "do" = return $ MkSomeOf TokDo ()
checkKeyword "if" = return $ MkSomeOf TokIf ()
checkKeyword "then" = return $ MkSomeOf TokThen ()
checkKeyword "else" = return $ MkSomeOf TokElse ()
checkKeyword "type" = return $ MkSomeOf TokType ()
checkKeyword "datatype" = return $ MkSomeOf TokDataType ()
checkKeyword "entitytype" = return $ MkSomeOf TokEntityType ()
checkKeyword "predicatetype" = return $ MkSomeOf TokPredicateType ()
checkKeyword "subtype" = return $ MkSomeOf TokSubtype ()
checkKeyword "trustme" = return $ MkSomeOf TokTrustMe ()
checkKeyword "storable" = return $ MkSomeOf TokStorable ()
checkKeyword "expose" = return $ MkSomeOf TokExpose ()
checkKeyword "import" = return $ MkSomeOf TokImport ()
checkKeyword "as" = return $ MkSomeOf TokAs ()
checkKeyword "except" = return $ MkSomeOf TokExcept ()
checkKeyword "namespace" = return $ MkSomeOf TokNamespace ()
checkKeyword "docsec" = return $ MkSomeOf TokDocSec ()
checkKeyword "with" = return $ MkSomeOf TokWith ()
checkKeyword "debug"
    | debugSyntaxINTERNAL = return $ MkSomeOf TokDebug ()
checkKeyword _ = Nothing

allKeywords :: [(Text, Text)]
allKeywords =
    [ ("fn", "keyword.control.pinafore")
    , ("rec", "keyword.other.pinafore")
    , ("let", "keyword.other.pinafore")
    , ("imply", "keyword.other.pinafore")
    , ("ap", "keyword.control.pinafore")
    , ("do", "keyword.control.pinafore")
    , ("if", "keyword.control.pinafore")
    , ("then", "keyword.control.pinafore")
    , ("else", "keyword.control.pinafore")
    , ("type", "keyword.declaration.pinafore")
    , ("datatype", "keyword.declaration.pinafore")
    , ("entitytype", "keyword.declaration.pinafore")
    , ("predicatetype", "keyword.declaration.pinafore")
    , ("subtype", "keyword.declaration.pinafore")
    , ("trustme", "keyword.declaration.pinafore")
    , ("storable", "keyword.declaration.pinafore")
    , ("module", "keyword.declaration.pinafore")
    , ("expose", "keyword.declaration.pinafore")
    , ("import", "keyword.declaration.pinafore")
    , ("as", "keyword.other.pinafore")
    , ("except", "keyword.other.pinafore")
    , ("namespace", "keyword.declaration.pinafore")
    , ("docsec", "keyword.declaration.pinafore")
    , ("with", "keyword.declaration.pinafore")
    ]

readTextToken :: Parser (SomeOf Token)
readTextToken = do
    (u, name) <- readName
    case checkKeyword $ showText name of
        Just stok -> return stok
        Nothing -> do
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
            let
                ttype =
                    if u
                        then TokNamesUpper
                        else TokNamesLower
                tns = MkTokenNames {tnAbsolute = isJust mabs, tnSpace = nspace, tnName = name}
            return $ MkSomeOf ttype tns

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

readImplicitName :: Parser (SomeOf Token)
readImplicitName =
    try $ do
        readChar '?'
        (u, name) <- readName
        altIf $ not u
        return $ MkSomeOf TokImplicitName $ MkImplicitName name

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
        "." -> return $ MkSomeOf TokOperator $ fromString name
        _ ->
            case nonEmpty name of
                Just nname
                    | '.' <- last nname -> do
                        nsfirst <-
                            many $
                            try $ do
                                n <- readName
                                readChar '.'
                                return n
                        mnslast <- optional readName
                        let
                            tnName = MkName $ pack $ init nname
                            (ns, tnAbsolute) =
                                case mnslast of
                                    Just nslast -> (nsfirst <> [nslast], False)
                                    Nothing -> (nsfirst, True)
                        tnSpace <-
                            for ns $ \(b, nsn) -> do
                                altIf b
                                return nsn
                        return $ MkSomeOf TokOperator MkTokenNames {..}
                _ -> return $ MkSomeOf TokOperator $ fromString name

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
        readImplicitName <|>
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

parseTokens :: Text -> StateT SourcePos (Result ParseError) [(SourcePos, SomeOf Token)]
parseTokens text = do
    oldpos <- get
    case parse (readTokens oldpos) (sourceName oldpos) (unpack text) of
        Right (newpos, a) -> do
            put newpos
            return a
        Left e -> throwExc e
