module Pinafore.Syntax.Lex
    ( parseTokens
    , allKeywords
    , LexLiteral (..)
    , readLiteralMaybe
    )
where

import Pinafore.Base
import Shapes hiding (try)
import Text.Parsec hiding (many, optional, (<|>))

import Pinafore.Syntax.Lex.Lexer
import Pinafore.Syntax.Lex.Literal
import Pinafore.Syntax.Name
import Pinafore.Syntax.Token

debugSyntaxINTERNAL :: Bool
debugSyntaxINTERNAL = False

lexWS :: Lexer ()
lexWS = spaces <?> "white space"

lexBlockComment :: Lexer Comment
lexBlockComment = let
    blockCommentOpen :: Lexer ()
    blockCommentOpen =
        try $ do
            lexChar '{'
            lexChar '#'
    blockCommentInterior :: Lexer String
    blockCommentInterior = (fmap show lexBlockComment) <|> (endOfLine >> return "") <|> fmap pure anyToken
    blockCommentClose :: Lexer ()
    blockCommentClose =
        try $ do
            lexChar '#'
            lexChar '}'
    in do
        blockCommentOpen
        s <- manyTill blockCommentInterior blockCommentClose
        return $ BlockComment $ mconcat s

lexLineComment :: Lexer Comment
lexLineComment = let
    isLineBreak :: Char -> Bool
    isLineBreak '\n' = True
    isLineBreak '\r' = True
    isLineBreak _ = False
    in do
        lexChar '#'
        s <- many (satisfy (\c -> not (isLineBreak c)))
        void endOfLine
        return $ LineComment s

lexComment :: Lexer Comment
lexComment = lexBlockComment <|> lexLineComment

readName :: Lexer (Bool, Name)
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

-- https://macromates.com/manual/en/language_grammars#naming-conventions
keywordClasses :: [(Text, [Token ()])]
keywordClasses =
    [ ("keyword.control.pinafore", [TokFn, TokAp, TokDo, TokIf, TokThen, TokElse])
    ,
        ( "keyword.declaration.pinafore"
        ,
            [ TokType
            , TokDataType
            , TokEntityType
            , TokPredicateType
            , TokSubtype
            , TokTrustMe
            , TokStorable
            , TokExpose
            , TokImport
            , TokNamespace
            , TokDocSec
            , TokWith
            ]
        )
    , ("keyword.other.pinafore", [TokRec, TokLet, TokImply, TokAs, TokExcept])
    ]

extraKeywords :: [(Text, Text)]
extraKeywords = [("!expression", "keyword.other.pinafore"), ("!declarations", "keyword.other.pinafore")]

allKeywords :: [(Text, Text)]
allKeywords =
    (mconcat $ fmap (\(tokclass, toks) -> fmap (\tok -> (fixedTokenName tok, tokclass)) toks) keywordClasses)
        <> extraKeywords

readTextToken :: Lexer (SomeOf Token)
readTextToken = do
    (u, name) <- readName
    case checkKeyword $ showText name of
        Just stok -> return stok
        Nothing -> do
            ns <-
                many
                    $ try
                    $ do
                        lexChar '.'
                        readName
            nspace <-
                for ns $ \(b, nsn) -> do
                    guard b
                    return nsn
            mabs <- optional $ lexChar '.'
            let
                ttype =
                    if u
                        then TokNamesUpper
                        else TokNamesLower
                tns = MkTokenNames{tnAbsolute = isJust mabs, tnSpace = nspace, tnName = name}
            return $ MkSomeOf ttype tns

toHexDigit :: Char -> Maybe Word8
toHexDigit c =
    if isHexDigit c
        then Just $ fromIntegral $ digitToInt c
        else Nothing

fromHex :: [Char] -> Maybe [Word8]
fromHex [] = Just []
fromHex (chi : clo : cc) = do
    whi <- toHexDigit chi
    wlo <- toHexDigit clo
    ww <- fromHex cc
    return $ (whi * 16 + wlo) : ww
fromHex [_] = Nothing

hexAnchorChar :: Char -> Bool
hexAnchorChar '-' = True
hexAnchorChar c = isHexDigit c

readHexAnchor :: Lexer Anchor
readHexAnchor = do
    cs <- some $ satisfy hexAnchorChar
    mpure $ do
        octets <- fromHex $ filter isHexDigit cs
        decode anchorCodec $ fromList octets

readLowerCaseName :: Lexer Name
readLowerCaseName = do
    (u, name) <- readName
    guard $ not u
    return name

readImplicitName :: Lexer (SomeOf Token)
readImplicitName =
    try $ do
        lexChar '?'
        name <- readLowerCaseName
        return $ MkSomeOf TokImplicitName $ MkImplicitName name

readOpToken :: Lexer (SomeOf Token)
readOpToken = do
    name <-
        many1
            $ satisfy
            $ \c ->
                elem c ("!$%&*+./<=>?@\\^|-~:" :: String) || (not (isAscii c) && (isSymbol c || isPunctuation c))
    case name of
        ":" -> return $ MkSomeOf TokTypeJudge ()
        ":?" -> return $ MkSomeOf TokTypeDynamic ()
        "=" -> return $ MkSomeOf TokAssign ()
        "=>" -> return $ MkSomeOf TokMap ()
        "<-" -> return $ MkSomeOf TokBackMap ()
        "%" -> return $ MkSomeOf TokUnquote ()
        "!" ->
            ( do
                lexChar '{'
                return $ MkSomeOf TokSpliceOpenBrace ()
            )
                <|> ( do
                        s <- lexLiteral
                        return $ MkSomeOf TokAnchor $ codeAnchor s
                    )
                <|> ( do
                        n <- readLowerCaseName
                        return $ MkSomeOf TokSpecialName n
                    )
                <|> ( try $ do
                        anchor <- readHexAnchor
                        return $ MkSomeOf TokAnchor anchor
                    )
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
                            many
                                $ try
                                $ do
                                    n <- readName
                                    lexChar '.'
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
                                guard b
                                return nsn
                        return $ MkSomeOf TokOperator MkTokenNames{..}
                _ -> return $ MkSomeOf TokOperator $ fromString name

readCharTok :: Char -> Token () -> Lexer (SomeOf Token)
readCharTok c tok = do
    lexChar c
    return $ MkSomeOf tok ()

readToken :: Lexer (SourcePos, SomeOf Token)
readToken = do
    pos <- getPosition
    t <-
        fmap (MkSomeOf TokComment) lexComment
            <|> readCharTok ';' TokSemicolon
            <|> readCharTok ',' TokComma
            <|> readCharTok '(' TokOpenParen
            <|> readCharTok ')' TokCloseParen
            <|> readCharTok '[' TokOpenBracket
            <|> readCharTok ']' TokCloseBracket
            <|> readCharTok '{' TokOpenBrace
            <|> readCharTok '}' TokCloseBrace
            <|> try (fmap (MkSomeOf TokNumber) lexLiteral)
            <|> fmap (MkSomeOf TokString) lexLiteral
            <|> readTextToken
            <|> readImplicitName
            <|> readOpToken
    lexWS
    return (pos, t)

readTokens :: SourcePos -> Lexer (SourcePos, [(SourcePos, SomeOf Token)])
readTokens oldpos = do
    setPosition oldpos
    lexWS
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
