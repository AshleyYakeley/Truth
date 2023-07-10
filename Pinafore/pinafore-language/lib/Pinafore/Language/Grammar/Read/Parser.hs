module Pinafore.Language.Grammar.Read.Parser
    ( Parser
    , readEnd
    , SourcePos
    , initialPos
    , getPosition
    , try
    , (<?>)
    , parseReader
    , readAskNamespace
    , readWithNamespace
    , readWithNamespaceRef
    , readWithNamespaceName
    , parseScopedReaderWhole
    , readThis
    , readExactly
    , readExactlyThis
    , readBracketed
    , readParen
    , readBracket
    , readSeparated1
    , readCommaM
    , readCommaList
    , readWithSourcePos
    , readFullUName
    , readFullLName
    , readFullNameRef
    , readUName
    , readLName
    , readNewUName
    , readNewLName
    , readNamespaceRef
    , readNamespace
    , readModuleName
    , readLines1
    , readLines
    , readOf
    , readWithDoc
    , chainModify
    ) where

import Pinafore.Language.Error
import Pinafore.Language.Grammar.Read.Token
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Text
import Shapes hiding (try)
import qualified Text.Parsec as P

newtype Parser a =
    MkParser (ReaderT Namespace (P.ParsecT [(SourcePos, SomeOf Token)] () (Result PinaforeError)) a)
    deriving newtype (Functor, Applicative, Monad, Alternative, MonadPlus, MonadFail)

getPosition :: Parser SourcePos
getPosition = MkParser $ lift P.getPosition

readEnd :: Parser ()
readEnd = do
    ignoreComments
    MkParser $ lift P.eof

try :: Parser --> Parser
try (MkParser p) = MkParser $ hoist P.try p

(<?>) :: Parser a -> String -> Parser a
MkParser p <?> t = MkParser $ hoist (\pp -> pp P.<?> t) p

parseReader :: Parser a -> Text -> StateT SourcePos InterpretResult a
parseReader r text = let
    MkParser r' = do
        a <- r
        readEnd
        return a
    in do
           spos <- get
           toks <- parseTokens text
           case P.runParserT (runReaderT r' RootNamespace) () (P.sourceName spos) toks of
               SuccessResult (Right a) -> return a
               SuccessResult (Left e) -> throw $ parseErrorMessage e
               FailureResult e -> throw e

readAskNamespace :: Parser Namespace
readAskNamespace = MkParser ask

readWithNamespace :: Namespace -> Parser --> Parser
readWithNamespace ns (MkParser p) = MkParser $ local (\_ -> ns) p

readWithNamespaceRef :: NamespaceRef -> Parser --> Parser
readWithNamespaceRef nr (MkParser p) = MkParser $ local (\n -> namespaceConcatRef n nr) p

readWithNamespaceName :: Name -> Parser --> Parser
readWithNamespaceName name = readWithNamespaceRef $ RelativeNamespaceRef [name]

parseScopedReaderWhole :: Parser (QInterpreter t) -> Text -> QInterpreter t
parseScopedReaderWhole parser text = do
    spos <- paramAsk sourcePosParam
    result <- runInterpretResult $ evalStateT (parseReader parser text) spos
    case result of
        SuccessResult a -> a
        FailureResult e -> throw e

readToken :: Token t -> Parser t
readToken stok = let
    showToken :: (SourcePos, SomeOf Token) -> String
    showToken (_, MkSomeOf tok _) = show tok
    nextpos _ tok ts =
        case runIdentity (P.uncons ts) of
            Nothing -> fst tok
            Just (tok', _) -> fst tok'
    test (_, MkSomeOf tok t) =
        case testEquality stok tok of
            Just Refl -> Just t
            Nothing -> Nothing
    in MkParser $ lift $ P.tokenPrim showToken nextpos test

instance MonadThrow ErrorType Parser where
    throw err = do
        spos <- getPosition
        MkParser $ lift $ lift $ throwExc $ MkPinaforeError $ pure $ MkErrorMessage spos toText err mempty

readComments :: Parser [Comment]
readComments = many $ readToken TokComment

ignoreComments :: Parser ()
ignoreComments = void readComments

lineMarkdown :: [Comment] -> Maybe RawMarkdown
lineMarkdown [] = Just ""
lineMarkdown (LineComment ('|':s):cc) = do
    ss <- lineMarkdown cc
    return $ (fromString $ trimSpace s) <> "\n" <> ss
lineMarkdown _ = Nothing

getMarkdown :: [Comment] -> Maybe RawMarkdown
getMarkdown [] = Nothing
getMarkdown [BlockComment ('|':s)] = Just $ fromString $ trimSpace s
getMarkdown (LineComment ('|':s):cc) = do
    ss <- lineMarkdown cc
    return $ (fromString $ trimSpace s) <> "\n" <> ss
getMarkdown (_:cc) = getMarkdown cc

readDocComment :: Parser RawMarkdown
readDocComment = do
    comments <- readComments
    return $
        case getMarkdown comments of
            Just md -> md
            Nothing -> ""

readThis :: Token t -> Parser t
readThis tok =
    try $ do
        ignoreComments
        readToken tok

readExactly :: Eq t => Parser t -> t -> Parser ()
readExactly p t =
    try $ do
        a <- p
        altIf $ a == t

readExactlyThis :: Eq t => Token t -> t -> Parser ()
readExactlyThis tok t = readExactly (readThis tok) t

readBracketed :: Token () -> Token () -> Parser t -> Parser t
readBracketed op cl pp = do
    readThis op
    t <- pp
    readThis cl
    return t

readParen :: Parser t -> Parser t
readParen = readBracketed TokOpenParen TokCloseParen

readBracket :: Parser t -> Parser t
readBracket = readBracketed TokOpenBracket TokCloseBracket

readSeparated1 :: Semigroup t => Parser () -> Parser t -> Parser t
readSeparated1 sep p = do
    v1 <- p
    mv2 <-
        optional $ do
            sep
            readSeparated1 sep p
    case mv2 of
        Just v2 -> return $ v1 <> v2
        Nothing -> return v1

readSeparated :: Monoid t => Parser () -> Parser t -> Parser t
readSeparated sep p = readSeparated1 sep p <|> return mempty

readCommaM :: Monoid t => Parser t -> Parser t
readCommaM = readSeparated $ readThis TokComma

readCommaList :: Parser t -> Parser [t]
readCommaList p = readCommaM $ fmap pure p

readWithSourcePos :: Parser t -> Parser (WithSourcePos t)
readWithSourcePos p = do
    spos <- getPosition
    t <- p
    return $ MkWithSourcePos spos t

readFullUName :: Parser FullNameRef
readFullUName = fmap tokenNamesToFullNameRef $ readThis TokNamesUpper

readFullLName :: Parser FullNameRef
readFullLName = fmap tokenNamesToFullNameRef $ readThis TokNamesLower

readFullNameRef :: Parser FullNameRef
readFullNameRef =
    fmap tokenNamesToFullNameRef $ readThis TokNamesUpper <|> readThis TokNamesLower <|> readThis TokOperator

readUName :: Parser Name
readUName = do
    tns <- readThis TokNamesUpper
    mpure $ tokenNamesToSingleName tns

readLName :: Parser Name
readLName = do
    tns <- readThis TokNamesLower
    mpure $ tokenNamesToSingleName tns

readNewUName :: Parser FullName
readNewUName = do
    name <- readUName
    ns <- readAskNamespace
    return $ MkFullName name ns

readNewLName :: Parser FullName
readNewLName = do
    name <- readLName
    ns <- readAskNamespace
    return $ MkFullName name ns

readNamespaceRef :: Parser NamespaceRef
readNamespaceRef = fmap tokenNamesToNamespaceRef $ readThis TokNamesUpper

readNamespace :: Parser Namespace
readNamespace = do
    ns <- readAskNamespace
    nref <- readNamespaceRef
    return $ namespaceConcatRef ns nref

readModuleName :: Parser ModuleName
readModuleName = do
    s <- readThis TokString
    return $ MkModuleName s

readLines1 :: Parser a -> Parser (NonEmpty a)
readLines1 p = do
    a <- try p
    ma <-
        optional $ do
            readThis TokSemicolon
            readLines p
    return $ a :| fromMaybe [] ma

readLines :: Parser a -> Parser [a]
readLines p = (fmap toList $ readLines1 p) <|> (return [])

readOf :: Parser a -> Parser [a]
readOf p = do
    readThis TokOf
    aa <- readLines p
    readThis TokEnd
    return aa

readWithDoc :: Parser t -> Parser (SyntaxWithDoc t)
readWithDoc pt = do
    doc <- readDocComment
    decl <- pt
    return $ MkSyntaxWithDoc doc decl

chainModify :: forall a. (a -> Parser a) -> a -> Parser a
chainModify p a = (p a >>= chainModify p) <|> return a
