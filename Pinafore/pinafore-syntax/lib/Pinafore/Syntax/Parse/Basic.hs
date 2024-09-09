module Pinafore.Syntax.Parse.Basic
    ( readAskNamespace
    , readEnd
    , readWithNamespace
    , readWithNamespaceRef
    , readWithNamespaceName
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
    , readNamespaceQualifier
    , readModuleName
    , readLines1
    , readLines
    , readBraced
    , readWithDoc
    , chainModify
    ) where

import Pinafore.Syntax.Name
import Pinafore.Syntax.Parse.Parser
import Pinafore.Syntax.Parse.Token
import Pinafore.Syntax.Syntax
import Pinafore.Syntax.Text
import Shapes hiding (try)

readAskNamespace :: Parser Namespace
readAskNamespace = paramAsk namespaceParam

readWithNamespace :: Namespace -> Parser --> Parser
readWithNamespace = paramWith namespaceParam

readWithNamespaceRef :: NamespaceRef -> Parser --> Parser
readWithNamespaceRef nr = paramLocal namespaceParam $ \n -> namespaceConcatRef n nr

readWithNamespaceName :: Name -> Parser --> Parser
readWithNamespaceName name = readWithNamespaceRef $ RelativeNamespaceRef [name]

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

readNamespaceQualifier :: Parser Namespace
readNamespaceQualifier = do
    readExactlyThis TokOperator $ MkTokenNames False "." []
    readNamespace

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

readBraced :: Parser a -> Parser [a]
readBraced p = do
    readThis TokOpenBrace
    aa <- readLines p
    readThis TokCloseBrace
    return aa

readWithDoc :: Parser t -> Parser (SyntaxWithDoc t)
readWithDoc pt = do
    doc <- readDocComment
    decl <- pt
    return $ MkSyntaxWithDoc doc decl

chainModify :: forall a. (a -> Parser a) -> a -> Parser a
chainModify p a = (p a >>= chainModify p) <|> return a
