{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Grammar.Read.Parser
    ( module Pinafore.Language.Grammar.Read.Parser
    , SourcePos
    , initialPos
    , getPosition
    , try
    , (<?>)
    ) where

import Pinafore.Language.Error
import Pinafore.Language.Grammar.Read.Token
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Markdown
import Shapes hiding (try)
import Text.Parsec hiding ((<|>), many, optional)

type Parser = ParsecT [(SourcePos, SomeOf Token)] () (Result PinaforeError)

readEnd :: Parser ()
readEnd = do
    ignoreComments
    eof

parseReader :: Parser a -> Text -> StateT SourcePos InterpretResult a
parseReader r text = let
    r' = do
        a <- r
        readEnd
        return a
    in do
           spos <- get
           toks <- parseTokens text
           case runParserT r' () (sourceName spos) toks of
               SuccessResult (Right a) -> return a
               SuccessResult (Left e) -> throw $ parseErrorMessage e
               FailureResult e -> throw e

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
        case runIdentity (Text.Parsec.uncons ts) of
            Nothing -> fst tok
            Just (tok', _) -> fst tok'
    test (_, MkSomeOf tok t) =
        case testEquality stok tok of
            Just Refl -> Just t
            Nothing -> Nothing
    in tokenPrim showToken nextpos test

instance MonadThrow ErrorType Parser where
    throw err = do
        spos <- getPosition
        lift $ throwExc $ MkPinaforeError $ pure $ MkErrorMessage spos err mempty

readComments :: Parser [Comment]
readComments = many $ readToken TokComment

ignoreComments :: Parser ()
ignoreComments = void readComments

lineMarkdown :: [Comment] -> Maybe Markdown
lineMarkdown [] = Nothing
lineMarkdown (LineComment ('|':s):cc) = do
    ss <- lineMarkdown cc
    return $ (rawMarkdown $ strip $ pack s) <> "\n" <> ss
lineMarkdown _ = Nothing

getMarkdown :: [Comment] -> Maybe Markdown
getMarkdown [] = Nothing
getMarkdown [BlockComment ('|':s)] = Just $ rawMarkdown $ strip $ pack s
getMarkdown (LineComment ('|':s):cc) = do
    ss <- lineMarkdown cc
    return $ (rawMarkdown $ strip $ pack s) <> "\n" <> ss
getMarkdown (_:cc) = getMarkdown cc

readDocComment :: Parser Markdown
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

readSourcePos :: Parser t -> Parser (WithSourcePos t)
readSourcePos p = do
    spos <- getPosition
    t <- p
    return $ MkWithSourcePos spos t

readFullUName :: Parser FullNameRef
readFullUName = do
    MkTokenNames {..} <- readThis TokNamesUpper
    return $
        MkFullNameRef
            ((if tnAbsolute
                  then FullNamespaceRef . MkNamespace
                  else RelativeNamespaceRef)
                 tnSpace)
            tnName

readFullLName :: Parser FullNameRef
readFullLName = do
    MkTokenNames {..} <- readThis TokNamesLower
    return $
        MkFullNameRef
            ((if tnAbsolute
                  then FullNamespaceRef . MkNamespace
                  else RelativeNamespaceRef)
                 tnSpace)
            tnName

readFullNameRef :: Parser FullNameRef
readFullNameRef = readFullUName <|> readFullLName

readUName :: Parser Name
readUName = do
    MkTokenNames {..} <- readThis TokNamesUpper
    altIf $ not tnAbsolute
    case tnSpace of
        [] -> return tnName
        _ -> empty

readLName :: Parser Name
readLName = do
    MkTokenNames {..} <- readThis TokNamesLower
    altIf $ not tnAbsolute
    case tnSpace of
        [] -> return tnName
        _ -> empty

readNamespaceRef :: Parser NamespaceRef
readNamespaceRef = do
    MkTokenNames {..} <- readThis TokNamesUpper
    return $
        (if tnAbsolute
             then FullNamespaceRef . MkNamespace
             else RelativeNamespaceRef) $
        tnSpace <> [tnName]

neList :: [a] -> a -> NonEmpty a
neList [] b = b :| []
neList (a:aa) b = a :| (aa <> [b])

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

readWithDoc :: Parser t -> Parser (SyntaxWithDoc t)
readWithDoc pt = do
    doc <- readDocComment
    decl <- pt
    return $ MkSyntaxWithDoc doc decl

chainModify :: forall a. (a -> Parser a) -> a -> Parser a
chainModify p a = (p a >>= chainModify p) <|> return a
