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

type Parser = Parsec [(SourcePos, AnyValue Token)] ()

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
           case parse r' (sourceName spos) toks of
               Right a -> return a
               Left e -> throwErrorMessage $ parseErrorMessage e

parseScopedReaderWhole :: Parser (PinaforeInterpreter t) -> Text -> PinaforeInterpreter t
parseScopedReaderWhole parser text = do
    spos <- askD sourcePosParam
    result <- runInterpretResult $ evalStateT (parseReader parser text) spos
    case result of
        SuccessResult a -> a
        FailureResult e -> throw e

readToken :: Token t -> Parser t
readToken stok =
    token (\(_, MkAnyValue tok _) -> show tok) fst $ \(_, MkAnyValue tok t) ->
        case testEquality stok tok of
            Just Refl -> Just t
            Nothing -> Nothing

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

readExactlyThis :: Eq t => Token t -> t -> Parser ()
readExactlyThis tok t =
    try $ do
        a <- readThis tok
        if a == t
            then return ()
            else mzero

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

readReferenceUName :: Parser ReferenceName
readReferenceUName =
    (fmap UnqualifiedReferenceName $ readThis TokUName) <|>
    (fmap (\(m, n) -> QualifiedReferenceName (MkModuleName m) n) $ readThis TokQUName)

readReferenceLName :: Parser ReferenceName
readReferenceLName =
    (fmap UnqualifiedReferenceName $ readThis TokLName) <|>
    (fmap (\(m, n) -> QualifiedReferenceName m n) $ readThis TokQLName)

readReferenceName :: Parser ReferenceName
readReferenceName = readReferenceUName <|> readReferenceLName
