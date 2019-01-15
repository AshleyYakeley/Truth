module Pinafore.Language.Read.Parser
    ( module Pinafore.Language.Read.Parser
    , SourcePos
    , initialPos
    , getPosition
    , try
    , (<?>)
    ) where

import Pinafore.Language.Read.Token
import Pinafore.Language.Scope
import Pinafore.Language.Type
import Shapes hiding (try)
import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.Pos (initialPos)

type Parser = Parsec [(SourcePos, AnyValue Token)] ()

parseEnd :: Parser ()
parseEnd = eof

parseReader :: Parser a -> Text -> StateT SourcePos (Result Text) a
parseReader parser text = do
    spos <- get
    toks <- parseTokens text
    case parse parser (sourceName spos) toks of
        Right a -> return a
        Left e -> fail $ show e

parseScopedReader :: Parser (PinaforeScoped baseedit t) -> Text -> PinaforeSourceScoped baseedit t
parseScopedReader parser text = do
    spos <- askSourcePos
    case evalStateT (parseReader parser text) spos of
        SuccessResult a -> liftSourcePos a
        FailureResult e -> fail $ unpack e

readThis :: Token t -> Parser t
readThis tok =
    token (\(_, MkAnyValue tok' _) -> show tok') fst $ \(_, MkAnyValue tok' t) ->
        case testEquality tok tok' of
            Just Refl -> Just t
            Nothing -> Nothing

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

readCommaList1 :: Semigroup t => Parser t -> Parser t
readCommaList1 p = do
    v1 <- p
    mv2 <-
        optional $ do
            readThis TokComma
            readCommaList1 p
    case mv2 of
        Just v2 -> return $ v1 <> v2
        Nothing -> return v1

readCommaList :: Monoid t => Parser t -> Parser t
readCommaList p = readCommaList1 p <|> return mempty
