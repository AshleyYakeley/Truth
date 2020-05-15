module Pinafore.Language.Read.Parser
    ( module Pinafore.Language.Read.Parser
    , SourcePos
    , initialPos
    , getPosition
    , try
    , (<?>)
    ) where

import Pinafore.Language.Error
import Pinafore.Language.Read.Token
import Pinafore.Language.Scope
import Pinafore.Language.Syntax
import Pinafore.Language.TypeSystem
import Shapes hiding (try)
import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.Pos (initialPos)

type Parser = Parsec [(SourcePos, AnyValue Token)] ()

readEnd :: Parser ()
readEnd = eof

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

parseScopedReaderWhole :: Parser (PinaforeScoped t) -> Text -> PinaforeSourceScoped t
parseScopedReaderWhole parser text = do
    spos <- askSourcePos
    case evalStateT (parseReader parser text) spos of
        SuccessResult a -> liftSourcePos a
        FailureResult e -> throw e

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

readCommaList :: Monoid t => Parser t -> Parser t
readCommaList = readSeparated $ readThis TokComma

readSourcePos :: Parser t -> Parser (WithSourcePos t)
readSourcePos p = do
    spos <- getPosition
    t <- p
    return $ MkWithSourcePos spos t
