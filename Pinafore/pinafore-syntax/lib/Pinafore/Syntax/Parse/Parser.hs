module Pinafore.Syntax.Parse.Parser
    ( runTokens
    , Parser
    , namespaceParam
    , readEnd
    , SourcePos
    , initialPos
    , getPosition
    , readToken
    , readComments
    , ignoreComments
    , try
    , (<?>)
    , runParser
    ) where

import Pinafore.Syntax.Name
import Pinafore.Syntax.Parse.Error
import Pinafore.Syntax.Parse.Token
import Pinafore.Syntax.Text
import Shapes hiding (try)
import qualified Text.Parsec as P
import Text.Parsec.Pos (SourcePos, initialPos)

runTokens :: Text -> StateT SourcePos ParseResult [(SourcePos, SomeOf Token)]
runTokens text = hoist (mapResultFailure $ fmap LexicalErrorType . parseErrorMessage) $ parseTokens text

newtype Parser a =
    MkParser (ReaderT Namespace (P.ParsecT [(SourcePos, SomeOf Token)] () ParseResult) a)
    deriving newtype (Functor, Applicative, Monad, Alternative, MonadPlus, MonadFail)

instance MonadThrow ParseErrorType Parser where
    throw err = do
        spos <- getPosition
        MkParser $ lift $ lift $ throwExc $ MkSourceError spos toText err

namespaceParam :: Param Parser Namespace
namespaceParam = MkParam (MkParser ask) $ \a (MkParser m) -> MkParser $ with a m

getPosition :: Parser SourcePos
getPosition = MkParser $ lift P.getPosition

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

readComments :: Parser [Comment]
readComments = many $ readToken TokComment

ignoreComments :: Parser ()
ignoreComments = void readComments

readEnd :: Parser ()
readEnd = do
    ignoreComments
    MkParser $ lift P.eof

try :: Parser --> Parser
try (MkParser p) = MkParser $ hoist P.try p

(<?>) :: Parser a -> String -> Parser a
MkParser p <?> t = MkParser $ hoist (\pp -> pp P.<?> t) p

runParser :: Parser a -> Text -> StateT SourcePos ParseResult a
runParser r text = let
    MkParser r' = do
        a <- r
        readEnd
        return a
    in do
           spos <- get
           toks <- runTokens text
           ma <- lift $ P.runParserT (runReaderT r' RootNamespace) () (P.sourceName spos) toks
           case ma of
               Right a -> return a
               Left e -> throwExc $ fmap SyntaxErrorType $ parseErrorMessage e
