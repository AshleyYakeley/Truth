module Pinafore.Syntax.Parse.Error
    ( SourceError(..)
    , parseErrorMessage
    , ParseErrorType(..)
    , ParseResult
    ) where

import Pinafore.Base
import Pinafore.Syntax.Name
import Pinafore.Syntax.Text
import Shapes hiding (try)
import Text.Parsec qualified as P
import Text.Parsec.Error qualified as P

showSourceError :: P.SourcePos -> Text -> Text
showSourceError spos s =
    toText (P.sourceName spos) <>
    ":" <> toText (show (P.sourceLine spos)) <> ":" <> toText (show (P.sourceColumn spos)) <> ": " <> s

data SourceError a =
    MkSourceError P.SourcePos
                  (NamedText -> Text)
                  a

instance Functor SourceError where
    fmap ab (MkSourceError spos ntt a) = MkSourceError spos ntt $ ab a

instance ShowNamedText a => ShowText (SourceError a) where
    showText (MkSourceError spos ntt err) = showSourceError spos $ ntt $ showNamedText err

parseErrorMessage :: P.ParseError -> SourceError [P.Message]
parseErrorMessage err = MkSourceError (P.errorPos err) toText (P.errorMessages err)

data ParseErrorType
    = LexicalErrorType [P.Message]
    | SyntaxErrorType [P.Message]
    | DeclareDatatypeStorableSupertypeError FullName
    | MatchesDifferentCount Natural
                            Natural

getMessagesNamedText :: [P.Message] -> NamedText
getMessagesNamedText msgs = let
    getMsgs :: (P.Message -> Maybe String) -> [Text]
    getMsgs getm =
        nub $
        mapMaybe
            (\msg -> do
                 s <- getm msg
                 if s == ""
                     then Nothing
                     else return $ toText s)
            msgs
    msgsSysUnExpect =
        getMsgs $ \case
            P.SysUnExpect s -> Just s
            _ -> Nothing
    msgsExpect =
        getMsgs $ \case
            P.Expect s -> Just s
            _ -> Nothing
    msgsMessage =
        getMsgs $ \case
            P.Message s -> Just s
            _ -> Nothing
    semicolon :: Text -> Text -> Text
    semicolon "" b = b
    semicolon a "" = a
    semicolon a b = a <> "; " <> b
    strUnexpected =
        case msgsSysUnExpect of
            [] -> ""
            s -> "unexpected: " <> intercalate ", " s
    strExpecting =
        case msgsExpect of
            [] -> ""
            s -> "expecting: " <> intercalate ", " s
    strMessage = intercalate "; " msgsMessage
    in toNamedText $ strUnexpected `semicolon` strExpecting `semicolon` strMessage

instance ShowNamedText ParseErrorType where
    showNamedText (LexicalErrorType msgs) = "lexical: " <> getMessagesNamedText msgs
    showNamedText (SyntaxErrorType msgs) = "syntax: " <> getMessagesNamedText msgs
    showNamedText (DeclareDatatypeStorableSupertypeError n) = "datatype storable has supertypes: " <> showNamedText n
    showNamedText (MatchesDifferentCount expected found) =
        "different number of patterns in match, expected " <>
        showNamedText expected <> ", found " <> showNamedText found

type ParseResult = Result (SourceError ParseErrorType)
