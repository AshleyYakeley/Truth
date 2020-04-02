module Pinafore.Language.Error where

import Data.Shim
import Language.Expression.Error
import Pinafore.Language.Name
import Shapes
import Text.Parsec.Error
import Text.Parsec.Pos

data ErrorType
    = ParserError [Message]
    | ExpressionErrorError ExpressionError
    | LookupTypeUnknownError Name
    | LookupTypeNotOpenError Name
    | LookupConstructorUnknownError Name
    | DeclareTypeDuplicateError Name
    | DeclareConstructorDuplicateError Name
    | TypeConvertError Text
                       Text
    | TypeConvertInverseError Text
                              Text
    | TypeSubsumeError Polarity
                       Text
                       Text
    | TypeRecursiveError Name
                         Text
    | TypeNoInverseLimitError Text
    | NotationBareUnquoteError
    | InterpretTypeExprBadLimitError Polarity
    | InterpretTypeExprBadJoinMeetError Polarity
    | InterpretTypeNotEntityError Text
    | InterpretTypeNotOpenEntityError Text
    | InterpretTypeNoneNotNegativeEntityError
    | InterpretTypeUnderApplyError Text
    | InterpretTypeOverApplyError Text
    | InterpretTypeRangeApplyError Text
    | InterpretConstructorUnknownError Name
    | InterpretBindingsDuplicateError [Name]

instance Show ErrorType where
    show (ParserError msgs) = let
        getMsgs :: (Message -> Maybe String) -> [String]
        getMsgs getm =
            nub $
            mapMaybe
                (\msg -> do
                     s <- getm msg
                     if s == ""
                         then Nothing
                         else return s)
                msgs
        msgsSysUnExpect =
            getMsgs $ \case
                SysUnExpect s -> Just s
                _ -> Nothing
        msgsExpect =
            getMsgs $ \case
                Expect s -> Just s
                _ -> Nothing
        msgsMessage =
            getMsgs $ \case
                Message s -> Just s
                _ -> Nothing
        semicolon :: String -> String -> String
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
        in strUnexpected `semicolon` strExpecting `semicolon` strMessage
    show (ExpressionErrorError e) = show e
    show (LookupTypeUnknownError n) = "unknown type: " <> show n
    show (LookupTypeNotOpenError n) = show n <> " is not an open entity type"
    show (LookupConstructorUnknownError n) = "unknown constructor: " <> show n
    show (DeclareTypeDuplicateError n) = "duplicate type: " <> show n
    show (DeclareConstructorDuplicateError n) = "duplicate constructor: " <> show n
    show (TypeConvertError ta tb) = "cannot convert " <> unpack ta <> " <= " <> unpack tb
    show (TypeConvertInverseError ta tb) = "cannot inverse convert " <> unpack ta <> " <= " <> unpack tb
    show (TypeSubsumeError Positive tinf tdecl) = "cannot subsume " <> unpack tinf <> " <= " <> unpack tdecl
    show (TypeSubsumeError Negative tinf tdecl) = "cannot subsume " <> unpack tinf <> " >= " <> unpack tdecl
    show (TypeRecursiveError n t) = "cannot construct recursive type " <> show n <> " = " <> unpack t
    show (TypeNoInverseLimitError t) = "no inverse limit for " <> unpack t
    show NotationBareUnquoteError = "unquote outside Ref quote"
    show (InterpretTypeExprBadLimitError Positive) = "\"Any\" in positive type"
    show (InterpretTypeExprBadLimitError Negative) = "\"None\" in negative type"
    show (InterpretTypeExprBadJoinMeetError Positive) = "\"&\" in positive type"
    show (InterpretTypeExprBadJoinMeetError Negative) = "\"|\" in negative type"
    show (InterpretTypeNotEntityError t) = unpack t <> " is not an entity type"
    show (InterpretTypeNotOpenEntityError t) = unpack t <> " is not an open entity type"
    show InterpretTypeNoneNotNegativeEntityError = "\"None\" is not a negative entity type"
    show (InterpretTypeUnderApplyError t) = "underapplied type constuctor: " <> unpack t
    show (InterpretTypeOverApplyError t) = "overapplied type constuctor: " <> unpack t
    show (InterpretTypeRangeApplyError t) = "inappropriate range in type constructor: " <> unpack t
    show (InterpretConstructorUnknownError n) = "unknown constructor: " <> show n
    show (InterpretBindingsDuplicateError nn) = "duplicate bindings: " <> (intercalate ", " $ fmap show nn)

data ErrorMessage =
    MkErrorMessage SourcePos
                   ErrorType

instance Show ErrorMessage where
    show (MkErrorMessage spos err) =
        sourceName spos <> ":" <> show (sourceLine spos) <> ":" <> show (sourceColumn spos) <> ": " <> show err

parseErrorMessage :: ParseError -> ErrorMessage
parseErrorMessage err = MkErrorMessage (errorPos err) $ ParserError $ errorMessages err

newtype PinaforeError =
    MkPinaforeError [ErrorMessage]

instance Show PinaforeError where
    show (MkPinaforeError msgs) = intercalate "\n" $ fmap show msgs

instance Exception PinaforeError

type InterpretResult = Result [ErrorMessage]

ioRunInterpretResult :: MonadIO m => InterpretResult a -> m a
ioRunInterpretResult (SuccessResult a) = return a
ioRunInterpretResult (FailureResult err) = liftIO $ throw $ MkPinaforeError err
