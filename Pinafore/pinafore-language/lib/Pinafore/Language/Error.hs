module Pinafore.Language.Error where

import Data.Shim
import Language.Expression.Common
import Pinafore.Language.Name
import Shapes
import Text.Parsec.Error
import Text.Parsec.Pos

data ErrorType
    = UnicodeDecodeError Text
    | ParserError [Message]
    | ExpressionErrorError ExpressionError
    | LookupNamesUnknownError (NonEmpty Name)
    | LookupRefNameUnknownError ReferenceName
    | LookupTypeUnknownError ReferenceName
    | LookupSpecialFormUnknownError ReferenceName
    | SpecialFormWrongAnnotationsError ReferenceName
                                       [Text]
                                       [Text]
    | TypeNotOpenEntityError Text
    | TypeNotSimpleEntityError Text
    | LookupConstructorUnknownError ReferenceName
    | DeclareTypeDuplicateError Name
    | DeclareConstructorDuplicateError Name
    | DeclareDynamicTypeCycleError (NonEmpty Name)
    | TypeConvertError Text
                       (Maybe Polarity)
                       Text
                       (Maybe Polarity)
    | TypeNotInvertibleError Text
    | NotationBareUnquoteError
    | InterpretTypeExprBadLimitError Polarity
    | InterpretTypeExprBadJoinMeetError Polarity
    | InterpretTypeNotAmbipolarError Text
    | InterpretTypeNotEntityError Text
    | InterpretTypeNotDynamicEntityError Text
    | InterpretTypeNotOpenEntityError Text
    | InterpretTypeNotConcreteDynamicEntityError Text
    | InterpretTypeNoneNotNegativeEntityError
    | InterpretTypeUnderApplyError Text
    | InterpretTypeOverApplyError Text
    | InterpretTypeRangeApplyError Text
    | InterpretConstructorUnknownError ReferenceName
    | InterpretBindingsDuplicateError (NonEmpty Name)
    | InterpretTypeDeclDuplicateTypeVariablesError Name
                                                   (NonEmpty Name)
    | InterpretTypeDeclUnboundTypeVariablesError Name
                                                 (NonEmpty Name)
    | ModuleNotFoundError ModuleName
    | ModuleCycleError (NonEmpty ModuleName)

instance Show ErrorType where
    show (UnicodeDecodeError t) = "Unicode decode error: " <> unpack t
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
    show (LookupNamesUnknownError nn) = "unknown names: " <> (intercalate ", " $ fmap show $ toList nn)
    show (LookupRefNameUnknownError n) = "undefined: " <> show n
    show (LookupTypeUnknownError n) = "unknown type: " <> show n
    show (LookupSpecialFormUnknownError n) = "unknown special form: " <> show n
    show (SpecialFormWrongAnnotationsError n exp found) =
        "wrong annotations for special form " <>
        show n <>
        ": expecting " <> intercalate " " (fmap unpack exp) <> ", found " <> intercalate " " (fmap unpack found)
    show (TypeNotOpenEntityError t) = unpack t <> " is not an open entity type"
    show (TypeNotSimpleEntityError t) = unpack t <> " is not a simple entity type"
    show (LookupConstructorUnknownError n) = "unknown constructor: " <> show n
    show (DeclareTypeDuplicateError n) = "duplicate type: " <> show n
    show (DeclareConstructorDuplicateError n) = "duplicate constructor: " <> show n
    show (DeclareDynamicTypeCycleError nn) =
        "cycle in dynamictype declarations: " <> (intercalate ", " $ fmap show $ toList nn)
    show (TypeConvertError ta pa tb pb) =
        unpack $ "cannot convert " <> ta <> maybe "" polaritySymbol pa <> " <: " <> tb <> maybe "" polaritySymbol pb
    show (TypeNotInvertibleError t) = "cannot invert type " <> unpack t
    show NotationBareUnquoteError = "unquote outside WholeRef quote"
    show (InterpretTypeExprBadLimitError Positive) = "\"Any\" in positive type"
    show (InterpretTypeExprBadLimitError Negative) = "\"None\" in negative type"
    show (InterpretTypeExprBadJoinMeetError Positive) = "\"&\" in positive type"
    show (InterpretTypeExprBadJoinMeetError Negative) = "\"|\" in negative type"
    show (InterpretTypeNotAmbipolarError t) = unpack t <> " is not an ambipolar type"
    show (InterpretTypeNotEntityError t) = unpack t <> " is not an entity type"
    show (InterpretTypeNotDynamicEntityError t) = unpack t <> " is not a dynamic entity type"
    show (InterpretTypeNotOpenEntityError t) = unpack t <> " is not an open entity type"
    show (InterpretTypeNotConcreteDynamicEntityError t) = unpack t <> " is not a concrete dynamic entity type"
    show InterpretTypeNoneNotNegativeEntityError = "\"None\" is not a negative entity type"
    show (InterpretTypeUnderApplyError t) = "underapplied type constuctor: " <> unpack t
    show (InterpretTypeOverApplyError t) = "overapplied type constuctor: " <> unpack t
    show (InterpretTypeRangeApplyError t) = "inappropriate range in type constructor: " <> unpack t
    show (InterpretConstructorUnknownError n) = "unknown constructor: " <> show n
    show (InterpretBindingsDuplicateError nn) = "duplicate bindings: " <> (intercalate ", " $ fmap show $ toList nn)
    show (InterpretTypeDeclDuplicateTypeVariablesError n vv) =
        "duplicate type variables in declaration of " <> show n <> ": " <> (intercalate ", " $ fmap show $ toList vv)
    show (InterpretTypeDeclUnboundTypeVariablesError n vv) =
        "unbound type variables in declaration of " <> show n <> ": " <> (intercalate ", " $ fmap show $ toList vv)
    show (ModuleNotFoundError mname) = "can't find module " <> show mname
    show (ModuleCycleError nn) = "cycle in modules: " <> (intercalate ", " $ fmap show $ toList nn)

data ErrorMessage =
    MkErrorMessage SourcePos
                   ErrorType
                   PinaforeError

showSourceError :: SourcePos -> String -> String
showSourceError spos s =
    sourceName spos <> ":" <> show (sourceLine spos) <> ":" <> show (sourceColumn spos) <> ": " <> s

showIndentErrorMessage :: Int -> ErrorMessage -> String
showIndentErrorMessage n (MkErrorMessage spos err pe) =
    replicate n ' ' <> (showSourceError spos $ show err <> showIndentPinaforeError (succ n) pe)

showIndentPinaforeError :: Int -> PinaforeError -> String
showIndentPinaforeError n (MkPinaforeError ems) = let
    showMsg em = "\n" <> showIndentErrorMessage n em
    in mconcat $ fmap showMsg ems

instance Show ErrorMessage where
    show = showIndentErrorMessage 0

parseErrorMessage :: ParseError -> ErrorMessage
parseErrorMessage err = MkErrorMessage (errorPos err) (ParserError $ errorMessages err) mempty

newtype PinaforeError =
    MkPinaforeError [ErrorMessage]
    deriving (Semigroup, Monoid)

instance Show PinaforeError where
    show (MkPinaforeError msgs) = intercalate "\n" $ fmap show msgs

instance Exception PinaforeError

rethrowCause :: MonadCatch PinaforeError m => SourcePos -> ErrorType -> m a -> m a
rethrowCause spos err ma = catch ma $ \pe -> throwErrorMessage $ MkErrorMessage spos err pe

type InterpretResult = ExceptT PinaforeError IO

runInterpretResult :: MonadIO m => InterpretResult a -> m (Result PinaforeError a)
runInterpretResult ir = fmap eitherToResult $ liftIO $ runExceptT ir

throwErrorMessage :: MonadThrow PinaforeError m => ErrorMessage -> m a
throwErrorMessage e = throw $ MkPinaforeError [e]

throwInterpretResult ::
       forall m a. (MonadThrow PinaforeError m, MonadIO m)
    => InterpretResult a
    -> m a
throwInterpretResult ir = do
    result <- runInterpretResult ir
    throwResult result
