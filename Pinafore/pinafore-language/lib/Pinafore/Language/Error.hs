module Pinafore.Language.Error where

import Data.Shim
import Language.Expression.Common
import Pinafore.Language.Name
import Shapes
import Shapes.Numeric
import Text.Parsec.Error
import Text.Parsec.Pos

data ErrorType
    = KnownIssueError Int
                      Text
    | UnicodeDecodeError Text
    | ParserError [Message]
    | ExpressionErrorError ExpressionError
    | LookupNamesUndefinedError (NonEmpty FullNameRef)
    | LookupNotDefinedError FullNameRef
    | LookupNotTypeError FullNameRef
    | LookupNotSpecialFormError FullNameRef
    | LookupNotConstructorError FullNameRef
    | LookupNotRecordConstructorError FullNameRef
    | SpecialFormWrongAnnotationsError FullNameRef
                                       [Text]
                                       [Text]
    | DeclareTypeDuplicateError FullName
    | DeclareConstructorDuplicateError FullNameRef
    | DeclareDynamicTypeCycleError (NonEmpty FullName)
    | DeclareDatatypeStorableSupertypeError FullName
    | DeclareDatatypeBadSupertypeError Text
    | DeclareDatatypeConstructorNotSupertypeError FullNameRef
                                                  Text
                                                  [Text]
    | DeclareDatatypeNoSupertypeConstructorError Text
    | DeclareDatatypeMultipleSupertypeConstructorsError Text
                                                        [Text]
    | DeclareDatatypePositionalConstructorWithSupertypeError
    | TypeConvertError Text
                       Polarity
                       Text
                       Polarity
    | NoGroundTypeConversionError Text
                                  Text
    | IncoherentGroundTypeConversionError Text
                                          Text
    | TypeNotInvertibleError Text
    | NotationBareUnquoteError
    | MatchesDifferentCount Natural
                            Natural
    | InterpretTypeExprBadLimitError Polarity
    | InterpretTypeExprBadJoinMeetError Polarity
    | InterpretTypeRecursionNotCovariant Name
                                         Text
    | InterpretTypeNotAmbipolarError Text
    | InterpretTypeNotGroundedError Text
    | InterpretTypeNotEntityError Text
    | InterpretTypeNotSimpleEntityError Text
    | InterpretTypeNotDynamicEntityError Text
    | InterpretTypeNotOpenEntityError Text
    | InterpretTypeNotConcreteDynamicEntityError Text
    | InterpretTypeNoneNotNegativeEntityError
    | InterpretTypeUnderApplyError Text
    | InterpretTypeOverApplyError Text
    | InterpretTypeRangeApplyError Text
    | InterpretBindingsDuplicateError (NonEmpty FullName)
    | InterpretTypeDeclDuplicateTypeVariablesError FullName
                                                   (NonEmpty Name)
    | InterpretTypeDeclUnboundTypeVariablesError FullName
                                                 (NonEmpty Name)
    | InterpretTypeDeclTypeVariableWrongPolarityError FullName
                                                      Name
    | InterpretTypeDeclTypeVariableNotCovariantError FullName
    | InterpretTypeDeclTypeStorableRecord
    | InterpretSubtypeInconsistent Text
                                   Text
    | ModuleNotFoundError ModuleName
    | ModuleCycleError (NonEmpty ModuleName)

instance Show ErrorType where
    show (KnownIssueError n "") = "issue #" <> show n
    show (KnownIssueError n t) = unpack t <> " (issue #" <> show n <> ")"
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
    show (LookupNamesUndefinedError nn) = "undefined names: " <> (intercalate ", " $ fmap show $ toList nn)
    show (LookupNotDefinedError n) = "undefined: " <> show n
    show (LookupNotTypeError n) = "name not type: " <> show n
    show (LookupNotSpecialFormError n) = "name not special form: " <> show n
    show (LookupNotConstructorError n) = "name not constructor: " <> show n
    show (LookupNotRecordConstructorError n) = "name not record constructor: " <> show n
    show (SpecialFormWrongAnnotationsError n expected found) =
        "wrong annotations for special form " <>
        show n <>
        ": expecting " <> intercalate " " (fmap unpack expected) <> ", found " <> intercalate " " (fmap unpack found)
    show (DeclareTypeDuplicateError n) = "duplicate type: " <> show n
    show (DeclareConstructorDuplicateError n) = "duplicate constructor: " <> show n
    show (DeclareDynamicTypeCycleError nn) =
        "cycle in dynamictype declarations: " <> (intercalate ", " $ fmap show $ toList nn)
    show (DeclareDatatypeStorableSupertypeError n) = "datatype storable has supertypes: " <> show n
    show (DeclareDatatypeBadSupertypeError t) = "bad supertype for datatype: " <> unpack t
    show (DeclareDatatypeConstructorNotSupertypeError c t ss) =
        "constructor " <> show c <> ": " <> unpack t <> " is not from supertypes " <> (unpack $ intercalate " & " ss)
    show (DeclareDatatypeNoSupertypeConstructorError t) = "no constructor defined for supertype " <> unpack t
    show (DeclareDatatypeMultipleSupertypeConstructorsError t cc) =
        "multiple constructors defined for supertype " <> unpack t <> ": " <> (unpack $ intercalate ", " cc)
    show DeclareDatatypePositionalConstructorWithSupertypeError =
        "positional constructor not allowed in datatype with supertype"
    show (TypeConvertError ta pa tb pb) =
        unpack $ "cannot convert " <> ta <> polaritySymbol pa <> " <: " <> tb <> polaritySymbol pb
    show (NoGroundTypeConversionError ta tb) = unpack $ "no ground conversion for " <> ta <> " <: " <> tb
    show (IncoherentGroundTypeConversionError ta tb) =
        unpack $ "incoherent ground conversions for " <> ta <> " <: " <> tb
    show (TypeNotInvertibleError t) = "cannot invert type " <> unpack t
    show NotationBareUnquoteError = "unquote outside WholeModel quote"
    show (MatchesDifferentCount expected found) =
        "different number of patterns in match, expected " <> show expected <> ", found " <> show found
    show (InterpretTypeExprBadLimitError Positive) = "\"Any\" in positive type"
    show (InterpretTypeExprBadLimitError Negative) = "\"None\" in negative type"
    show (InterpretTypeExprBadJoinMeetError Positive) = "\"&\" in positive type"
    show (InterpretTypeExprBadJoinMeetError Negative) = "\"|\" in negative type"
    show (InterpretTypeRecursionNotCovariant var tp) =
        "recursive variable " <> show var <> " is not covariant in type " <> unpack tp
    show (InterpretTypeNotAmbipolarError t) = unpack t <> " is not an ambipolar type"
    show (InterpretTypeNotGroundedError t) = unpack t <> " is not a grounded type"
    show (InterpretTypeNotEntityError t) = unpack t <> " is not an entity type"
    show (InterpretTypeNotSimpleEntityError t) = unpack t <> " is not a simple entity type"
    show (InterpretTypeNotDynamicEntityError t) = unpack t <> " is not a dynamic entity type"
    show (InterpretTypeNotOpenEntityError t) = unpack t <> " is not an open entity type"
    show (InterpretTypeNotConcreteDynamicEntityError t) = unpack t <> " is not a concrete dynamic entity type"
    show InterpretTypeNoneNotNegativeEntityError = "\"None\" is not a negative entity type"
    show (InterpretTypeUnderApplyError t) = "underapplied type constructor: " <> unpack t
    show (InterpretTypeOverApplyError t) = "overapplied type constructor: " <> unpack t
    show (InterpretTypeRangeApplyError t) = "inappropriate range in type constructor: " <> unpack t
    show (InterpretBindingsDuplicateError nn) = "duplicate bindings: " <> (intercalate ", " $ fmap show $ toList nn)
    show (InterpretTypeDeclDuplicateTypeVariablesError n vv) =
        "duplicate type variables in declaration of " <> show n <> ": " <> (intercalate ", " $ fmap show $ toList vv)
    show (InterpretTypeDeclUnboundTypeVariablesError n vv) =
        "unbound type variables in declaration of " <> show n <> ": " <> (intercalate ", " $ fmap show $ toList vv)
    show (InterpretTypeDeclTypeVariableWrongPolarityError n v) =
        "wrong polarity of type variable " <> show v <> " in declaration of " <> show n
    show (InterpretTypeDeclTypeVariableNotCovariantError n) =
        "type variable is not covariant in declaration of storable datatype " <> show n
    show InterpretTypeDeclTypeStorableRecord = "record constructor not allowed in storable datatype"
    show (InterpretSubtypeInconsistent ta tb) =
        "subtype relation is inconsistent with existing subtype relation " <> unpack ta <> " <: " <> unpack tb
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

rethrowCause :: (MonadCatch PinaforeError m, MonadThrow ErrorMessage m) => SourcePos -> ErrorType -> m a -> m a
rethrowCause spos err ma = catch ma $ \pe -> throw $ MkErrorMessage spos err pe

newtype InterpretResult a =
    MkInterpretResult (ResultT PinaforeError IO a)
    deriving ( Functor
             , Applicative
             , Alternative
             , Monad
             , MonadException
             , MonadPlus
             , MonadIO
             , MonadFix
             , MonadHoistIO
             , MonadTunnelIO
             )

instance MonadThrow PinaforeError InterpretResult where
    throw e = throwExc $ Left e

instance MonadCatch PinaforeError InterpretResult where
    catch ma ema =
        catchSomeExc ma $ \case
            Left e -> fmap Just $ ema e
            Right _ -> return Nothing

instance MonadThrow ErrorMessage InterpretResult where
    throw err = throw $ MkPinaforeError [err]

runInterpretResult :: MonadIO m => InterpretResult a -> m (Result PinaforeError a)
runInterpretResult (MkInterpretResult ir) = liftIO $ runResultT ir

fromInterpretResult ::
       forall m a. (MonadThrow PinaforeError m, MonadIO m)
    => InterpretResult a
    -> m a
fromInterpretResult ir = do
    result <- runInterpretResult ir
    fromResult result
