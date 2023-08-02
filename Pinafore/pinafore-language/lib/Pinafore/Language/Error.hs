module Pinafore.Language.Error where

import Data.Shim
import Language.Expression.Common
import Pinafore.Language.Name
import Pinafore.Text
import Shapes
import Shapes.Numeric
import Text.Parsec.Error
import Text.Parsec.Pos

data ErrorType
    = KnownIssueError Int
                      NamedText
    | UnicodeDecodeError NamedText
    | ParserError [Message]
    | PatternErrorError PatternError
    | ExpressionErrorError (NonEmpty (FullNameRef, NamedText))
    | LookupNamesUndefinedError (NonEmpty FullNameRef)
    | LookupNotDefinedError FullNameRef
    | LookupNotTypeError FullNameRef
    | LookupNotSpecialFormError FullNameRef
    | LookupNotConstructorError FullNameRef
    | LookupNotRecordConstructorError FullNameRef
    | SpecialFormWrongAnnotationsError FullNameRef
                                       [NamedText]
                                       [NamedText]
    | DeclareBindingDuplicateError FullName
    | DeclareConstructorDuplicateError FullNameRef
    | DeclareDynamicTypeCycleError (NonEmpty FullName)
    | DeclareDatatypeStorableSupertypeError FullName
    | DeclareDatatypeBadSupertypeError NamedText
    | DeclareDatatypeConstructorNotSupertypeError FullNameRef
                                                  NamedText
                                                  [NamedText]
    | DeclareDatatypeNoSupertypeConstructorError NamedText
    | DeclareDatatypeMultipleSupertypeConstructorsError NamedText
                                                        [NamedText]
    | DeclareDatatypePositionalConstructorWithSupertypeError
    | DeclareDatatypeMissingSupertypeMember Name
    | DeclareDatatypeDuplicateMembers Name
    | DeclareDatatypeDuplicateInheritedMembers Name
    | RecordConstructorMissingName Name
    | RecordConstructorExtraName Name
    | TypeConvertError NamedText
                       Polarity
                       NamedText
                       Polarity
    | NoGroundTypeConversionError NamedText
                                  NamedText
    | IncoherentGroundTypeConversionError NamedText
                                          NamedText
    | TypeNotInvertibleError NamedText
    | NotationBareUnquoteError
    | MatchesDifferentCount Natural
                            Natural
    | InterpretTypeExprBadLimitError Polarity
    | InterpretTypeExprBadJoinMeetError Polarity
    | InterpretTypeRecursionNotCovariant Name
                                         NamedText
    | InterpretTypeNotAmbipolarError NamedText
    | InterpretTypeNotGroundedError NamedText
    | InterpretTypeNotEntityError NamedText
    | InterpretTypeNotSimpleEntityError NamedText
    | InterpretTypeNotDynamicEntityError NamedText
    | InterpretTypeNotOpenEntityError NamedText
    | InterpretTypeNotConcreteDynamicEntityError NamedText
    | InterpretTypeNoneNotNegativeEntityError
    | InterpretTypeUnderApplyError NamedText
    | InterpretTypeOverApplyError NamedText
    | InterpretTypeRangeApplyError NamedText
    | InterpretBindingsDuplicateError (NonEmpty FullName)
    | InterpretTypeDeclDuplicateTypeVariablesError FullName
                                                   (NonEmpty Name)
    | InterpretTypeDeclUnboundTypeVariablesError FullName
                                                 (NonEmpty Name)
    | InterpretTypeDeclTypeVariableWrongPolarityError FullName
                                                      Name
    | InterpretTypeDeclTypeVariableNotCovariantError FullName
    | InterpretTypeDeclTypeStorableRecord
    | InterpretSubtypeInconsistent NamedText
                                   NamedText
    | ModuleNotFoundError ModuleName
    | ModuleCycleError (NonEmpty ModuleName)

instance ShowNamedText ErrorType where
    showNamedText (KnownIssueError n nt) =
        bindNamedText nt $ \t ->
            case t of
                "" -> "issue #" <> showNamedText n
                _ -> toNamedText t <> " (issue #" <> showNamedText n <> ")"
    showNamedText (UnicodeDecodeError t) = "Unicode decode error: " <> t
    showNamedText (ParserError msgs) = let
        getMsgs :: (Message -> Maybe String) -> [Text]
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
    showNamedText (PatternErrorError e) = showNamedText e
    showNamedText (ExpressionErrorError nn) =
        "undefined: " <>
        intercalate
            ", "
            (fmap (\(n, t) -> showNamedText n <> ": " <> toNamedText t) $ nubBy (\x y -> fst x == fst y) $ toList nn)
    showNamedText (LookupNamesUndefinedError nn) =
        "undefined names: " <> (intercalate ", " $ fmap showNamedText $ toList nn)
    showNamedText (LookupNotDefinedError n) = "undefined: " <> showNamedText n
    showNamedText (LookupNotTypeError n) = "name not type: " <> showNamedText n
    showNamedText (LookupNotSpecialFormError n) = "name not special form: " <> showNamedText n
    showNamedText (LookupNotConstructorError n) = "name not constructor: " <> showNamedText n
    showNamedText (LookupNotRecordConstructorError n) = "name not record constructor: " <> showNamedText n
    showNamedText (SpecialFormWrongAnnotationsError n expected found) =
        "wrong annotations for special form " <>
        showNamedText n <> ": expecting " <> intercalate " " expected <> ", found " <> intercalate " " found
    showNamedText (DeclareBindingDuplicateError n) = "duplicate binding: " <> showNamedText n
    showNamedText (DeclareConstructorDuplicateError n) = "duplicate constructor: " <> showNamedText n
    showNamedText (DeclareDynamicTypeCycleError nn) =
        "cycle in dynamictype declarations: " <> (intercalate ", " $ fmap showNamedText $ toList nn)
    showNamedText (DeclareDatatypeStorableSupertypeError n) = "datatype storable has supertypes: " <> showNamedText n
    showNamedText (DeclareDatatypeBadSupertypeError t) = "bad supertype for datatype: " <> t
    showNamedText (DeclareDatatypeConstructorNotSupertypeError c t ss) =
        "constructor " <> showNamedText c <> ": " <> t <> " is not from supertypes " <> (intercalate " & " ss)
    showNamedText (DeclareDatatypeNoSupertypeConstructorError t) = "no constructor defined for supertype " <> t
    showNamedText (DeclareDatatypeMultipleSupertypeConstructorsError t cc) =
        "multiple constructors defined for supertype " <> t <> ": " <> (intercalate ", " cc)
    showNamedText DeclareDatatypePositionalConstructorWithSupertypeError =
        "positional constructor not allowed in datatype with supertype"
    showNamedText (DeclareDatatypeMissingSupertypeMember t) = "missing member for supertype: " <> showNamedText t
    showNamedText (DeclareDatatypeDuplicateMembers m) = "duplicate member declarations for " <> showNamedText m
    showNamedText (DeclareDatatypeDuplicateInheritedMembers m) =
        "multiple inherited member declarations for " <> showNamedText m
    showNamedText (RecordConstructorMissingName n) = "missing name for record constructor: " <> showNamedText n
    showNamedText (RecordConstructorExtraName n) = "extra name for record constructor: " <> showNamedText n
    showNamedText (TypeConvertError ta pa tb pb) =
        "cannot convert " <> ta <> toNamedText (polaritySymbol pa) <> " <: " <> tb <> toNamedText (polaritySymbol pb)
    showNamedText (NoGroundTypeConversionError ta tb) = "no ground conversion for " <> ta <> " <: " <> tb
    showNamedText (IncoherentGroundTypeConversionError ta tb) =
        "incoherent ground conversions for " <> ta <> " <: " <> tb
    showNamedText (TypeNotInvertibleError t) = "cannot invert type " <> t
    showNamedText NotationBareUnquoteError = "unquote outside WholeModel quote"
    showNamedText (MatchesDifferentCount expected found) =
        "different number of patterns in match, expected " <>
        showNamedText expected <> ", found " <> showNamedText found
    showNamedText (InterpretTypeExprBadLimitError Positive) = "\"Any\" in positive type"
    showNamedText (InterpretTypeExprBadLimitError Negative) = "\"None\" in negative type"
    showNamedText (InterpretTypeExprBadJoinMeetError Positive) = "\"&\" in positive type"
    showNamedText (InterpretTypeExprBadJoinMeetError Negative) = "\"|\" in negative type"
    showNamedText (InterpretTypeRecursionNotCovariant var tp) =
        "recursive variable " <> showNamedText var <> " is not covariant in type " <> tp
    showNamedText (InterpretTypeNotAmbipolarError t) = t <> " is not an ambipolar type"
    showNamedText (InterpretTypeNotGroundedError t) = t <> " is not a grounded type"
    showNamedText (InterpretTypeNotEntityError t) = t <> " is not an entity type"
    showNamedText (InterpretTypeNotSimpleEntityError t) = t <> " is not a simple entity type"
    showNamedText (InterpretTypeNotDynamicEntityError t) = t <> " is not a dynamic entity type"
    showNamedText (InterpretTypeNotOpenEntityError t) = t <> " is not an open entity type"
    showNamedText (InterpretTypeNotConcreteDynamicEntityError t) = t <> " is not a concrete dynamic entity type"
    showNamedText InterpretTypeNoneNotNegativeEntityError = "\"None\" is not a negative entity type"
    showNamedText (InterpretTypeUnderApplyError t) = "underapplied type constructor: " <> t
    showNamedText (InterpretTypeOverApplyError t) = "overapplied type constructor: " <> t
    showNamedText (InterpretTypeRangeApplyError t) = "inappropriate range in type constructor: " <> t
    showNamedText (InterpretBindingsDuplicateError nn) =
        "duplicate bindings: " <> (intercalate ", " $ fmap showNamedText $ toList nn)
    showNamedText (InterpretTypeDeclDuplicateTypeVariablesError n vv) =
        "duplicate type variables in declaration of " <>
        showNamedText n <> ": " <> (intercalate ", " $ fmap showNamedText $ toList vv)
    showNamedText (InterpretTypeDeclUnboundTypeVariablesError n vv) =
        "unbound type variables in declaration of " <>
        showNamedText n <> ": " <> (intercalate ", " $ fmap showNamedText $ toList vv)
    showNamedText (InterpretTypeDeclTypeVariableWrongPolarityError n v) =
        "wrong polarity of type variable " <> showNamedText v <> " in declaration of " <> showNamedText n
    showNamedText (InterpretTypeDeclTypeVariableNotCovariantError n) =
        "type variable is not covariant in declaration of storable datatype " <> showNamedText n
    showNamedText InterpretTypeDeclTypeStorableRecord = "record constructor not allowed in storable datatype"
    showNamedText (InterpretSubtypeInconsistent ta tb) =
        "subtype relation is inconsistent with existing subtype relation " <> ta <> " <: " <> tb
    showNamedText (ModuleNotFoundError mname) = "can't find module " <> showNamedText mname
    showNamedText (ModuleCycleError nn) = "cycle in modules: " <> (intercalate ", " $ fmap showNamedText $ toList nn)

data ErrorMessage =
    MkErrorMessage SourcePos
                   (NamedText -> Text)
                   ErrorType
                   PinaforeError

showSourceError :: SourcePos -> Text -> Text
showSourceError spos s =
    toText (sourceName spos) <>
    ":" <> toText (show (sourceLine spos)) <> ":" <> toText (show (sourceColumn spos)) <> ": " <> s

showIndentErrorMessage :: Int -> ErrorMessage -> Text
showIndentErrorMessage n (MkErrorMessage spos ntt err pe) =
    replicate n ' ' <> (showSourceError spos $ ntt (showNamedText err) <> showIndentPinaforeError (succ n) pe)

showIndentPinaforeError :: Int -> PinaforeError -> Text
showIndentPinaforeError n (MkPinaforeError ems) = let
    showMsg em = "\n" <> showIndentErrorMessage n em
    in mconcat $ fmap showMsg ems

instance ShowText ErrorMessage where
    showText = showIndentErrorMessage 0

parseErrorMessage :: ParseError -> ErrorMessage
parseErrorMessage err = MkErrorMessage (errorPos err) toText (ParserError $ errorMessages err) mempty

newtype PinaforeError =
    MkPinaforeError [ErrorMessage]
    deriving (Semigroup, Monoid)

instance ShowText PinaforeError where
    showText (MkPinaforeError msgs) = intercalate "\n" $ fmap showText msgs

instance Show PinaforeError where
    show = unpack . showText

instance Exception PinaforeError

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
