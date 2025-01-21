module Pinafore.Language.Error where

import Import

data QErrorType
    = InternalError
        (Maybe Int)
        NamedText
    | UnicodeDecodeError NamedText
    | ParserError ParseErrorType
    | PatternErrorError PatternError
    | ExpressionUndefinedError (NonEmpty (FullNameRef, NamedText))
    | ExpressionUnimpliedError (NonEmpty (ImplicitName, NamedText))
    | LookupNamesUndefinedError (NonEmpty FullNameRef)
    | LookupNotDefinedError FullNameRef
    | LookupNotValueError FullNameRef
    | LookupNotTypeError FullNameRef
    | LookupNotSpecialFormError FullNameRef
    | LookupNotConstructorError FullNameRef
    | LookupNotRecordError FullNameRef
    | LookupNotRecordConstructorError FullNameRef
    | SpecialFormWrongAnnotationsError
        FullNameRef
        [NamedText]
        [NamedText]
    | DeclareBindingDuplicateError FullName
    | DeclareConstructorDuplicateError FullNameRef
    | DeclareDatatypeBadSupertypeError NamedText
    | DeclareDatatypeConstructorNotSupertypeError
        FullNameRef
        NamedText
        [NamedText]
    | DeclareDatatypeNoSupertypeConstructorError NamedText
    | DeclareDatatypeMultipleSupertypeConstructorsError
        NamedText
        [NamedText]
    | DeclareDatatypePositionalConstructorWithSupertypeError
    | DeclareDatatypeMissingSupertypeMember Name
    | DeclareDatatypeDuplicateMembers Name
    | DeclareDatatypeDuplicateInheritedMembers Name
    | RecordConstructorMissingName Name
    | RecordConstructorExtraName Name
    | RecordFunctionSupertype
    | TypeConvertError
        NamedText
        Polarity
        NamedText
        Polarity
    | NoGroundTypeConversionError
        NamedText
        NamedText
    | IncoherentGroundTypeConversionError
        NamedText
        NamedText
    | TypeNotInvertibleError NamedText
    | NotationBareUnquoteError
    | InterpretTypeExprBadLimitError Polarity
    | InterpretTypeExprBadJoinMeetError Polarity
    | InterpretTypeRecursionNotCovariant
        Name
        NamedText
    | InterpretTypeRecursionImmediate
        Name
        NamedText
    | InterpretTypeNotAmbipolarError NamedText
    | InterpretTypeNotGroundedError NamedText
    | InterpretTypeNotStorableError NamedText
    | InterpretTypeNotSimpleEntityError NamedText
    | InterpretTypeNotOpenEntityError NamedText
    | InterpretTypeUnderApplyError NamedText
    | InterpretTypeOverApplyError NamedText
    | InterpretTypeRangeApplyError NamedText
    | InterpretBindingsDuplicateError (NonEmpty FullName)
    | InterpretTypeDeclDuplicateTypeVariablesError
        FullName
        (NonEmpty Name)
    | InterpretTypeDeclUnboundTypeVariablesError
        FullName
        (NonEmpty Name)
    | InterpretTypeDeclTypeVariableWrongPolarityError
        FullName
        Name
    | InterpretTypeDeclTypeVariableNotCovariantError FullName
    | InterpretTypeDeclTypeStorableRecord
    | InterpretSubtypeInconsistent
        NamedText
        NamedText
    | ModuleNotFoundError ModuleName
    | ModuleCycleError (NonEmpty ModuleName)

instance ShowNamedText QErrorType where
    showNamedText (InternalError mn nt) =
        bindNamedText nt $ \t ->
            case (mn, t) of
                (Just n, "") -> "INTERNAL ERROR: issue #" <> showNamedText n
                (Just n, _) -> "INTERNAL ERROR: " <> toNamedText t <> " (issue #" <> showNamedText n <> ")"
                (Nothing, "") -> "INTERNAL ERROR"
                (Nothing, _) -> "INTERNAL ERROR: " <> toNamedText t
    showNamedText (UnicodeDecodeError t) = "Unicode decode error: " <> t
    showNamedText (ParserError err) = showNamedText err
    showNamedText (PatternErrorError e) = showNamedText e
    showNamedText (ExpressionUndefinedError nn) =
        "undefined: "
            <> intercalate
                ", "
                (fmap (\(n, t) -> showNamedText n <> ": " <> toNamedText t) $ nubBy (\x y -> fst x == fst y) $ toList nn)
    showNamedText (ExpressionUnimpliedError nn) =
        "unimplied: "
            <> intercalate
                ", "
                (fmap (\(n, t) -> showNamedText n <> ": " <> toNamedText t) $ nubBy (\x y -> fst x == fst y) $ toList nn)
    showNamedText (LookupNamesUndefinedError nn) =
        "undefined names: " <> (intercalate ", " $ fmap showNamedText $ toList nn)
    showNamedText (LookupNotDefinedError n) = "undefined: " <> showNamedText n
    showNamedText (LookupNotValueError n) = "name not value: " <> showNamedText n
    showNamedText (LookupNotTypeError n) = "name not type: " <> showNamedText n
    showNamedText (LookupNotSpecialFormError n) = "name not special form: " <> showNamedText n
    showNamedText (LookupNotConstructorError n) = "name not constructor: " <> showNamedText n
    showNamedText (LookupNotRecordError n) = "name not record: " <> showNamedText n
    showNamedText (LookupNotRecordConstructorError n) = "name not record constructor: " <> showNamedText n
    showNamedText (SpecialFormWrongAnnotationsError n expected found) =
        "wrong annotations for special form "
            <> showNamedText n
            <> ": expecting "
            <> intercalate " " expected
            <> ", found "
            <> intercalate " " found
    showNamedText (DeclareBindingDuplicateError n) = "duplicate binding: " <> showNamedText n
    showNamedText (DeclareConstructorDuplicateError n) = "duplicate constructor: " <> showNamedText n
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
    showNamedText RecordFunctionSupertype = "supertype in record function declaration"
    showNamedText (TypeConvertError ta pa tb pb) =
        "cannot convert " <> ta <> toNamedText (polaritySymbol pa) <> " <: " <> tb <> toNamedText (polaritySymbol pb)
    showNamedText (NoGroundTypeConversionError ta tb) = "no ground conversion for " <> ta <> " <: " <> tb
    showNamedText (IncoherentGroundTypeConversionError ta tb) =
        "incoherent ground conversions for " <> ta <> " <: " <> tb
    showNamedText (TypeNotInvertibleError t) = "cannot invert type " <> t
    showNamedText NotationBareUnquoteError = "unquote outside WholeModel quote"
    showNamedText (InterpretTypeExprBadLimitError Positive) = "\"Any\" in positive type"
    showNamedText (InterpretTypeExprBadLimitError Negative) = "\"None\" in negative type"
    showNamedText (InterpretTypeExprBadJoinMeetError Positive) = "\"&\" in positive type"
    showNamedText (InterpretTypeExprBadJoinMeetError Negative) = "\"|\" in negative type"
    showNamedText (InterpretTypeRecursionNotCovariant var tp) =
        "recursive variable " <> showNamedText var <> " is not covariant in type " <> tp
    showNamedText (InterpretTypeRecursionImmediate var tp) =
        "recursive variable " <> showNamedText var <> " is used immediately in type " <> tp
    showNamedText (InterpretTypeNotAmbipolarError t) = t <> " is not an ambipolar type"
    showNamedText (InterpretTypeNotGroundedError t) = t <> " is not a grounded type"
    showNamedText (InterpretTypeNotStorableError t) = t <> " is not a storable type"
    showNamedText (InterpretTypeNotSimpleEntityError t) = t <> " is not a simple entity type"
    showNamedText (InterpretTypeNotOpenEntityError t) = t <> " is not an open entity type"
    showNamedText (InterpretTypeUnderApplyError t) = "underapplied type constructor: " <> t
    showNamedText (InterpretTypeOverApplyError t) = "overapplied type constructor: " <> t
    showNamedText (InterpretTypeRangeApplyError t) = "inappropriate range in type constructor: " <> t
    showNamedText (InterpretBindingsDuplicateError nn) =
        "duplicate bindings: " <> (intercalate ", " $ fmap showNamedText $ toList nn)
    showNamedText (InterpretTypeDeclDuplicateTypeVariablesError n vv) =
        "duplicate type variables in declaration of "
            <> showNamedText n
            <> ": "
            <> (intercalate ", " $ fmap showNamedText $ toList vv)
    showNamedText (InterpretTypeDeclUnboundTypeVariablesError n vv) =
        "unbound type variables in declaration of "
            <> showNamedText n
            <> ": "
            <> (intercalate ", " $ fmap showNamedText $ toList vv)
    showNamedText (InterpretTypeDeclTypeVariableWrongPolarityError n v) =
        "wrong polarity of type variable " <> showNamedText v <> " in declaration of " <> showNamedText n
    showNamedText (InterpretTypeDeclTypeVariableNotCovariantError n) =
        "type variable is not covariant in declaration of storable datatype " <> showNamedText n
    showNamedText InterpretTypeDeclTypeStorableRecord = "record constructor not allowed in storable datatype"
    showNamedText (InterpretSubtypeInconsistent ta tb) =
        "subtype relation is inconsistent with existing subtype relation " <> ta <> " <: " <> tb
    showNamedText (ModuleNotFoundError mname) = "can't find module " <> showNamedText mname
    showNamedText (ModuleCycleError nn) = "cycle in modules: " <> (intercalate ", " $ fmap showNamedText $ toList nn)

type QError = SourceError QErrorType

instance Show QError where
    show = unpack . showText

instance Exception QError

fromParseResult :: MonadThrow QError m => ParseResult --> m
fromParseResult pr = fromResult $ mapResultFailure (fmap ParserError) pr

newtype InterpretResult a = MkInterpretResult
    { unInterpretResult :: ResultT QError IO a
    }
    deriving newtype (Functor, Applicative, Monad, MonadException, MonadIO, MonadFix, MonadHoistIO, MonadTunnelIO)

instance MonadCoroutine InterpretResult where
    coroutineSuspend pqmr =
        hoist MkInterpretResult $ coroutineSuspend $ \pmq -> unInterpretResult $ pqmr $ \p -> MkInterpretResult $ pmq p

instance MonadThrow QError InterpretResult where
    throw e = throwExc $ Left e

instance MonadCatch QError InterpretResult where
    catch ma ema =
        catchSomeExc ma $ \case
            Left e -> fmap Just $ ema e
            Right _ -> return Nothing

runInterpretResult :: MonadIO m => InterpretResult a -> m (Result QError a)
runInterpretResult (MkInterpretResult ir) = liftIO $ runResultT ir

fromInterpretResult ::
    forall m a.
    (MonadThrow QError m, MonadIO m) =>
    InterpretResult a ->
    m a
fromInterpretResult ir = do
    result <- runInterpretResult ir
    fromResult result
