module Pinafore.Language.Interpreter.Binding
    ( QSpecialVals(..)
    , QSignature(..)
    , QRecordValue(..)
    , QRecordConstructor(..)
    , recordConstructorToValue
    , QSpecialForm(..)
    , QInterpreterBinding(..)
    , QBindingInfo(..)
    , BindingSelector(..)
    , typeBindingSelector
    , recordConstructorBindingSelector
    , recordValueBindingSelector
    , specialFormBindingSelector
    ) where

import Import
import Pinafore.Language.Error
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Subtype ()

newtype QSpecialVals = MkQSpecialVals
    { specialEvaluate :: forall t. QType 'Positive t -> Text -> IO (Result QError t)
        -- ^ in IO because this can do things like import files
    }

data QSignature (polarity :: Polarity) (t :: Type) =
    ValueSignature (Maybe SomeFamilialType)
                   Name
                   (QType polarity t)
                   (Maybe (QOpenExpression t))

instance Is PolarityType polarity => Show (QSignature polarity a) where
    show (ValueSignature _ name t mexpr) =
        show name <>
        ": " <>
        show t <>
        case mexpr of
            Nothing -> ""
            Just expr -> " = " <> show expr

instance (HasInterpreter, Is PolarityType polarity) => HasVarMapping (QSignature polarity) where
    getVarMapping (ValueSignature _ _ t _) = getVarMapping t

data QRecordValue =
    forall (tt :: [Type]). MkQRecordValue (ListType (QSignature 'Positive) tt)
                                          (QFExpression ((->) (ListProduct tt)))

instance Show QRecordValue where
    show (MkQRecordValue sigs expr) = "of " <> show sigs <> ": " <> show expr

data QRecordConstructor =
    forall (t :: Type) (tt :: [Type]). MkQRecordConstructor (ListVType (QSignature 'Positive) tt)
                                                            (QGroundedShimWit 'Positive t)
                                                            (QGroundedShimWit 'Negative t)
                                                            (Codec t (ListVProduct tt))

recordConstructorToValue :: QRecordConstructor -> QRecordValue
recordConstructorToValue (MkQRecordConstructor sigs vtype _ codec) =
    MkQRecordValue (listVTypeToType sigs) $
    constSealedFExpression $ MkSomeFor (shimWitToDolan vtype) (encode codec . listProductToVProduct sigs)

type QSpecialForm :: Type
data QSpecialForm =
    forall lt. MkQSpecialForm (ListType QAnnotation lt)
                              (ListProduct lt -> Interpreter QExpression)

data QInterpreterBinding
    = ValueBinding QExpression
    | RecordValueBinding QRecordValue
    | PatternConstructorBinding QExpression
                                QPatternConstructor
    | RecordConstructorBinding QRecordConstructor
    | TypeBinding QSomeGroundType
    | SpecialFormBinding QSpecialForm

instance HasInterpreter => Show QInterpreterBinding where
    show (ValueBinding e) = "val: " <> show e
    show (RecordValueBinding _) = "recordval"
    show (PatternConstructorBinding e _) = "cons: " <> show e
    show (RecordConstructorBinding _) = "recordcons"
    show (TypeBinding t) = "type: " <> exprShowShow t
    show (SpecialFormBinding _) = "special"

data QBindingInfo = MkQBindingInfo
    { biOriginalName :: FullName
    , biDocumentation :: DefDoc
    , biValue :: QInterpreterBinding
    }

data BindingSelector t = MkBindingSelector
    { bsEncode :: t -> QInterpreterBinding
    , bsDecode :: QInterpreterBinding -> Maybe t
    , bsError :: FullNameRef -> QErrorType
    }

typeBindingSelector :: BindingSelector QSomeGroundType
typeBindingSelector = let
    bsEncode = TypeBinding
    bsDecode (TypeBinding t) = Just t
    bsDecode _ = Nothing
    bsError = LookupNotTypeError
    in MkBindingSelector {..}

recordConstructorBindingSelector :: BindingSelector QRecordConstructor
recordConstructorBindingSelector = let
    bsEncode = RecordConstructorBinding
    bsDecode (RecordConstructorBinding t) = Just t
    bsDecode _ = Nothing
    bsError = LookupNotRecordConstructorError
    in MkBindingSelector {..}

recordValueBindingSelector :: BindingSelector QRecordValue
recordValueBindingSelector = let
    bsEncode = RecordValueBinding
    bsDecode (RecordValueBinding t) = Just t
    bsDecode (RecordConstructorBinding t) = Just $ recordConstructorToValue t
    bsDecode _ = Nothing
    bsError = LookupNotRecordError
    in MkBindingSelector {..}

specialFormBindingSelector :: BindingSelector QSpecialForm
specialFormBindingSelector = let
    bsEncode = SpecialFormBinding
    bsDecode (SpecialFormBinding t) = Just t
    bsDecode _ = Nothing
    bsError = LookupNotSpecialFormError
    in MkBindingSelector {..}
