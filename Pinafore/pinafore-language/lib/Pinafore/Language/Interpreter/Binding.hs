module Pinafore.Language.Interpreter.Binding
    ( QSignature (..)
    , QRecordValue (..)
    , QRecordConstructor (..)
    , recordConstructorToValue
    , QInterpreterBinding (..)
    , QBindingInfo (..)
    , BindingSelector (..)
    , typeBindingSelector
    , recordConstructorBindingSelector
    , recordValueBindingSelector
    )
where

import Import
import Pinafore.Language.Error
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Subtype ()

data QSignature (polarity :: Polarity) (t :: Type)
    = ValueSignature
        (Maybe SomeFamilialType)
        Name
        (QType polarity t)
        (Maybe (QOpenExpression t))

instance Is PolarityType polarity => Show (QSignature polarity a) where
    show (ValueSignature _ name t mexpr) =
        show name
            <> ": "
            <> show t
            <> case mexpr of
                Nothing -> ""
                Just expr -> " = " <> show expr

instance Is PolarityType polarity => HasVarMapping (QSignature polarity) where
    getVarMapping (ValueSignature _ _ t _) = getVarMapping t

data QRecordValue
    = forall (tt :: [Type]). MkQRecordValue
        (ListType (QSignature 'Positive) tt)
        (QFExpression ((->) (ListProduct tt)))

instance Show QRecordValue where
    show (MkQRecordValue sigs expr) = "of " <> show sigs <> ": " <> show expr

data QRecordConstructor
    = forall (t :: Type) (tt :: [Type]). MkQRecordConstructor
        (ListVType (QSignature 'Positive) tt)
        (QGroundedShimWit 'Positive t)
        (QGroundedShimWit 'Negative t)
        (Codec t (ListVProduct tt))

recordConstructorToValue :: QRecordConstructor -> QRecordValue
recordConstructorToValue (MkQRecordConstructor sigs vtype _ codec) =
    MkQRecordValue (listVTypeToType sigs)
        $ constSealedFExpression
        $ MkSomeFor (shimWitToDolan vtype) (encode codec . listProductToVProduct sigs)

data QInterpreterBinding
    = ValueBinding QExpression
    | RecordValueBinding QRecordValue
    | PatternConstructorBinding
        QExpression
        QPatternConstructor
    | RecordConstructorBinding QRecordConstructor
    | TypeBinding QSomeGroundType

instance HasInterpreter => Show QInterpreterBinding where
    show (ValueBinding e) = "val: " <> show e
    show (RecordValueBinding _) = "recordval"
    show (PatternConstructorBinding e _) = "cons: " <> show e
    show (RecordConstructorBinding _) = "recordcons"
    show (TypeBinding t) = "type: " <> exprShowShow t

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
    in MkBindingSelector{..}

recordConstructorBindingSelector :: BindingSelector QRecordConstructor
recordConstructorBindingSelector = let
    bsEncode = RecordConstructorBinding
    bsDecode (RecordConstructorBinding t) = Just t
    bsDecode _ = Nothing
    bsError = LookupNotRecordConstructorError
    in MkBindingSelector{..}

recordValueBindingSelector :: BindingSelector QRecordValue
recordValueBindingSelector = let
    bsEncode = RecordValueBinding
    bsDecode (RecordValueBinding t) = Just t
    bsDecode (RecordConstructorBinding t) = Just $ recordConstructorToValue t
    bsDecode _ = Nothing
    bsError = LookupNotRecordError
    in MkBindingSelector{..}
