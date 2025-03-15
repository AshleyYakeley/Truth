module Pinafore.Language.Interpreter.Binding
    ( QSignature (..)
    , QRecordValue (..)
    , QRecordConstructor (..)
    , recordConstructorToValue
    , QItem (..)
    , QScopeItem (..)
    , ItemSelector (..)
    , typeItemSelector
    , recordConstructorItemSelector
    , recordValueItemSelector
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

data QItem
    = ValueItem QExpression
    | RecordValueItem QRecordValue
    | PatternConstructorItem
        QExpression
        QPatternConstructor
    | RecordConstructorItem QRecordConstructor
    | TypeItem QSomeGroundType

instance HasInterpreter => Show QItem where
    show (ValueItem e) = "val: " <> show e
    show (RecordValueItem _) = "recordval"
    show (PatternConstructorItem e _) = "cons: " <> show e
    show (RecordConstructorItem _) = "recordcons"
    show (TypeItem t) = "type: " <> exprShowShow t

data QScopeItem = MkQScopeItem
    { siOriginalName :: FullName
    , siDocumentation :: DefDoc
    , siItem :: QItem
    }

data ItemSelector t = MkItemSelector
    { isEncode :: t -> QItem
    , isDecode :: QItem -> Maybe t
    , isError :: FullNameRef -> QError
    }

typeItemSelector :: ItemSelector QSomeGroundType
typeItemSelector = let
    isEncode = TypeItem
    isDecode (TypeItem t) = Just t
    isDecode _ = Nothing
    isError = LookupNotTypeError
    in MkItemSelector{..}

recordConstructorItemSelector :: ItemSelector QRecordConstructor
recordConstructorItemSelector = let
    isEncode = RecordConstructorItem
    isDecode (RecordConstructorItem t) = Just t
    isDecode _ = Nothing
    isError = LookupNotRecordConstructorError
    in MkItemSelector{..}

recordValueItemSelector :: ItemSelector QRecordValue
recordValueItemSelector = let
    isEncode = RecordValueItem
    isDecode (RecordValueItem t) = Just t
    isDecode (RecordConstructorItem t) = Just $ recordConstructorToValue t
    isDecode _ = Nothing
    isError = LookupNotRecordError
    in MkItemSelector{..}
