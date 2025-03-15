module Pinafore.Language.Interpreter.Lookup
    ( getBindingLookup
    , checkNameForRegister
    , lookupBindingInfo
    , lookupDebugBindingInfo
    , lookupSelector
    , lookupBoundType
    , lookupPatternConstructor
    , lookupRecordConstructor
    , QBoundValue (..)
    , getBoundValue
    , lookupValue
    , lookupRecord
    , lookupMaybeValue
    )
where

import Import
import Pinafore.Language.Error
import Pinafore.Language.Interpreter.Binding
import Pinafore.Language.Interpreter.Interpreter
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Subtype ()

getBindingLookup :: QInterpreter (FullNameRef -> Maybe QItem)
getBindingLookup = do
    bindmap <- getBindingInfoLookup
    return $ \rname -> fmap (siItem . snd) $ bindmap rname

checkNameForRegister :: FullName -> QInterpreter ()
checkNameForRegister name = do
    mnt <- getBindingLookup
    case mnt $ fullNameRef name of
        Just _ -> throw $ DeclareBindingDuplicateError name
        Nothing -> return ()

rnuToInterpreter :: FullNameRef -> Maybe a -> QInterpreter a
rnuToInterpreter _ (Just a) = return a
rnuToInterpreter nameref Nothing = throw $ LookupNotDefinedError nameref

lookupBinding :: FullNameRef -> QInterpreter QItem
lookupBinding nameref = do
    bindmap <- getBindingLookup
    rnuToInterpreter nameref $ bindmap nameref

lookupBindingInfo :: FullNameRef -> QInterpreter (FullName, QScopeItem)
lookupBindingInfo nameref = do
    bindmap <- getBindingInfoLookup
    rnuToInterpreter nameref $ bindmap nameref

lookupDebugBindingInfo :: FullNameRef -> QInterpreter (Maybe (FullName, String))
lookupDebugBindingInfo nameref = do
    bindmap <- getBindingInfoLookup
    return $ fmap (\(name, b) -> (name, show $ siItem b)) $ bindmap nameref

lookupSelector :: ItemSelector t -> FullNameRef -> QInterpreter t
lookupSelector bst name = do
    b <- lookupBinding name
    case isDecode bst b of
        Just t -> return t
        Nothing -> throw $ isError bst name

lookupBoundType :: FullNameRef -> QInterpreter QSomeGroundType
lookupBoundType = lookupSelector typeItemSelector

lookupPatternConstructor :: FullNameRef -> QInterpreter (Either QPatternConstructor QRecordConstructor)
lookupPatternConstructor name = do
    b <- lookupBinding name
    case b of
        PatternConstructorItem _ pc -> return $ Left pc
        RecordConstructorItem rc -> return $ Right rc
        _ -> throw $ LookupNotConstructorError name

lookupRecordConstructor :: FullNameRef -> QInterpreter QRecordConstructor
lookupRecordConstructor = lookupSelector recordConstructorItemSelector

data QBoundValue
    = ValueBoundValue QExpression
    | RecordBoundValue QRecordValue

lookupRecord :: FullNameRef -> QInterpreter QRecordValue
lookupRecord = lookupSelector recordValueItemSelector

getBoundValue :: QItem -> Maybe QBoundValue
getBoundValue =
    \case
        ValueItem expr -> Just $ ValueBoundValue expr
        PatternConstructorItem expr _ -> Just $ ValueBoundValue expr
        RecordValueItem rv -> Just $ RecordBoundValue rv
        RecordConstructorItem rc -> Just $ RecordBoundValue $ recordConstructorToValue rc
        _ -> Nothing

lookupValue :: FullNameRef -> QInterpreter QBoundValue
lookupValue name = do
    b <- lookupBinding name
    case getBoundValue b of
        Just bv -> return bv
        _ -> throw $ LookupNotValueError name

lookupMaybeValue :: FullNameRef -> QInterpreter (Maybe QBoundValue)
lookupMaybeValue name = do
    mb <- getBindingLookup
    return $ mb name >>= getBoundValue
