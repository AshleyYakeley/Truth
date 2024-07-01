module Pinafore.Language.Interpreter.Lookup
    ( getBindingLookup
    , checkNameForRegister
    , lookupBindingInfo
    , lookupDebugBindingInfo
    , lookupSelector
    , lookupBoundType
    , lookupPatternConstructor
    , lookupRecordConstructor
    , lookupSpecialForm
    , QBoundValue(..)
    , lookupBoundConstructor
    , lookupRecord
    , lookupMaybeValue
    ) where

import Import
import Pinafore.Language.Error
import Pinafore.Language.Interpreter.Binding
import Pinafore.Language.Interpreter.Interpreter
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Subtype ()

getBindingLookup :: QInterpreter (FullNameRef -> Maybe QInterpreterBinding)
getBindingLookup = do
    bindmap <- getBindingInfoLookup
    return $ \rname -> fmap (biValue . snd) $ bindmap rname

checkNameForRegister :: FullName -> QInterpreter ()
checkNameForRegister name = do
    mnt <- getBindingLookup
    case mnt $ fullNameRef name of
        Just _ -> throw $ DeclareBindingDuplicateError name
        Nothing -> return ()

rnuToInterpreter :: FullNameRef -> Maybe a -> QInterpreter a
rnuToInterpreter _ (Just a) = return a
rnuToInterpreter nameref Nothing = throw $ LookupNotDefinedError nameref

lookupBinding :: FullNameRef -> QInterpreter QInterpreterBinding
lookupBinding nameref = do
    bindmap <- getBindingLookup
    rnuToInterpreter nameref $ bindmap nameref

lookupBindingInfo :: FullNameRef -> QInterpreter (FullName, QBindingInfo)
lookupBindingInfo nameref = do
    bindmap <- getBindingInfoLookup
    rnuToInterpreter nameref $ bindmap nameref

lookupDebugBindingInfo :: FullNameRef -> QInterpreter (Maybe (FullName, String))
lookupDebugBindingInfo nameref = do
    bindmap <- getBindingInfoLookup
    return $ fmap (\(name, b) -> (name, show $ biValue b)) $ bindmap nameref

lookupSelector :: BindingSelector t -> FullNameRef -> QInterpreter t
lookupSelector bst name = do
    b <- lookupBinding name
    case bsDecode bst b of
        Just t -> return t
        Nothing -> throw $ bsError bst name

lookupBoundType :: FullNameRef -> QInterpreter QSomeGroundType
lookupBoundType = lookupSelector typeBindingSelector

lookupPatternConstructor :: FullNameRef -> QInterpreter (Either QPatternConstructor QRecordConstructor)
lookupPatternConstructor name = do
    b <- lookupBinding name
    case b of
        PatternConstructorBinding _ pc -> return $ Left pc
        RecordConstructorBinding rc -> return $ Right rc
        _ -> throw $ LookupNotConstructorError name

lookupRecordConstructor :: FullNameRef -> QInterpreter QRecordConstructor
lookupRecordConstructor = lookupSelector recordConstructorBindingSelector

lookupSpecialForm :: FullNameRef -> QInterpreter QSpecialForm
lookupSpecialForm = lookupSelector specialFormBindingSelector

data QBoundValue
    = ValueBoundValue QExpression
    | RecordBoundValue QRecordValue

lookupBoundConstructor :: FullNameRef -> QInterpreter QBoundValue
lookupBoundConstructor name = do
    b <- lookupBinding name
    case b of
        ValueBinding exp -> return $ ValueBoundValue exp
        PatternConstructorBinding exp _ -> return $ ValueBoundValue exp
        RecordValueBinding rv -> return $ RecordBoundValue rv
        RecordConstructorBinding rc -> return $ RecordBoundValue $ recordConstructorToValue rc
        _ -> throw $ LookupNotConstructorError name

lookupRecord :: FullNameRef -> QInterpreter QRecordValue
lookupRecord = lookupSelector recordValueBindingSelector

lookupMaybeValue :: FullNameRef -> QInterpreter (Maybe QExpression)
lookupMaybeValue name = do
    mb <- getBindingLookup
    return $
        case mb name of
            Just (ValueBinding exp) -> Just exp
            Just (PatternConstructorBinding exp _) -> Just exp
            _ -> Nothing
