module Pinafore.Language.Interpreter.Lookup
    ( getBindingLookup
    , checkNameForRegister
    , lookupDebugBindingInfo
    , lookupBoundType
    , lookupPatternConstructor
    , lookupRecordConstructor
    , lookupSpecialForm
    , QBoundValue(..)
    , lookupBoundValue
    , lookupMaybeValue
    ) where

import Pinafore.Language.Error
import Pinafore.Language.Interpreter.Binding
import Pinafore.Language.Interpreter.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Subtype ()
import Shapes

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
rnuToInterpreter name Nothing = throw $ LookupNotDefinedError name

lookupBinding :: FullNameRef -> QInterpreter QInterpreterBinding
lookupBinding name = do
    bindmap <- getBindingLookup
    rnuToInterpreter name $ bindmap name

lookupDebugBindingInfo :: FullNameRef -> QInterpreter (Maybe (FullName, String))
lookupDebugBindingInfo nameref = do
    bindmap <- getBindingInfoLookup
    return $ fmap (\(name, b) -> (name, show $ biValue b)) $ bindmap nameref

lookupBoundType :: FullNameRef -> QInterpreter QSomeGroundType
lookupBoundType name = do
    b <- lookupBinding name
    case b of
        TypeBinding t -> return t
        _ -> throw $ LookupNotTypeError name

lookupPatternConstructor :: FullNameRef -> QInterpreter (Either QPatternConstructor QRecordConstructor)
lookupPatternConstructor name = do
    b <- lookupBinding name
    case b of
        ValueBinding _ (Just pc) -> return $ Left pc
        RecordConstructorBinding rc -> return $ Right rc
        _ -> throw $ LookupNotConstructorError name

lookupRecordConstructor :: FullNameRef -> QInterpreter QRecordConstructor
lookupRecordConstructor name = do
    b <- lookupBinding name
    case b of
        RecordConstructorBinding rc -> return rc
        _ -> throw $ LookupNotRecordConstructorError name

lookupSpecialForm :: FullNameRef -> QInterpreter QSpecialForm
lookupSpecialForm name = do
    b <- lookupBinding name
    case b of
        SpecialFormBinding sf -> return sf
        _ -> throw $ LookupNotSpecialFormError name

data QBoundValue
    = ValueBoundValue QExpression
    | RecordBoundValue QRecordConstructor

lookupBoundValue :: FullNameRef -> QInterpreter QBoundValue
lookupBoundValue name = do
    b <- lookupBinding name
    case b of
        ValueBinding exp _ -> return $ ValueBoundValue exp
        RecordConstructorBinding rc -> return $ RecordBoundValue rc
        _ -> throw $ LookupNotConstructorError name

lookupMaybeValue :: FullNameRef -> QInterpreter (Maybe QExpression)
lookupMaybeValue name = do
    mb <- getBindingLookup
    return $
        case mb name of
            Just (ValueBinding exp _) -> Just exp
            _ -> Nothing
