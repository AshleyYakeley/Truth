{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE ApplicativeDo #-}

module Pinafore.Language.Type.Subtype
    ( funcGroundType
    , module Pinafore.Language.Type.Subtype.Hint
    )
where

import Import
import Pinafore.Language.Error
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Show
import Pinafore.Language.Type.Subtype.Hint
import Pinafore.Language.VarID

funcGroundType :: QGroundType '[ContraCCRVariance, CoCCRVariance] (->)
funcGroundType =
    singleGroundType $(iowitness [t|'MkWitKind (SingletonFamily (->))|]) $ \ta tb ->
        namedTextPrec 6 $ precNamedText 5 ta <> " -> " <> precNamedText 6 tb

type instance DolanSubtypeHint QGroundType = QSubtypeHint

instance HasInterpreter => IsDolanSubtypeGroundType QGroundType where
    getSubtypeChain = entries_getSubtypeChain
    throwTypeError (InternalTypeError msg) = throw $ InternalError Nothing $ toNamedText msg
    throwTypeError (InternalSafetyError msg err t) =
        throw
            $ InternalError Nothing
            $ toNamedText msg
            <> " simplification: "
            <> showNamedText err
            <> " recursive type: "
            <> showNamedText t
    throwTypeError (UninvertibleTypeError t) = throw $ TypeNotInvertibleError $ exprShow t
    throwTypeError (NoGroundConvertTypeError ga gb) =
        throw $ NoGroundTypeConversionError (showGroundType ga) (showGroundType gb)
    throwTypeError (IncoherentGroundConvertTypeError ga gb) =
        throw $ IncoherentGroundTypeConversionError (showGroundType ga) (showGroundType gb)
    throwTypeError (ConvertTypeError fta ftb) =
        flipToType fta $ \(ta :: _ pola _) ->
            flipToType ftb $ \(tb :: _ polb _) ->
                throw
                    $ TypeConvertError
                        (exprShow ta)
                        (witnessToValue $ polarityType @pola)
                        (exprShow tb)
                        (witnessToValue $ polarityType @polb)
    shouldMerge = shouldMergeVarID

instance HasInterpreter => IsDolanSubtypeEntriesGroundType QGroundType where
    subtypeConversionEntries = getSubtypeConversions
    getSubtypeGroup t =
        case qgtSubtypeGroup t of
            Just sg -> sg
            Nothing -> singletonSubtypeGroup t

instance HasInterpreter => IsDolanFunctionGroundType QGroundType where
    functionGroundType = funcGroundType
