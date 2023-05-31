{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE ApplicativeDo #-}

module Pinafore.Language.Type.Subtype
    ( funcGroundType
    , module Pinafore.Language.Type.Subtype.Hint
    ) where

import Data.Shim
import Language.Expression.Dolan
import Pinafore.Language.Error
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Show
import Pinafore.Language.Type.Subtype.Hint
import Shapes

funcGroundType :: QGroundType '[ ContraCCRVariance, CoCCRVariance] (->)
funcGroundType =
    singleGroundType $(iowitness [t|'MkWitKind (SingletonFamily (->))|]) $ \ta tb ->
        namedTextPrec 6 $ precNamedText 5 ta <> " -> " <> precNamedText 6 tb

instance IsDolanSubtypeGroundType QGroundType where
    type DolanSubtypeHint QGroundType = QSubtypeHint
    subtypeGroundedTypes = entries_subtypeGroundedTypes
    tackOnTypeConvertError (ta :: _ pola _) (tb :: _ polb _) ma = do
        spos <- paramAsk sourcePosParam
        ntt <- getRenderFullName
        rethrowCause
            spos
            (TypeConvertError
                 (ntt $ exprShow ta)
                 (witnessToValue $ polarityType @pola)
                 (ntt $ exprShow tb)
                 (witnessToValue $ polarityType @polb))
            ma
    throwTypeNotInvertible t = throwWithName $ \ntt -> TypeNotInvertibleError $ ntt $ exprShow t

instance IsDolanSubtypeEntriesGroundType QGroundType where
    subtypeConversionEntries = getSubtypeConversions
    getSubtypeGroup t =
        case qgtSubtypeGroup t of
            Just sg -> sg
            Nothing -> singletonSubtypeGroup t
    throwNoGroundTypeConversionError ta tb =
        throwWithName $ \ntt -> NoGroundTypeConversionError (ntt $ showGroundType ta) (ntt $ showGroundType tb)
    throwIncoherentGroundTypeConversionError ta tb =
        throwWithName $ \ntt -> IncoherentGroundTypeConversionError (ntt $ showGroundType ta) (ntt $ showGroundType tb)

instance IsDolanFunctionGroundType QGroundType where
    functionGroundType = funcGroundType
