{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE ApplicativeDo #-}

module Pinafore.Language.Type.Subtype
    (
    ) where

import Data.Shim
import Language.Expression.Dolan
import Pinafore.Language.Error
import Pinafore.Language.ExprShow
import Pinafore.Language.Interpreter
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Show
import Pinafore.Language.Type.Types
import Shapes

instance IsDolanSubtypeGroundType QGroundType where
    subtypeGroundedTypes = entries_subtypeGroundedTypes
    tackOnTypeConvertError (ta :: _ pola _) (tb :: _ polb _) ma = do
        spos <- paramAsk sourcePosParam
        rethrowCause
            spos
            (TypeConvertError
                 (exprShow ta)
                 (witnessToValue $ polarityType @pola)
                 (exprShow tb)
                 (witnessToValue $ polarityType @polb))
            ma
    throwTypeNotInvertible t = throw $ TypeNotInvertibleError $ exprShow t

instance IsDolanSubtypeEntriesGroundType QGroundType where
    subtypeConversionEntries = getSubtypeConversions
    getSubtypeGroup t =
        case pgtSubtypeGroup t of
            Just sg -> sg
            Nothing -> singletonSubtypeGroup t
    throwNoGroundTypeConversionError ta tb = throw $ NoGroundTypeConversionError (showGroundType ta) (showGroundType tb)
    throwIncoherentGroundTypeConversionError ta tb =
        throw $ IncoherentGroundTypeConversionError (showGroundType ta) (showGroundType tb)

instance IsDolanFunctionGroundType QGroundType where
    functionGroundType = funcGroundType
