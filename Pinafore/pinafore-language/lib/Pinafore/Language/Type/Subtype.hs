{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE ApplicativeDo #-}

module Pinafore.Language.Type.Subtype
    (
    ) where

import Data.Shim
import Language.Expression.Dolan
import Pinafore.Language.Error
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Show
import Pinafore.Language.Type.Types
import Shapes

instance IsDolanSubtypeGroundType QGroundType where
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
