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

instance IsDolanSubtypeGroundType PinaforeGroundType where
    tackOnTypeConvertError (ta :: _ pola _) (tb :: _ polb _) ma = do
        spos <- paramAsk sourcePosParam
        rethrowCause
            spos
            (TypeConvertError
                 (exprShow ta)
                 (Just $ witnessToValue $ polarityType @pola)
                 (exprShow tb)
                 (Just $ witnessToValue $ polarityType @polb))
            ma
    throwTypeNotInvertible t = throw $ TypeNotInvertibleError $ exprShow t

instance IsDolanSubtypeEntriesGroundType PinaforeGroundType where
    subtypeConversionEntries = getSubtypeConversions
    subtypeConversionMatchType gta gtb = do
        (Refl, HRefl) <- groundTypeTestEquality gta gtb
        return idSubtypeConversion
    throwGroundTypeConvertError ta tb = throw $ TypeConvertError (showGroundType ta) Nothing (showGroundType tb) Nothing

instance IsDolanFunctionGroundType PinaforeGroundType where
    functionGroundType = funcGroundType
