{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE ApplicativeDo #-}

module Pinafore.Language.Type.Subtype
    ( funcGroundType
    , module Pinafore.Language.Type.Subtype.Hint
    ) where

import Data.Shim
import Language.Expression.Dolan
import Pinafore.Language.Error
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

instance HasInterpreter => IsDolanSubtypeGroundType QGroundType where
    type DolanSubtypeHint QGroundType = QSubtypeHint
    subtypeGroundedTypes = entries_subtypeGroundedTypes
    tackOnTypeConvertError (ta :: _ pola _) (tb :: _ polb _) ma = do
        msg <- mkErrorMessage
        catch ma $ \pe ->
            throw $
            msg
                (TypeConvertError
                     (exprShow ta)
                     (witnessToValue $ polarityType @pola)
                     (exprShow tb)
                     (witnessToValue $ polarityType @polb))
                pe
    throwTypeConvertError (ta :: _ pola _) (tb :: _ polb _) =
        throw $
        TypeConvertError
            (exprShow ta)
            (witnessToValue $ polarityType @pola)
            (exprShow tb)
            (witnessToValue $ polarityType @polb)
    throwTypeNotInvertible t = throw $ TypeNotInvertibleError $ exprShow t

instance HasInterpreter => IsDolanSubtypeEntriesGroundType QGroundType where
    subtypeConversionEntries = getSubtypeConversions
    getSubtypeGroup t =
        case qgtSubtypeGroup t of
            Just sg -> sg
            Nothing -> singletonSubtypeGroup t
    throwNoGroundTypeConversionError ta tb = throw $ NoGroundTypeConversionError (showGroundType ta) (showGroundType tb)
    throwIncoherentGroundTypeConversionError ta tb =
        throw $ IncoherentGroundTypeConversionError (showGroundType ta) (showGroundType tb)

instance HasInterpreter => IsDolanFunctionGroundType QGroundType where
    functionGroundType = funcGroundType
