{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE ApplicativeDo #-}

module Pinafore.Language.Type.Subtype
    ( funcGroundType
    ) where

import Data.Shim
import Language.Expression.Dolan
import Pinafore.Language.DefDoc
import Pinafore.Language.Error
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type.DynamicSupertype
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Identified
import Pinafore.Language.Type.Show
import Shapes

funcGroundType :: QGroundType '[ ContraCCRVariance, CoCCRVariance] (->)
funcGroundType =
    singleGroundType $(iowitness [t|'MkWitKind (SingletonFamily (->))|]) $ \ta tb ->
        namedTextPrec 6 $ precNamedText 5 ta <> " -> " <> precNamedText 6 tb

instance IsDolanSubtypeGroundType QGroundType where
    type DolanPatternWitness QGroundType = PatternWitness QGroundType
    dolanMakePatternWitness v w = ValuePatternWitness v w
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

instance IsInterpreterGroundType QGroundType where
    type EntryDoc QGroundType = DefDoc
    createGroundType fn =
        withNewTypeID $ \(tid :: _ t) -> do
            Refl <- unsafeIdentifyKind @t @Type tid
            let
                gt :: QGroundType '[] (Identified t)
                gt =
                    MkQGroundType
                        { qgtVarianceType = NilListType
                        , qgtVarianceMap = NilDolanVarianceMap
                        , qgtShowType = namedTextToPrec $ toNamedText fn
                        , qgtFamilyType = MkFamilialType identifiedFamilyWitness $ MkIdentifiedTypeFamily tid
                        , qgtProperties = mempty
                        , qgtSubtypeGroup = Nothing
                        , qgtGreatestDynamicSupertype = nullPolyGreatestDynamicSupertype
                        }
            return $ MkSomeGroundType gt
