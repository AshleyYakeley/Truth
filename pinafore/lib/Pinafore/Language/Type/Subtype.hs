{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE ApplicativeDo #-}

module Pinafore.Language.Type.Subtype
    (
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Language.Error
import Pinafore.Language.Interpreter
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Show
import Pinafore.Language.Type.Type
import Shapes

showGroundType :: PinaforeGroundType dv gt -> Text
showGroundType t =
    newUVar "_" $ \var ->
        fst $
        saturatedGroundTypeShowPrec
            @PinaforeGroundType
            (MkAnyW $ singleDolanType @PinaforeGroundType $ VarDolanSingularType var)
            t

instance IsDolanSubtypeGroundType PinaforeGroundType where
    throwTypeConvertInverseError tp tq = throw $ TypeConvertInverseError (exprShow tp) (exprShow tq)
    throwTypeSubsumeError ::
           forall polarity tinf tdecl a. Is PolarityType polarity
        => PinaforeSingularType polarity tinf
        -> PinaforeType polarity tdecl
        -> PinaforeSourceInterpreter a
    throwTypeSubsumeError tinf tdecl = let
        pol =
            case polarityType @polarity of
                PositiveType -> Positive
                NegativeType -> Negative
        in throw $ TypeSubsumeError pol (exprShow tinf) (exprShow tdecl)
    throwTypeNotInvertible t = throw $ TypeNotInvertibleError $ exprShow t

instance IsDolanSubtypeEntriesGroundType PinaforeGroundType where
    subtypeConversionEntries = liftSourcePos getSubtypeConversions
    subtypeConversionMatchType gta gtb = do
        (Refl, HRefl) <- groundTypeTestEquality gta gtb
        return idSubtypeConversion
    throwTypeConvertError tp tq = convertFailure (showGroundType tp) (showGroundType tq)

instance IsDolanFunctionGroundType PinaforeGroundType where
    functionGroundType = FuncPinaforeGroundType
