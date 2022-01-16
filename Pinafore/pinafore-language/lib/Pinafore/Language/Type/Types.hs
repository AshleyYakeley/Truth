module Pinafore.Language.Type.Types where

import Data.Shim
import Data.Time
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.ExprShow
import Pinafore.Language.Type.DynamicSupertype
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Value
import Shapes

literalGroundType :: PinaforeGroundType '[] Literal
literalGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Literal)|]) "Literal"

unitGroundType :: PinaforeGroundType '[] ()
unitGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily ())|]) "Unit"

textGroundType :: PinaforeGroundType '[] Text
textGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Text)|]) "Text"

numberGroundType :: PinaforeGroundType '[] Number
numberGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Number)|]) "Number"

rationalGroundType :: PinaforeGroundType '[] SafeRational
rationalGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily SafeRational)|]) "Rational")
        { pgtGreatestDynamicSupertype =
              \NilCCRArguments ->
                  Just $ makeNilGDS numberGroundType $ functionToShim "safeRationalNumber" $ decode safeRationalNumber
        }

integerGroundType :: PinaforeGroundType '[] Integer
integerGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Integer)|]) "Integer")
        { pgtGreatestDynamicSupertype =
              \NilCCRArguments ->
                  Just $
                  makeNilGDS numberGroundType $
                  functionToShim "integerSafeRational . safeRationalNumber" $
                  decode $ integerSafeRational . safeRationalNumber
        }

booleanGroundType :: PinaforeGroundType '[] Bool
booleanGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Bool)|]) "Boolean"

orderingGroundType :: PinaforeGroundType '[] Ordering
orderingGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Ordering)|]) "Ordering"

timeGroundType :: PinaforeGroundType '[] UTCTime
timeGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily UTCTime)|]) "Time"

durationGroundType :: PinaforeGroundType '[] NominalDiffTime
durationGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily NominalDiffTime)|]) "Duration"

dateGroundType :: PinaforeGroundType '[] Day
dateGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Day)|]) "Date"

timeOfDayGroundType :: PinaforeGroundType '[] TimeOfDay
timeOfDayGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily TimeOfDay)|]) "TimeOfDay"

localTimeGroundType :: PinaforeGroundType '[] LocalTime
localTimeGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LocalTime)|]) "LocalTime"

actionGroundType :: PinaforeGroundType '[ CoCCRVariance] PinaforeAction
actionGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily PinaforeAction)|]) "Action"

wholeRefGroundType :: PinaforeGroundType '[ 'RangeCCRVariance] LangWholeRef
wholeRefGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangWholeRef)|]) "WholeRef"

funcGroundType :: PinaforeGroundType '[ ContraCCRVariance, CoCCRVariance] (->)
funcGroundType =
    singleGroundType $(iowitness [t|'MkWitKind (SingletonFamily (->))|]) $ \ta tb ->
        (precShow 5 ta <> " -> " <> precShow 6 tb, 6)

maybeGroundType :: PinaforeGroundType '[ CoCCRVariance] Maybe
maybeGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Maybe)|]) "Maybe"

listGroundType :: PinaforeGroundType '[ CoCCRVariance] []
listGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily [])|]) "List"

list1GroundType :: IsDolanSubtypeGroundType PinaforeGroundType => PinaforeGroundType '[ CoCCRVariance] NonEmpty
list1GroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily NonEmpty)|]) "List1")
        { pgtGreatestDynamicSupertype =
              \(ConsCCRArguments ta NilCCRArguments) -> do
                  tt <-
                      invertTypeMaybe $
                      singleDolanType $ GroundedDolanSingularType listGroundType $ ConsCCRArguments ta NilCCRArguments
                  Just $ mapPolarShimWit (MkPolarMap $ functionToShim "nonEmpty" nonEmpty . iJoinL1) tt
        }

eitherGroundType :: PinaforeGroundType '[ CoCCRVariance, CoCCRVariance] Either
eitherGroundType =
    singleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Either)|]) $ \ta tb ->
        (precShow 3 ta <> " :+: " <> precShow 4 tb, 4)

pairGroundType :: PinaforeGroundType '[ CoCCRVariance, CoCCRVariance] (,)
pairGroundType =
    singleGroundType $(iowitness [t|'MkWitKind (SingletonFamily (,))|]) $ \ta tb ->
        (precShow 2 ta <> " :*: " <> precShow 3 tb, 3)

morphismGroundType :: PinaforeGroundType '[ 'RangeCCRVariance, 'RangeCCRVariance] LangMorphism
morphismGroundType =
    singleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangMorphism)|]) $ \ta tb ->
        (precShow 1 ta <> " ~> " <> precShow 2 tb, 2)
