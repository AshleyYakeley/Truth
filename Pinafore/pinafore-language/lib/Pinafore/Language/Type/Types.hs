module Pinafore.Language.Type.Types where

import Data.Shim
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.ExprShow
import Pinafore.Language.Type.DynamicSupertype
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Value ()
import Shapes

literalGroundType :: PinaforeGroundType '[] Literal
literalGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Literal)|]) "Literal"

literalGreatestDynamicSupertype :: AsLiteral t => PinaforePolyGreatestDynamicSupertype '[] t
literalGreatestDynamicSupertype =
    SimplePolyGreatestDynamicSupertype
        literalGroundType
        (functionToShim "fromLiteral" fromLiteral)
        (functionToShim "toLiteral" toLiteral)

actionGroundType :: PinaforeGroundType '[ CoCCRVariance] PinaforeAction
actionGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily PinaforeAction)|]) "Action"

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
              GeneralPolyGreatestDynamicSupertype $ \(ConsCCRArguments ta NilCCRArguments) -> let
                  tt = mkShimWit $ GroundedDolanSingularType listGroundType $ ConsCCRArguments ta NilCCRArguments
                  in Just $ mapPolarShimWit (MkPolarMap $ functionToShim "nonEmpty" nonEmpty) tt
        }

eitherGroundType :: PinaforeGroundType '[ CoCCRVariance, CoCCRVariance] Either
eitherGroundType =
    singleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Either)|]) $ \ta tb ->
        (precShow 3 ta <> " +: " <> precShow 4 tb, 4)

pairGroundType :: PinaforeGroundType '[ CoCCRVariance, CoCCRVariance] (,)
pairGroundType =
    singleGroundType $(iowitness [t|'MkWitKind (SingletonFamily (,))|]) $ \ta tb ->
        (precShow 2 ta <> " *: " <> precShow 3 tb, 3)
