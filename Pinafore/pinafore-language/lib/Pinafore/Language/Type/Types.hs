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

literalGroundType :: QGroundType '[] Literal
literalGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Literal)|]) "Literal"

literalGreatestDynamicSupertype :: AsLiteral t => PinaforePolyGreatestDynamicSupertype '[] t
literalGreatestDynamicSupertype =
    SimplePolyGreatestDynamicSupertype
        literalGroundType
        (functionToShim "fromLiteral" fromLiteral)
        (functionToShim "toLiteral" toLiteral)

actionGroundType :: QGroundType '[ CoCCRVariance] Action
actionGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Action)|]) "Action"

funcGroundType :: QGroundType '[ ContraCCRVariance, CoCCRVariance] (->)
funcGroundType =
    singleGroundType $(iowitness [t|'MkWitKind (SingletonFamily (->))|]) $ \ta tb ->
        (precShow 5 ta <> " -> " <> precShow 6 tb, 6)

maybeGroundType :: QGroundType '[ CoCCRVariance] Maybe
maybeGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Maybe)|]) "Maybe"

listGroundType :: QGroundType '[ CoCCRVariance] []
listGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily [])|]) "List"

list1GroundType :: IsDolanSubtypeGroundType QGroundType => QGroundType '[ CoCCRVariance] NonEmpty
list1GroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily NonEmpty)|]) "List1")
        { pgtGreatestDynamicSupertype =
              GeneralPolyGreatestDynamicSupertype $ \(ConsCCRArguments ta NilCCRArguments) -> let
                  tt = MkDolanGroundedType listGroundType $ ConsCCRArguments ta NilCCRArguments
                  in Just $ MkShimWit tt (MkPolarMap $ functionToShim "nonEmpty" nonEmpty)
        }

eitherGroundType :: QGroundType '[ CoCCRVariance, CoCCRVariance] Either
eitherGroundType =
    singleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Either)|]) $ \ta tb ->
        (precShow 3 ta <> " +: " <> precShow 4 tb, 4)

pairGroundType :: QGroundType '[ CoCCRVariance, CoCCRVariance] (,)
pairGroundType =
    singleGroundType $(iowitness [t|'MkWitKind (SingletonFamily (,))|]) $ \ta tb ->
        (precShow 2 ta <> " *: " <> precShow 3 tb, 3)

showableGroundType :: QGroundType '[] Showable
showableGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Showable)|]) "Showable"
