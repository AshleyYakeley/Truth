{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Convert.Literal where

import Import
import Pinafore.Language.Convert.HasType
import Pinafore.Language.Type

literalGroundType :: QGroundType '[] Literal
literalGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily Literal)|]) "Literal"

instance HasQGroundType '[] Literal where
    qGroundType = literalGroundType

literalStorabilityProp ::
       forall (t :: Type). AsLiteral t
    => GroundProperties '[] t
literalStorabilityProp =
    singleGroundProperty storabilityProperty $
    MkStorability
        { stbKind = NilListType
        , stbCovaryMap = covarymap
        , stbAdapter = pureStorabilityAdapter $ \NilArguments -> asLiteralStoreAdapter
        }

literalGreatestDynamicSupertype :: AsLiteral t => PinaforePolyGreatestDynamicSupertype '[] t
literalGreatestDynamicSupertype =
    simplePolyGreatestDynamicSupertype literalGroundType (functionToShim "fromLiteral" fromLiteral)

-- Literal types
mkLiteralGroundType ::
       forall (t :: Type). AsLiteral t
    => IOWitness ('MkWitKind (SingletonFamily t))
    -> FullName
    -> QGroundType '[] t
mkLiteralGroundType wit name =
    (stdSingleGroundType wit name)
        {qgtProperties = literalStorabilityProp, qgtGreatestDynamicSupertype = literalGreatestDynamicSupertype}
