{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE ApplicativeDo #-}

module Pinafore.Language.Type.Subtype
    ( funcGroundType
    , module Pinafore.Language.Type.Subtype.Hint
    )
where

import Import
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Subtype.Hint
import Pinafore.Language.VarID

funcGroundType :: QGroundType '[ContraCCRVariance, CoCCRVariance] (->)
funcGroundType =
    singleGroundType $(iowitness [t|'MkWitKind (SingletonFamily (->))|]) $ \ta tb ->
        applyTypeOperatorPrecNamedText ta "->" tb

type instance DolanSubtypeHint QGroundType = QSubtypeHint

instance IsDolanSubtypeGroundType QGroundType where
    shouldMerge = shouldMergeVarID

instance IsDolanSubtypeEntriesGroundType QGroundType where
    getSubtypeGroup t =
        case qgtSubtypeGroup t of
            Just sg -> sg
            Nothing -> singletonSubtypeGroup t

instance IsDolanFunctionGroundType QGroundType where
    functionGroundType = funcGroundType
