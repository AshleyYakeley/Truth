{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Storage
    ( storageLibSection
    ) where

import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.Expression
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Optics
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type
import Pinafore.Language.Value
import Shapes

-- QStore
storeGroundType :: QGroundType '[] QStore
storeGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily QStore)|]) "Store"

instance HasQGroundType '[] QStore where
    qGroundType = storeGroundType

storageLibSection :: BindDocTree context
storageLibSection =
    headingBDT "Storage" "" $
    [ typeBDT "Store" "Storage of information." (MkSomeGroundType storeGroundType) []
    , namespaceBDT
          "Store"
          ""
          [ nameInRootBDT $
            specialFormBDT
                "property"
                "A property for this anchor. `A` and `B` are types that are subtypes of `Entity`."
                ["@A", "@B", "<anchor>"]
                "Store -> Property A B" $
            MkSpecialForm
                (ConsListType AnnotNonpolarType $ ConsListType AnnotNonpolarType $ ConsListType AnnotAnchor NilListType) $ \(MkSome ta, (MkSome tb, (anchor, ()))) -> do
                eta <- getMonoStorableType ta
                etb <- getMonoStorableType tb
                let
                    bta = biRangeSomeFor (nonpolarToNegative @QTypeSystem ta, nonpolarToPositive @QTypeSystem ta)
                    btb = biRangeSomeFor (nonpolarToNegative @QTypeSystem tb, nonpolarToPositive @QTypeSystem tb)
                    in case (bta, btb) of
                           (MkSomeFor (MkRangeType rtap rtaq) (MkRange praContra praCo), MkSomeFor (MkRangeType rtbp rtbq) (MkRange prbContra prbCo)) -> let
                               typem =
                                   typeToDolan $
                                   MkDolanGroundedType propertyGroundType $
                                   ConsCCRArguments (RangeCCRPolarArgument rtap rtaq) $
                                   ConsCCRArguments (RangeCCRPolarArgument rtbp rtbq) NilCCRArguments
                               typeStorage = typeToDolan $ MkDolanGroundedType storeGroundType NilCCRArguments
                               typef = qFunctionPosWitness typeStorage typem
                               property =
                                   predicateProperty (monoStoreAdapter eta) (monoStoreAdapter etb) (MkPredicate anchor)
                               pinaproperty =
                                   \qstore ->
                                       MkLangProperty $
                                       storageModelBased qstore $
                                       cfmap4 (MkCatDual $ shimToFunction praContra) $
                                       cfmap3 (shimToFunction praCo) $
                                       cfmap2 (MkCatDual $ shimToFunction prbContra) $
                                       cfmap1 (shimToFunction prbCo) property
                               anyval = MkSomeOf typef pinaproperty
                               in return anyval
          ]
    ]
