{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Storage
    ( storageStuff
    ) where

import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.DocTree
import Pinafore.Language.Expression
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std.Convert ()
import Pinafore.Language.Library.Std.Model
import Pinafore.Language.Name
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type
import Pinafore.Language.Value
import Shapes

-- QStore
storeGroundType :: QGroundType '[] QStore
storeGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily QStore)|]) "Store"

instance HasQGroundType '[] QStore where
    qGroundType = storeGroundType

storageStuff :: DocTreeEntry (BindDocTree context)
storageStuff =
    docTreeEntry "Storage" "" $
    namespaceRelative
        "Storage"
        [ mkTypeEntry "Store" "Storage of information." (MkSomeGroundType storeGroundType) []
        , mkSpecialFormEntry
              "property"
              "A property for this anchor. `A` and `B` are types that are subtypes of `Entity`."
              ["@A", "@B", "<anchor>"]
              "Store -> A ~> B" $
          MkSpecialForm
              (ConsListType AnnotNonpolarType $ ConsListType AnnotNonpolarType $ ConsListType AnnotAnchor NilListType) $ \(MkSome ta, (MkSome tb, (anchor, ()))) -> do
              eta <- getMonoEntityType ta
              etb <- getMonoEntityType tb
              let
                  bta = biRangeSomeFor (nonpolarToNegative @QTypeSystem ta, nonpolarToPositive @QTypeSystem ta)
                  btb = biRangeSomeFor (nonpolarToNegative @QTypeSystem tb, nonpolarToPositive @QTypeSystem tb)
                  in case (bta, btb) of
                         (MkSomeFor (MkRangeType rtap rtaq) (MkRange praContra praCo), MkSomeFor (MkRangeType rtbp rtbq) (MkRange prbContra prbCo)) -> let
                             typem =
                                 typeToDolan $
                                 MkDolanGroundedType morphismGroundType $
                                 ConsCCRArguments (RangeCCRPolarArgument rtap rtaq) $
                                 ConsCCRArguments (RangeCCRPolarArgument rtbp rtbq) NilCCRArguments
                             typeStorage = typeToDolan $ MkDolanGroundedType storeGroundType NilCCRArguments
                             typef = qFunctionPosWitness typeStorage typem
                             morphism =
                                 propertyMorphism (monoEntityAdapter eta) (monoEntityAdapter etb) (MkPredicate anchor)
                             pinamorphism =
                                 \qstore ->
                                     MkLangMorphism $
                                     storageModelBased qstore $
                                     cfmap4 (MkCatDual $ shimToFunction praContra) $
                                     cfmap3 (shimToFunction praCo) $
                                     cfmap2 (MkCatDual $ shimToFunction prbContra) $
                                     cfmap1 (shimToFunction prbCo) morphism
                             anyval = MkSomeOf typef pinamorphism
                             in return anyval
        ]
