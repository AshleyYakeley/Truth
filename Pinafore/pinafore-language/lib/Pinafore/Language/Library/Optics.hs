{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Optics
    ( opticsLibSection
    , propertyGroundType
    ) where

import Pinafore.Language.Convert
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Model ()
import Pinafore.Language.Type
import Pinafore.Language.Value
import Pinafore.Language.Var
import Shapes

-- LangAttribute
attributeGroundType :: QGroundType '[ 'RangeCCRVariance, 'RangeCCRVariance] LangAttribute
attributeGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangAttribute)|]) "Attribute"

instance HasQGroundType '[ 'RangeCCRVariance, 'RangeCCRVariance] LangAttribute where
    qGroundType = attributeGroundType

-- LangProperty
propertyGroundType :: QGroundType '[ 'RangeCCRVariance, 'RangeCCRVariance] LangProperty
propertyGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangProperty)|]) "Property"

instance HasQGroundType '[ 'RangeCCRVariance, 'RangeCCRVariance] LangProperty where
    qGroundType = propertyGroundType

-- LangLens
lensGroundType :: QGroundType '[ 'RangeCCRVariance, 'RangeCCRVariance] LangLens
lensGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangLens)|]) "Lens"

instance HasQGroundType '[ 'RangeCCRVariance, 'RangeCCRVariance] LangLens where
    qGroundType = lensGroundType

-- LangPrism
prismGroundType :: QGroundType '[ 'RangeCCRVariance, 'RangeCCRVariance] LangPrism
prismGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangPrism)|]) "Prism"

instance HasQGroundType '[ 'RangeCCRVariance, 'RangeCCRVariance] LangPrism where
    qGroundType = prismGroundType

opticsLibSection :: BindDocTree context
opticsLibSection =
    headingBDT
        "Optics & Properties"
        ""
        [ headingBDT
              "Lens"
              ""
              [ typeBDT "Lens" "" (MkSomeGroundType lensGroundType) []
              , hasSubtypeRelationBDT @(LangLens '( AP, AQ) '( BP, BQ)) @(LangAttribute '( AP, AQ) '( BP, BQ)) Verify "" $
                functionToShim "langLensAttribute" $ langLensAttribute @AP @AQ @BP @BQ
              , valPatBDT "MkLens" "" (MkLangLens @'( AP, AQ) @'( BP, BQ)) $
                PureFunction $ \(MkLangLens @'(AP, AQ) @'(BP, BQ) g pb) -> (g, (pb, ()))
              , namespaceBDT
                    "Lens"
                    ""
                    [ valBDT "get" "" $ langLensGet @'( AP, AQ) @'( BP, BQ)
                    , valBDT "putback" "" $ langLensPutback @'( AP, AQ) @'( BP, BQ)
                    , valBDT "identity" "Identity lens." $ identityLangLens @X @Y
                    , valBDT "compose" "Compose lenses." $ composeLangLens @AP @AQ @BX @BY @CP @CQ
                    , valBDT "product" "Product of lenses." $ pairLangLens @A @BP @BQ @CP @CQ
                    , valBDT "sum" "Sum of lenses." $ eitherLangLens @AP @AQ @BP @BQ @CP @CQ
                    ]
              ]
        , headingBDT
              "Prism"
              ""
              [ typeBDT "Prism" "" (MkSomeGroundType prismGroundType) []
              , hasSubtypeRelationBDT
                    @(LangPrism '( AP, AQ) '( BP, BQ))
                    @(LangAttribute '( AP, AQ) '( BP, BQ))
                    Verify
                    "" $
                functionToShim "langPrismAttribute" $ langPrismAttribute @AP @AQ @BP @BQ
              , valPatBDT "MkPrism" "" (MkLangPrism @'( AP, AQ) @'( BP, BQ)) $
                PureFunction $ \(MkLangPrism @'(AP, AQ) @'(BP, BQ) d e) -> (d, (e, ()))
              , namespaceBDT
                    "Prism"
                    ""
                    [ valBDT "decode" "" $ prismDecode @AP @AQ @BP @BQ
                    , valBDT "encode" "" $ prismEncode @AP @AQ @BP @BQ
                    , valBDT "identity" "Identity prism." $ identityLangPrism @X @Y
                    , valBDT "compose" "Compose prisms." $ composeLangPrism @AP @AQ @BX @BY @CP @CQ
                    ]
              ]
        , headingBDT
              "Attribute"
              "Attributes relate entities."
              [ typeBDT "Attribute" "" (MkSomeGroundType attributeGroundType) []
              , valBDT "!$" "Apply an attribute to a model." $ applyLangAttributeModel @AP @AQ @BP @BQ
              , valSupertypeBDT "!$" "Apply an attribute to a model." $ applyLangAttributeModel @A @A @B @B
              , valBDT "!$%" "Apply an attribute to an immutable model.\n`m !$% r = m !$ immutWholeModel r`" $
                applyLangAttributeImmutModel @A @BP @BQ
              , valSupertypeBDT "!$%" "Apply an attribute to an immutable model.\n`m !$% r = m !$ immutWholeModel r`" $
                applyLangAttributeImmutModel @A @B @B
              , valBDT "!$$" "Apply an attribute to a set." $ applyLangAttributeSet @A @B
              , valBDT "!$." "Compose attributes." $ composeLangAttribute @AP @AQ @BX @BY @CP @CQ
              , valSupertypeBDT "!$." "Compose attributes." $ composeLangAttribute @A @A @B @B @C @C
              , valBDT "!$**" "Pair attributes. Models from these attributes are undeleteable." $
                pairLangAttribute @AP @AQ @BP @BQ @CP @CQ
              , valSupertypeBDT "!$**" "Pair attributes. Models from these attributes are undeleteable." $
                pairLangAttribute @A @A @B @B @C @C
              , valBDT "!$++" "Either attributes. Models from these attributes are undeleteable." $
                eitherLangAttribute @AP @AQ @BP @BQ @CP @CQ
              , valSupertypeBDT "!$++" "Either attributes. Models from these attributes are undeleteable." $
                eitherLangAttribute @A @A @B @B @C @C
              ]
        , headingBDT
              "Property"
              "Properties relate entities."
              [ typeBDT "Property" "" (MkSomeGroundType propertyGroundType) []
              , hasSubtypeRelationBDT
                    @(LangProperty '( AP, AQ) '( BP, BQ))
                    @(LangAttribute '( AP, AQ) '( BP, BQ))
                    Verify
                    "" $
                functionToShim "langPropertyAttribute" langPropertyAttribute
              , valBDT "identity" "The identity property." $ identityLangProperty @X @Y
              , valBDT "!." "Compose properties." $ composeLangProperty @AP @AQ @BX @BY @CP @CQ
              , valSupertypeBDT "!." "Compose properties." $ composeLangProperty @A @A @B @B @C @C
              , valBDT "!**" "Pair properties. Models from these properties are undeleteable." $
                pairLangProperty @AP @AQ @BP @BQ @CP @CQ
              , valSupertypeBDT "!**" "Pair properties. Models from these properties are undeleteable." $
                pairLangProperty @A @A @B @B @C @C
              , valBDT "!++" "Either properties. Models from these properties are undeleteable." $
                eitherLangProperty @AP @AQ @BP @BQ @CP @CQ
              , valSupertypeBDT "!++" "Either properties. Models from these properties are undeleteable." $
                eitherLangProperty @A @A @B @B @C @C
              , valBDT "!@" "Co-apply a property to a model." $ inverseApplyLangPropertyModel @A @BX @BY
              , valSupertypeBDT "!@" "Co-apply a property to a model." $ inverseApplyLangPropertyModel @A @B @B
              , valBDT "!@%" "Co-apply a property to an immutable model.\n`m !@% r = m !@ immutWholeModel r`" $
                inverseApplyLangPropertyImmutModel @A @B
              , valBDT "!@@" "Co-apply a property to a set." $ inverseApplyLangPropertySet @A @BX @BY
              , valSupertypeBDT "!@@" "Co-apply a property to a set." $ inverseApplyLangPropertySet @A @B @B
              ]
        ]
