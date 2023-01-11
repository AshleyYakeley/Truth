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
                    , valBDT "id" "Identity lens." $ identityLangLens @X @Y
                    , valBDT "." "Compose lenses." $ composeLangLens @AP @AQ @BX @BY @CP @CQ
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
                    , valBDT "id" "Identity prism." $ identityLangPrism @X @Y
                    , valBDT "." "Compose prisms." $ composeLangPrism @AP @AQ @BX @BY @CP @CQ
                    , valBDT "reverse" "" $ langPrismReverseAttribute @AP @AQ @BP @BQ
                    ]
              ]
        , headingBDT
              "Attribute"
              "Attributes relate entities."
              [ typeBDT "Attribute" "" (MkSomeGroundType attributeGroundType) []
              , namespaceBDT
                    "Attribute"
                    ""
                    [ valBDT "id" "The identity attribute." $ identityLangAttribute @X @Y
                    , valBDT "." "Compose attributes." $ composeLangAttribute @AP @AQ @BX @BY @CP @CQ
                    , valBDT "**" "Type product of attributes. Models from these attributes are undeleteable." $
                      pairLangAttribute @AP @AQ @BP @BQ @CP @CQ
                    , valBDT "++" "Type sum of attributes. Models from these attributes are undeleteable." $
                      eitherLangAttribute @AP @AQ @BP @BQ @CP @CQ
                    , valBDT "!$" "Apply an attribute to a model." $ applyLangAttributeModel @AP @AQ @BP @BQ
                    , valBDT "!$%" "Apply an attribute to an immutable model.\n`m !$% r = m !$ immut.WholeModel r`" $
                      applyLangAttributeImmutModel @A @BP @BQ
                    , valBDT "!$$" "Apply an attribute to a set." $ applyLangAttributeSet @A @B
                    ]
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
              , namespaceBDT
                    "Property"
                    ""
                    [ valBDT "id" "The identity property." $ identityLangProperty @X @Y
                    , valBDT "." "Compose properties." $ composeLangProperty @AP @AQ @BX @BY @CP @CQ
                    , valBDT "**" "Type product of properties. Models from these properties are undeleteable." $
                      pairLangProperty @AP @AQ @BP @BQ @CP @CQ
                    , valBDT "++" "Type sum of properties. Models from these properties are undeleteable." $
                      eitherLangProperty @AP @AQ @BP @BQ @CP @CQ
                    , valBDT "!@" "Co-apply a property to a model." $ inverseApplyLangPropertyModel @A @BX @BY
                    , valBDT "!@%" "Co-apply a property to an immutable model.\n`m !@% r = m !@ immut.WholeModel r`" $
                      inverseApplyLangPropertyImmutModel @A @B
                    , valBDT "!@@" "Co-apply a property to a set." $ inverseApplyLangPropertySet @A @BX @BY
                    ]
              ]
        ]
