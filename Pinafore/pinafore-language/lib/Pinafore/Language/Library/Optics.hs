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

opticsLibSection :: LibraryStuff context
opticsLibSection =
    headingBDS
        "Optics & Properties"
        ""
        [ headingBDS
              "Lens"
              ""
              [ typeBDS
                    "Lens"
                    ""
                    (MkSomeGroundType lensGroundType)
                    [ valPatBDS "Mk" "" (MkLangLens @'( AP, AQ) @'( BP, BQ)) $
                      PureFunction $ \(MkLangLens @'(AP, AQ) @'(BP, BQ) g pb) -> (g, (pb, ()))
                    ]
              , hasSubtypeRelationBDS @(LangLens '( AP, AQ) '( BP, BQ)) @(LangAttribute '( AP, AQ) '( BP, BQ)) Verify "" $
                functionToShim "langLensAttribute" $ langLensAttribute @AP @AQ @BP @BQ
              , namespaceBDS
                    "Lens"
                    [ addNameInRootBDS $ valBDS "fetch" "" $ langLensGet @'( AP, AQ) @'( BP, BQ)
                    , addNameInRootBDS $ valBDS "putback" "" $ langLensPutback @'( AP, AQ) @'( BP, BQ)
                    , valBDS "id" "Identity lens." $ identityLangLens @X @Y
                    , valBDS "." "Compose lenses." $ composeLangLens @AP @AQ @BX @BY @CP @CQ
                    , valBDS "product" "Product of lenses." $ pairLangLens @A @BP @BQ @CP @CQ
                    , valBDS "sum" "Sum of lenses." $ eitherLangLens @AP @AQ @BP @BQ @CP @CQ
                    ]
              ]
        , headingBDS
              "Prism"
              ""
              [ typeBDS
                    "Prism"
                    ""
                    (MkSomeGroundType prismGroundType)
                    [ valPatBDS "Mk" "" (MkLangPrism @'( AP, AQ) @'( BP, BQ)) $
                      PureFunction $ \(MkLangPrism @'(AP, AQ) @'(BP, BQ) d e) -> (d, (e, ()))
                    ]
              , hasSubtypeRelationBDS
                    @(LangPrism '( AP, AQ) '( BP, BQ))
                    @(LangAttribute '( AP, AQ) '( BP, BQ))
                    Verify
                    "" $
                functionToShim "langPrismAttribute" $ langPrismAttribute @AP @AQ @BP @BQ
              , namespaceBDS
                    "Prism"
                    [ addNameInRootBDS $ valBDS "decode" "" $ prismDecode @AP @AQ @BP @BQ
                    , addNameInRootBDS $ valBDS "encode" "" $ prismEncode @AP @AQ @BP @BQ
                    , valBDS "id" "Identity prism." $ identityLangPrism @X @Y
                    , valBDS "." "Compose prisms." $ composeLangPrism @AP @AQ @BX @BY @CP @CQ
                    , valBDS "reverse" "" $ langPrismReverseAttribute @AP @AQ @BP @BQ
                    ]
              ]
        , headingBDS
              "Attribute"
              "Attributes relate entities."
              [ typeBDS "Attribute" "" (MkSomeGroundType attributeGroundType) []
              , namespaceBDS
                    "Attribute"
                    [ valBDS "id" "The identity attribute." $ identityLangAttribute @X @Y
                    , valBDS "." "Compose attributes." $ composeLangAttribute @AP @AQ @BX @BY @CP @CQ
                    , valBDS "**" "Type product of attributes. Models from these attributes are undeleteable." $
                      pairLangAttribute @AP @AQ @BP @BQ @CP @CQ
                    , valBDS "++" "Type sum of attributes. Models from these attributes are undeleteable." $
                      eitherLangAttribute @AP @AQ @BP @BQ @CP @CQ
                    , addNameInRootBDS $
                      valBDS "!$" "Apply an attribute to a model." $ applyLangAttributeModel @AP @AQ @BP @BQ
                    , addNameInRootBDS $
                      valBDS "!$%" "Apply an attribute to an immutable model.\n`m !$% r = m !$ immut.WholeModel r`" $
                      applyLangAttributeImmutModel @A @BP @BQ
                    , addNameInRootBDS $ valBDS "!$$" "Apply an attribute to a set." $ applyLangAttributeSet @A @B
                    ]
              ]
        , headingBDS
              "Property"
              "Properties relate entities."
              [ typeBDS "Property" "" (MkSomeGroundType propertyGroundType) []
              , hasSubtypeRelationBDS
                    @(LangProperty '( AP, AQ) '( BP, BQ))
                    @(LangAttribute '( AP, AQ) '( BP, BQ))
                    Verify
                    "" $
                functionToShim "langPropertyAttribute" langPropertyAttribute
              , namespaceBDS
                    "Property"
                    [ valBDS "id" "The identity property." $ identityLangProperty @X @Y
                    , valBDS "." "Compose properties." $ composeLangProperty @AP @AQ @BX @BY @CP @CQ
                    , valBDS "**" "Type product of properties. Models from these properties are undeleteable." $
                      pairLangProperty @AP @AQ @BP @BQ @CP @CQ
                    , valBDS "++" "Type sum of properties. Models from these properties are undeleteable." $
                      eitherLangProperty @AP @AQ @BP @BQ @CP @CQ
                    , addNameInRootBDS $
                      valBDS "!@" "Co-apply a property to a model." $ inverseApplyLangPropertyModel @A @BX @BY
                    , addNameInRootBDS $
                      valBDS "!@%" "Co-apply a property to an immutable model.\n`m !@% r = m !@ immut.WholeModel r`" $
                      inverseApplyLangPropertyImmutModel @A @B
                    , addNameInRootBDS $
                      valBDS "!@@" "Co-apply a property to a set." $ inverseApplyLangPropertySet @A @BX @BY
                    ]
              ]
        ]
