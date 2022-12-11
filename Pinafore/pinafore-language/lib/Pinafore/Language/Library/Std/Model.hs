{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE ApplicativeDo #-}

module Pinafore.Language.Library.Std.Model
    ( modelLibEntries
    , morphismGroundType
    ) where

import Changes.Core
import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.Convert.Types
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std.Convert ()
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Language.Value
import Pinafore.Language.Var
import Shapes

-- LangModel
modelGroundType :: QGroundType '[] LangModel
modelGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangModel)|]) "Model"

instance HasQGroundType '[] LangModel where
    qGroundType = modelGroundType

-- WModel
instance (HasQType 'Positive t, HasQType 'Negative t) => HasQType 'Positive (WModel (WholeUpdate (Know t))) where
    qType = mapPosShimWit (functionToShim "wModelToWholeModel" wModelToWholeModel) qType

instance (HasQType 'Positive t, HasQType 'Negative t) => HasQType 'Negative (WModel (WholeUpdate (Know t))) where
    qType = mapNegShimWit (functionToShim "langWholeModelToValue" langWholeModelToValue) qType

-- WROWModel
instance (HasQType 'Negative a) => HasQType 'Negative (WROWModel (Know a)) where
    qType = mapNegShimWit (functionToShim "langWholeModelToReadOnlyValue" langWholeModelToReadOnlyValue) qType

instance (HasQType 'Positive a) => HasQType 'Positive (WROWModel (Know a)) where
    qType = mapPosShimWit (functionToShim "wROWModelToWholeModel" wROWModelToWholeModel) qType

-- SetModel
setModelGroundType :: QGroundType '[ ContraCCRVariance] LangSetModel
setModelGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangSetModel)|]) "SetModel"

instance HasQGroundType '[ ContraCCRVariance] LangSetModel where
    qGroundType = setModelGroundType

-- FiniteSetModel
finiteSetModelGroundType :: QGroundType '[ 'RangeCCRVariance] LangFiniteSetModel
finiteSetModelGroundType =
    stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangFiniteSetModel)|]) "FiniteSetModel"

instance HasQGroundType '[ 'RangeCCRVariance] LangFiniteSetModel where
    qGroundType = finiteSetModelGroundType

-- ListModel
listModelGroundType :: QGroundType '[ 'RangeCCRVariance] LangListModel
listModelGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangListModel)|]) "ListModel"

instance HasQGroundType '[ 'RangeCCRVariance] LangListModel where
    qGroundType = listModelGroundType

-- TextModel
textModelGroundType :: QGroundType '[] LangTextModel
textModelGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangTextModel)|]) "TextModel"

instance HasQGroundType '[] LangTextModel where
    qGroundType = textModelGroundType

-- ModelOrder
modelOrderGroundType :: QGroundType '[ ContraCCRVariance] LangModelOrder
modelOrderGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangModelOrder)|]) "ModelOrder"

instance HasQGroundType '[ ContraCCRVariance] LangModelOrder where
    qGroundType = modelOrderGroundType

-- LangMorphism
morphismGroundType :: QGroundType '[ 'RangeCCRVariance, 'RangeCCRVariance] LangMorphism
morphismGroundType =
    singleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangMorphism)|]) $ \ta tb ->
        namedTextPrec 2 $ precNamedText 1 ta <> " ~> " <> precNamedText 2 tb

instance HasQGroundType '[ 'RangeCCRVariance, 'RangeCCRVariance] LangMorphism where
    qGroundType = morphismGroundType

getFiniteSetModelList :: LangModelOrder A -> LangFiniteSetModel '( A, EnA) -> View (LangListModel '( TopType, A))
getFiniteSetModelList order val =
    modelOrderUpdateOrder order $ \(model :: Model update) uorder -> do
        let
            uo :: UpdateOrder (ContextUpdate update (ConstWholeUpdate EnA))
            uo =
                mapUpdateOrder
                    (liftContextChangeLens $ fromReadOnlyRejectingChangeLens . funcChangeLens (Known . meet2))
                    uorder
            rows :: Model (FiniteSetUpdate EnA)
            rows = unWModel $ unLangFiniteSetModel $ contraRangeLift meet2 val
            pkSub :: Model (ContextUpdate update (FiniteSetUpdate EnA))
            pkSub = contextModels model rows
        colSub :: Model (ContextUpdate update (OrderedListUpdate (ConstWholeUpdate EnA))) <-
            viewFloatMapModel (contextOrderedSetLens uo) pkSub
        return $
            OrderedLangListModel $
            eaMap (liftOrderedListChangeLens (constWholeChangeLens meet2) . tupleChangeLens SelectContent) $
            MkWModel colSub

setentity :: LangWholeModel '( A, TopType) -> A -> Action ()
setentity model val = langWholeModelSet model $ Known val

deleteentity :: LangWholeModel '( BottomType, TopType) -> Action ()
deleteentity model = langWholeModelSet model Unknown

modelLibEntries :: [BindDocTree context]
modelLibEntries =
    [ headingBDT
          "Models"
          "Models keep track of updates, and will update user interfaces constructed from them when their value changes."
          [ headingBDT
                "Models"
                ""
                [ typeBDT "Model" "" (MkSomeGroundType modelGroundType) []
                , valBDT "onModelUpdate" "Do this on every update, during this lifecycle." langModelSubscribe
                ]
          , headingBDT
                "Whole Models"
                ""
                [ typeBDT
                      "WholeModel"
                      "A whole model of type `WholeModel {-p,+q}` has a setting type of `p` and a getting type of `q`."
                      (MkSomeGroundType wholeModelGroundType)
                      []
                , hasSubtypeRelationBDT Verify "" $
                  functionToShim "WholeModel to Model" $ langWholeModelToModel @BottomType @TopType
                , valBDT "pureWholeModel" "A constant whole model for a value." (pure :: A -> ImmutableWholeModel A)
                , valBDT
                      "immutWholeModel"
                      "Convert a whole model to immutable.\n`immutWholeModel r = {%r}`"
                      (id :: ImmutableWholeModel A -> ImmutableWholeModel A)
                , valBDT
                      "coMapWholeModel"
                      "Map a function on getting a whole model."
                      (coRangeLift :: (A -> B) -> LangWholeModel '( C, A) -> LangWholeModel '( C, B))
                , valBDT
                      "contraMapWholeModel"
                      "Map a function on setting a whole model."
                      (contraRangeLift :: (B -> A) -> LangWholeModel '( A, C) -> LangWholeModel '( B, C))
                , valBDT "maybeLensMapWholeModel" "Map getter & pushback functions on a whole model." $
                  maybeLensLangWholeModel @AP @AQ @BP @BQ
                , valBDT "lensMapWholeModel" "Map getter & pushback functions on a whole model." $
                  fLensLangWholeModel @AP @AQ @B
                , valBDT "toMaybeWholeModel" "Map known/unknown to `Maybe` for a whole model." $
                  langToMaybeWholeModel @A @B
                , valBDT "fromMaybeWholeModel" "Map `Maybe` to known/unknown for a whole model." $
                  langFromMaybeWholeModel @A @B
                , valBDT "forWholeModel" "Traverse a list to make a model of a list." $
                  (for :: [A] -> (A -> ImmutableWholeModel B) -> ImmutableWholeModel [B])
                , valBDT "pairWholeModel" "Combine whole models." $ langPairWholeModels @AP @AQ @BP @BQ
                , valBDT
                      "applyWholeModel"
                      "Combine getting of whole models.\n`applyWholeModel f x` = `{%f %x}`"
                      ((<*>) :: ImmutableWholeModel (A -> B) -> ImmutableWholeModel A -> ImmutableWholeModel B)
                , valBDT
                      "unknown"
                      "The unknown whole model, representing missing information."
                      (empty :: ImmutableWholeModel BottomType)
                , valBDT "known" "True if the whole model is known." $ \(val :: WROWModel (Know TopType)) ->
                      (eaMapReadOnlyWhole (Known . isKnown) val :: WROWModel (Know Bool))
                , valBDT
                      "??"
                      "`p ?? q` = `p` if it is known, else `q`."
                      ((<|>) :: ImmutableWholeModel A -> ImmutableWholeModel A -> ImmutableWholeModel A)
                , valBDT "get" "Get a whole model, or `stop` if the whole model is unknown." $
                  langWholeModelGet @BottomType @A
                , valBDT ":=" "Set a whole model to a value. Stop if failed." setentity
                , valBDT "delete" "Delete a whole model (i.e., make unknown). Stop if failed." deleteentity
                , valBDT "onWholeModelUpdate" "Do an action initially and on every update, during this lifecycle." $
                  langWholeModelSubscribe @A
                , valBDT "newMemWholeModel" "Create a new whole model of memory, initially unknown." $
                  newMemWholeModel @A
                ]
          , headingBDT
                "Set Models"
                ""
                [ typeBDT "SetModel" "" (MkSomeGroundType setModelGroundType) []
                , hasSubtypeRelationBDT Verify "" $ functionToShim "SetModel to Model" $ langSetModelToModel @BottomType
                , valBDT
                      "mapSetModel"
                      "Map a function on a set."
                      (contramap :: (A -> B) -> LangSetModel B -> LangSetModel A)
                , valBDT "pureSetModel" "Convert a predicate to a set." $ predicateToLangSetModel @A
                , valBDT "predicateSetModel" "Convert a predicate model to a set." $ predicateModelToLangSetModel @A
                , valBDT "immutSetModel" "Convert a set to immutable." $ langSetModelImmutable @A
                , valBDT "+=" "Add an entity to a set." $ langSetModelAdd @A
                , valBDT "-=" "Remove an entity from a set." $ langSetModelRemove @A
                , valBDT "member" "A model of the membership of a value in a set." $ langSetModelMember @A
                , valBDT
                      "notSetModel"
                      "Complement of a set. The resulting set can be added to (deleting from the original set) and deleted from (adding to the original set)." $
                  langSetModelComplement @A
                , valBDT
                      "<&>"
                      "Intersection of sets. The resulting set can be added to (adding to both sets), but not deleted from." $
                  langSetModelIntersect @A
                , valBDT
                      "<|>"
                      "Union of sets. The resulting set can be deleted from (deleting from both sets), but not added to." $
                  langSetModelUnion @A
                , valBDT
                      "<\\>"
                      "Difference of sets, everything in the first set but not the second. The resulting set can be added to (adding to the first and deleting from the second), but not deleted from." $
                  langSetModelDifference @A
                , valBDT
                      "<^>"
                      "Symmetric difference of sets, everything in exactly one of the sets. The resulting set will be read-only." $
                  langSetModelSymmetricDifference @A
                , valBDT "<+>" "Cartesian sum of sets." $ langSetModelCartesianSum @A @B
                , valBDT "<*>" "Cartesian product of sets. The resulting set will be read-only." $
                  langSetModelCartesianProduct @A @B
                ]
          , headingBDT
                "Finite Set Models"
                ""
                [ typeBDT "FiniteSetModel" "" (MkSomeGroundType finiteSetModelGroundType) []
                , hasSubtypeRelationBDT Verify "" $
                  functionToShim "FiniteSetModel to SetModel" $ langFiniteSetModelToSetModel @A @TopType
                , valBDT
                      "coMapFiniteSetModel"
                      "Map a function on getting from a finite set."
                      (coRangeLift :: (A -> B) -> LangFiniteSetModel '( C, A) -> LangFiniteSetModel '( C, B))
                , valBDT
                      "contraMapFiniteSetModel"
                      "Map a function on setting to and testing a finite set."
                      (contraRangeLift :: (B -> A) -> LangFiniteSetModel '( A, C) -> LangFiniteSetModel '( B, C))
                , valBDT "maybeMapFiniteSetModel" "Map and filter a function on a finite set." $
                  langFiniteSetMaybeMap @AP @AQ @B
                , valBDT "<:&>" "Intersect a finite set with any set. The resulting finite set will be read-only." $
                  langFiniteSetModelSetIntersect @A @B
                , valBDT "<:\\>" "Difference of a finite set and any set. The resulting finite set will be read-only." $
                  langFiniteSetModelSetDifference @A @B
                , valBDT
                      "<:&:>"
                      "Intersection of finite sets. The resulting finite set can be added to, but not deleted from." $
                  langFiniteSetModelMeet @A
                , valBDT "<:|:>" "Union of finite sets. The resulting finite set can be deleted from, but not added to." $
                  langFiniteSetModelJoin @A
                , valBDT "<:+:>" "Cartesian sum of finite sets." $ langFiniteSetModelCartesianSum @AP @AQ @BP @BQ
                , valSupertypeBDT "<:+:>" "Cartesian sum of finite sets." $ langFiniteSetModelCartesianSum @A @A @B @B
                , valBDT "<:*:>" "Cartesian product of finite sets. The resulting finite set will be read-only." $
                  langFiniteSetModelCartesianProduct @AP @AQ @BP @BQ
                , valSupertypeBDT
                      "<:*:>"
                      "Cartesian product of finite sets. The resulting finite set will be read-only." $
                  langFiniteSetModelCartesianProduct @A @A @B @B
                , valBDT "finiteSetModelList" "All members of a finite set, by an order." $ finiteSetGetOrdered @A
                , valBDT
                      "getFiniteSetModelList"
                      "Get all members of a finite set, by an order. \
                    \The resulting `ListModel` will behave more \"list-like\" than `finiteSetModelList`."
                      getFiniteSetModelList
                , valBDT
                      "listFiniteSetModel"
                      "Represent a model of a list as a finite set. Changing the set may scramble the order of the list." $
                  langListModelToFiniteSetModel @A
                , valBDT "finiteSetModelSingle" "The member of a single-member finite set, or unknown." $
                  langFiniteSetModelSingle @A
                , valBDT "finiteSetModelCount" "Count of members in a finite set." $
                  langFiniteSetModelFunc @TopType @Int olength
                , valBDT
                      "finiteSetModelClear"
                      "Remove all entities from a finite set."
                      (langFiniteSetModelRemoveAll :: LangFiniteSetModel '( BottomType, TopType) -> Action ())
                , valBDT "newMemFiniteSetModel" "Create a new finite set model of memory, initially empty." $
                  newMemFiniteSetModel @A
                ]
          , headingBDT
                "List Models"
                ""
                [ typeBDT "ListModel" "" (MkSomeGroundType listModelGroundType) []
                , hasSubtypeRelationBDT TrustMe "" $
                  functionToShim "langListModelToModel" $ langListModelToModel @BottomType @TopType
                , hasSubtypeRelationBDT @(LangListModel '( A, A)) @(LangWholeModel '( Vector A, Vector A)) Verify "" $
                  functionToShim "langListModelToWholeModel" langListModelToWholeModel
                , valBDT "wholeModelList" "Represent a whole model as a list model." $ langWholeModelToListModel @A
                , valBDT "listModelGetCount" "Get Count of elements in a list model." $
                  langListModelGetCount @BottomType @TopType
                , valBDT "listModelGetItem" "Get an element of a list model." $ langListModelGetItem @BottomType @Q
                , valBDT "listModelInsert" "Insert an element in a list model." $ langListModelInsert @P @TopType
                , valBDT "listModelSet" "Set an element of a list model." $ langListModelSet @P @TopType
                , valBDT "listModelDelete" "Delete an element of a list model." $
                  langListModelDelete @BottomType @TopType
                , valBDT "listModelClear" "Delete all elements of a list model." $
                  langListModelClear @BottomType @TopType
                , valBDT "listModelCount" "Model of a count of elements in a list model." $
                  langListModelCountModel @BottomType @TopType
                , valBDT
                      "listModelItem"
                      "Get a whole model of a particular item in the list. It will track the item as the list changes. Pass `True` for an existing item, `False` for a point between items." $
                  langListModelItem @P @Q
                , valBDT "immutListModel" "Convert a list model to immutable." $
                  langImmutListModel @BottomType @A @TopType
                , valBDT "newMemListModel" "Create a new list model of memory, initially empty." $ newMemListModel @A
                ]
          , headingBDT
                "Text Models"
                ""
                [ typeBDT "TextModel" "" (MkSomeGroundType textModelGroundType) []
                , hasSubtypeRelationBDT @LangTextModel @(LangWholeModel '( Text, Text)) Verify "" $
                  functionToShim "langTextModelToWholeModel" langTextModelToWholeModel
                , valBDT "wholeModelText" "Represent a whole model as a text model." langWholeModelToTextModel
                , valBDT "textModelGetLength" "Get the length of text." langTextModelGetLength
                , valBDT "textModelGet" "Get the whole text." langTextModelGet
                , valBDT "textModelSet" "Set the whole text." langTextModelSet
                , valBDT "textModelGetSection" "Get a (start,length) section of the text." langTextModelGetSection
                , valBDT "textModelSetSection" "Set a (start,length) section of the text." langTextModelSetSection
                , valBDT
                      "textModelSection"
                      "Create a model of a (start,length) section of a text model. It will track the section as the text changes."
                      langTextModelSection
                , valBDT "newMemTextModel" "Create a new text model of memory, initially empty." newMemTextModel
                ]
          ]
    , headingBDT
          "Morphisms"
          "Morphisms relate entities."
          [ typeBDT "~>" "" (MkSomeGroundType morphismGroundType) []
          , valBDT "identity" "The identity morphism." $ identityLangMorphism @X @Y
          , valBDT "!." "Compose morphisms." $ composeLangMorphism @AP @AQ @BX @BY @CP @CQ
          , valSupertypeBDT "!." "Compose morphisms." $ composeLangMorphism @A @A @B @B @C @C
          , valBDT "!**" "Pair morphisms. Models from these morphisms are undeleteable." $
            pairLangMorphism @AP @AQ @BP @BQ @CP @CQ
          , valSupertypeBDT "!**" "Pair morphisms. Models from these morphisms are undeleteable." $
            pairLangMorphism @A @A @B @B @C @C
          , valBDT "!++" "Either morphisms. Models from these morphisms are undeleteable." $
            eitherLangMorphism @AP @AQ @BP @BQ @CP @CQ
          , valSupertypeBDT "!++" "Either morphisms. Models from these morphisms are undeleteable." $
            eitherLangMorphism @A @A @B @B @C @C
          , valBDT "!$" "Apply a morphism to a model." $ applyLangMorphismModel @AP @AQ @BP @BQ
          , valSupertypeBDT "!$" "Apply a morphism to a model." $ applyLangMorphismModel @A @A @B @B
          , valBDT "!$%" "Apply a morphism to an immutable model.\n`m !$% r = m !$ immutWholeModel r`" $
            applyLangMorphismImmutModel @A @BP @BQ
          , valSupertypeBDT "!$%" "Apply a morphism to an immutable model.\n`m !$% r = m !$ immutWholeModel r`" $
            applyLangMorphismImmutModel @A @B @B
          , valBDT "!$$" "Apply a morphism to a set." $ applyLangMorphismSet @A @B
          , valBDT "!@" "Co-apply a morphism to a model." $ inverseApplyLangMorphismModel @A @BX @BY
          , valSupertypeBDT "!@" "Co-apply a morphism to a model." $ inverseApplyLangMorphismModel @A @B @B
          , valBDT "!@%" "Co-apply a morphism to an immutable model.\n`m !@% r = m !@ immutWholeModel r`" $
            inverseApplyLangMorphismImmutModel @A @B
          , valBDT "!@@" "Co-apply a morphism to a set." $ inverseApplyLangMorphismSet @A @BX @BY
          , valSupertypeBDT "!@@" "Co-apply a morphism to a set." $ inverseApplyLangMorphismSet @A @B @B
          ]
    , headingBDT
          "ModelOrders"
          ""
          [ typeBDT "ModelOrder" "" (MkSomeGroundType modelOrderGroundType) []
          , hasSubtypeRelationBDT Verify "" $ functionToShim "Order to ModelOrder" $ pureLangModelOrder @A
          , valBDT "orders" "Join `ModelOrder`s by priority." $ modelOrders @A
          , valBDT
                "mapOrder"
                "Map a function on a `ModelOrder`."
                (contramap :: (B -> A) -> LangModelOrder A -> LangModelOrder B)
          , valBDT "orderOn" "Order by a `ModelOrder` on a particular morphism." $ langModelOrderOn @B @A
          , valBDT "reverseOrder" "Reverse a `ModelOrder`." $ reverseLangModelOrder @A
          , valBDT "orderWhole" "Order two whole models." $ langModelOrderCompare @A
          ]
    ]
