{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE ApplicativeDo #-}

module Pinafore.Language.Library.Model
    ( modelLibSection
    , modelOrderGroundType
    ) where

import Changes.Core
import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.Convert.Types
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
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

setentity :: LangWholeModel '( A, TopType) -> A -> Action ()
setentity model val = langWholeModelSet model $ Known val

deleteentity :: LangWholeModel '( BottomType, TopType) -> Action ()
deleteentity model = langWholeModelSet model Unknown

getFiniteSetModelList :: LangModelOrder A -> LangFiniteSetModel '( BottomType, A) -> View (LangListModel '( TopType, A))
getFiniteSetModelList order (MkLangFiniteSetModel r (val :: WModel (FiniteSetUpdate t))) =
    modelOrderUpdateOrder order $ \(model :: Model update) uorder -> do
        let
            conv :: t -> A
            conv = shimToFunction $ rangeCo r
            uo :: UpdateOrder (ContextUpdate update (ConstWholeUpdate t))
            uo =
                mapUpdateOrder
                    (liftContextChangeLens $ fromReadOnlyRejectingChangeLens . funcChangeLens (Known . conv))
                    uorder
            rows :: Model (FiniteSetUpdate t)
            rows = unWModel val
            pkSub :: Model (ContextUpdate update (FiniteSetUpdate t))
            pkSub = contextModels model rows
        colSub :: Model (ContextUpdate update (OrderedListUpdate (ConstWholeUpdate t))) <-
            viewFloatMapModel (contextOrderedSetLens uo) pkSub
        return $
            OrderedLangListModel $
            eaMap (liftOrderedListChangeLens (constWholeChangeLens conv) . tupleChangeLens SelectContent) $
            MkWModel colSub

modelLibSection :: LibraryStuff context
modelLibSection =
    headingBDS
        "Models"
        "Models keep track of updates, and will update user interfaces constructed from them when their value changes."
        [ headingBDS
              "Model"
              ""
              [ typeBDS "Model" "" (MkSomeGroundType modelGroundType) []
              , namespaceBDS
                    "Model"
                    [ addNameInRootBDS $
                      valBDS "onUpdate" "Do this on every update, during this lifecycle." langModelSubscribe
                    ]
              ]
        , headingBDS
              "WholeModel"
              ""
              [ typeBDS
                    "WholeModel"
                    "A whole model of type `WholeModel {-p,+q}` has a setting type of `p` and a getting type of `q`."
                    (MkSomeGroundType wholeModelGroundType)
                    []
              , hasSubtypeRelationBDS Verify "" $
                functionToShim "WholeModel to Model" $ langWholeModelToModel @BottomType @TopType
              , namespaceBDS "WholeModel" $
                applicativeEntries @_ @ImmutableWholeModel <>
                [ valBDS
                      "immut"
                      "Convert a whole model to immutable.\n`immut.WholeModel r = {%r}`"
                      (id :: ImmutableWholeModel A -> ImmutableWholeModel A)
                , valBDS
                      "coMap"
                      "Map a function on getting a whole model."
                      (coRangeLift :: (A -> B) -> LangWholeModel '( C, A) -> LangWholeModel '( C, B))
                , valBDS
                      "contraMap"
                      "Map a function on setting a whole model."
                      (contraRangeLift :: (B -> A) -> LangWholeModel '( A, C) -> LangWholeModel '( B, C))
                , valBDS "maybeLensMap" "Map getter & pushback functions on a whole model." $
                  maybeLensLangWholeModel @AP @AQ @BP @BQ
                , valBDS "lensMap" "Map getter & pushback functions on a whole model." $ fLensLangWholeModel @AP @AQ @B
                , valBDS "toMaybe" "Map known/unknown to `Maybe` for a whole model." $ langToMaybeWholeModel @A @B
                , valBDS "fromMaybe" "Map `Maybe` to known/unknown for a whole model." $ langFromMaybeWholeModel @A @B
                , valBDS "for" "Traverse a list to make a model of a list." $
                  (for :: [A] -> (A -> ImmutableWholeModel B) -> ImmutableWholeModel [B])
                , valBDS "product" "Combine whole models." $ langPairWholeModels @AP @AQ @BP @BQ
                , addNameInRootBDS $
                  valBDS
                      "unknown"
                      "The unknown whole model, representing missing information."
                      (empty :: ImmutableWholeModel BottomType)
                , addNameInRootBDS $
                  valBDS "known" "True if the whole model is known." $ \(val :: WROWModel (Know TopType)) ->
                      (eaMapReadOnlyWhole (Known . isKnown) val :: WROWModel (Know Bool))
                , addNameInRootBDS $
                  valBDS
                      "??"
                      "`p ?? q` = `p` if it is known, else `q`."
                      ((<|>) :: ImmutableWholeModel A -> ImmutableWholeModel A -> ImmutableWholeModel A)
                , addNameInRootBDS $
                  valBDS "get" "Get a whole model, or `stop` if the whole model is unknown." $
                  langWholeModelGet @BottomType @A
                , addNameInRootBDS $ valBDS ":=" "Set a whole model to a value. Stop if failed." setentity
                , addNameInRootBDS $
                  valBDS "delete" "Delete a whole model (i.e., make unknown). Stop if failed." deleteentity
                , valBDS "onUpdate" "Do an action initially and on every update, during this lifecycle." $
                  langWholeModelSubscribe @A
                , valBDS "newMem" "Create a new whole model of memory, initially unknown." $ newMemWholeModel @A
                ]
              ]
        , headingBDS
              "SetModel"
              ""
              [ typeBDS "SetModel" "" (MkSomeGroundType setModelGroundType) []
              , hasSubtypeRelationBDS Verify "" $ functionToShim "SetModel to Model" $ langSetModelToModel @BottomType
              , namespaceBDS
                    "SetModel"
                    [ valBDS
                          "map"
                          "Map a function on a set."
                          (contramap :: (A -> B) -> LangSetModel B -> LangSetModel A)
                    , valBDS "pure" "Convert a predicate to a set." $ predicateToLangSetModel @A
                    , valBDS "predicate" "Convert a predicate model to a set." $ predicateModelToLangSetModel @A
                    , valBDS "immut" "Convert a set to immutable." $ langSetModelImmutable @A
                    , addNameInRootBDS $ valBDS "+=" "Add an entity to a set." $ langSetModelAdd @A
                    , addNameInRootBDS $ valBDS "-=" "Remove an entity from a set." $ langSetModelRemove @A
                    , addNameInRootBDS $
                      valBDS "member" "A model of the membership of a value in a set." $ langSetModelMember @A
                    , valBDS "empty" "The immutable empty set." $ langSetModelEmpty @Entity
                    , valBDS "full" "The immutable full set." $ langSetModelFull @Entity
                    , valBDS
                          "not"
                          "Complement of a set. The resulting set can be added to (deleting from the original set) and deleted from (adding to the original set)." $
                      langSetModelComplement @A
                    , addNameInRootBDS $
                      valBDS
                          "<&>"
                          "Intersection of sets. The resulting set can be added to (adding to both sets), but not deleted from." $
                      langSetModelIntersect @A
                    , addNameInRootBDS $
                      valBDS
                          "<|>"
                          "Union of sets. The resulting set can be deleted from (deleting from both sets), but not added to." $
                      langSetModelUnion @A
                    , addNameInRootBDS $
                      valBDS
                          "<\\>"
                          "Difference of sets, everything in the first set but not the second. The resulting set can be added to (adding to the first and deleting from the second), but not deleted from." $
                      langSetModelDifference @A
                    , addNameInRootBDS $
                      valBDS
                          "<^>"
                          "Symmetric difference of sets, everything in exactly one of the sets. The resulting set will be read-only." $
                      langSetModelSymmetricDifference @A
                    , addNameInRootBDS $ valBDS "<+>" "Cartesian sum of sets." $ langSetModelCartesianSum @A @B
                    , addNameInRootBDS $
                      valBDS "<*>" "Cartesian product of sets. The resulting set will be read-only." $
                      langSetModelCartesianProduct @A @B
                    ]
              ]
        , headingBDS
              "FiniteSetModel"
              ""
              [ typeBDS "FiniteSetModel" "" (MkSomeGroundType finiteSetModelGroundType) []
              , hasSubtypeRelationBDS Verify "" $
                functionToShim "FiniteSetModel to SetModel" $ langFiniteSetModelToSetModel @A @TopType
              , namespaceBDS
                    "FiniteSetModel"
                    [ valBDS
                          "coMap"
                          "Map a function on getting from a finite set."
                          (coRangeLift :: (A -> B) -> LangFiniteSetModel '( C, A) -> LangFiniteSetModel '( C, B))
                    , valBDS
                          "contraMap"
                          "Map a function on setting to and testing a finite set."
                          (contraRangeLift :: (B -> A) -> LangFiniteSetModel '( A, C) -> LangFiniteSetModel '( B, C))
                    , valBDS "filter" "Filter a finite set." $ langFiniteSetFilter @AP @AQ
                    , valBDS "maybeMap" "Map and filter a function on a finite set." $ langFiniteSetMaybeMap @AP @AQ @B
                    , valBDS "collect" "Map and filter a function from a model on a finite set." $
                      langFiniteSetCollect @AP @AQ @B
                    , addNameInRootBDS $
                      valBDS "<:&>" "Intersect a finite set with any set. The resulting finite set will be read-only." $
                      langFiniteSetModelSetIntersect @A @B
                    , addNameInRootBDS $
                      valBDS
                          "<:\\>"
                          "Difference of a finite set and any set. The resulting finite set will be read-only." $
                      langFiniteSetModelSetDifference @A @B
                    , addNameInRootBDS $
                      valBDS
                          "<:&:>"
                          "Intersection of finite sets. The resulting finite set can be added to, but not deleted from." $
                      langFiniteSetModelMeet @A
                    , addNameInRootBDS $
                      valBDS
                          "<:|:>"
                          "Union of finite sets. The resulting finite set can be deleted from, but not added to." $
                      langFiniteSetModelJoin @A
                    , addNameInRootBDS $
                      valBDS "<:+:>" "Cartesian sum of finite sets." $ langFiniteSetModelCartesianSum @AP @AQ @BP @BQ
                    , addNameInRootBDS $
                      valBDS "<:*:>" "Cartesian product of finite sets. The resulting finite set will be read-only." $
                      langFiniteSetModelCartesianProduct @AP @AQ @BP @BQ
                    , valBDS
                          "fromList"
                          "Represent a model of a list as a finite set. Changing the set may scramble the order of the list." $
                      langListModelToFiniteSetModel @A
                    , valBDS "single" "The member of a single-member finite set, or unknown." $
                      langFiniteSetModelSingle @A
                    , valBDS "count" "Count of members in a finite set." $ langFiniteSetModelFunc @TopType @Int olength
                    , valBDS
                          "clear"
                          "Remove all entities from a finite set."
                          (langFiniteSetModelRemoveAll :: LangFiniteSetModel '( BottomType, TopType) -> Action ())
                    , valBDS "toList" "All members of a finite set, by an order." $ finiteSetGetOrdered @A
                    , valBDS
                          "getList"
                          "Get all members of a finite set, by an order. \
                                    \The resulting `ListModel` will behave more \"list-like\" than `toList.FiniteSetModel`."
                          getFiniteSetModelList
                    , valBDS "newMem" "Create a new finite set model of memory, initially empty." $
                      newMemFiniteSetModel @A
                    ]
              ]
        , headingBDS
              "ListModel"
              ""
              [ typeBDS "ListModel" "" (MkSomeGroundType listModelGroundType) []
              , hasSubtypeRelationBDS TrustMe "" $
                functionToShim "langListModelToModel" $ langListModelToModel @BottomType @TopType
              , hasSubtypeRelationBDS @(LangListModel '( A, A)) @(LangWholeModel '( Vector A, Vector A)) Verify "" $
                functionToShim "langListModelToWholeModel" langListModelToWholeModel
              , namespaceBDS
                    "ListModel"
                    [ valBDS "fromWhole" "Represent a whole model as a list model." $ langWholeModelToListModel @A
                    , valBDS "getCount" "Get Count of elements in a list model." $
                      langListModelGetCount @BottomType @TopType
                    , valBDS "getItem" "Get an element of a list model." $ langListModelGetItem @BottomType @Q
                    , valBDS "insert" "Insert an element in a list model." $ langListModelInsert @P @TopType
                    , valBDS "set" "Set an element of a list model." $ langListModelSet @P @TopType
                    , valBDS "delete" "Delete an element of a list model." $ langListModelDelete @BottomType @TopType
                    , valBDS "clear" "Delete all elements of a list model." $ langListModelClear @BottomType @TopType
                    , valBDS "count" "Model of a count of elements in a list model." $
                      langListModelCountModel @BottomType @TopType
                    , valBDS
                          "item"
                          "Get a whole model of a particular item in the list. It will track the item as the list changes. Pass `True` for an existing item, `False` for a point between items." $
                      langListModelItem @P @Q
                    , valBDS "immut" "Convert a list model to immutable." $ langImmutListModel @BottomType @A @TopType
                    , valBDS "newMem" "Create a new list model of memory, initially empty." $ newMemListModel @A
                    ]
              ]
        , headingBDS
              "TextModel"
              ""
              [ typeBDS "TextModel" "" (MkSomeGroundType textModelGroundType) []
              , hasSubtypeRelationBDS @LangTextModel @(LangWholeModel '( Text, Text)) Verify "" $
                functionToShim "langTextModelToWholeModel" langTextModelToWholeModel
              , namespaceBDS
                    "TextModel"
                    [ valBDS "fromWhole" "Represent a whole model as a text model." langWholeModelToTextModel
                    , valBDS "getLength" "Get the length of text." langTextModelGetLength
                    , valBDS "get" "Get the whole text." langTextModelGet
                    , valBDS "set" "Set the whole text." langTextModelSet
                    , valBDS "getSection" "Get a (start,length) section of the text." langTextModelGetSection
                    , valBDS "setSection" "Set a (start,length) section of the text." langTextModelSetSection
                    , valBDS
                          "section"
                          "Create a model of a (start,length) section of a text model. It will track the section as the text changes."
                          langTextModelSection
                    , valBDS "newMem" "Create a new text model of memory, initially empty." newMemTextModel
                    ]
              ]
        ]
