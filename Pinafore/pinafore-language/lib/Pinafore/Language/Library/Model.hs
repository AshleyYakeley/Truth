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

modelLibSection :: BindDocTree context
modelLibSection =
    headingBDT
        "Models"
        "Models keep track of updates, and will update user interfaces constructed from them when their value changes."
        [ headingBDT
              "Model"
              ""
              [ typeBDT "Model" "" (MkSomeGroundType modelGroundType) []
              , namespaceBDT
                    "Model"
                    ""
                    [ nameInRootBDT $
                      valBDT "onUpdate" "Do this on every update, during this lifecycle." langModelSubscribe
                    ]
              ]
        , headingBDT
              "WholeModel"
              ""
              [ typeBDT
                    "WholeModel"
                    "A whole model of type `WholeModel {-p,+q}` has a setting type of `p` and a getting type of `q`."
                    (MkSomeGroundType wholeModelGroundType)
                    []
              , hasSubtypeRelationBDT Verify "" $
                functionToShim "WholeModel to Model" $ langWholeModelToModel @BottomType @TopType
              , namespaceBDT "WholeModel" "" $
                applicativeEntries @_ @ImmutableWholeModel <>
                [ valBDT
                      "immut"
                      "Convert a whole model to immutable.\n`immut.WholeModel r = {%r}`"
                      (id :: ImmutableWholeModel A -> ImmutableWholeModel A)
                , valBDT
                      "coMap"
                      "Map a function on getting a whole model."
                      (coRangeLift :: (A -> B) -> LangWholeModel '( C, A) -> LangWholeModel '( C, B))
                , valBDT
                      "contraMap"
                      "Map a function on setting a whole model."
                      (contraRangeLift :: (B -> A) -> LangWholeModel '( A, C) -> LangWholeModel '( B, C))
                , valBDT "maybeLensMap" "Map getter & pushback functions on a whole model." $
                  maybeLensLangWholeModel @AP @AQ @BP @BQ
                , valBDT "lensMap" "Map getter & pushback functions on a whole model." $ fLensLangWholeModel @AP @AQ @B
                , valBDT "toMaybe" "Map known/unknown to `Maybe` for a whole model." $ langToMaybeWholeModel @A @B
                , valBDT "fromMaybe" "Map `Maybe` to known/unknown for a whole model." $ langFromMaybeWholeModel @A @B
                , valBDT "for" "Traverse a list to make a model of a list." $
                  (for :: [A] -> (A -> ImmutableWholeModel B) -> ImmutableWholeModel [B])
                , valBDT "product" "Combine whole models." $ langPairWholeModels @AP @AQ @BP @BQ
                , nameInRootBDT $
                  valBDT
                      "unknown"
                      "The unknown whole model, representing missing information."
                      (empty :: ImmutableWholeModel BottomType)
                , nameInRootBDT $
                  valBDT "known" "True if the whole model is known." $ \(val :: WROWModel (Know TopType)) ->
                      (eaMapReadOnlyWhole (Known . isKnown) val :: WROWModel (Know Bool))
                , nameInRootBDT $
                  valBDT
                      "??"
                      "`p ?? q` = `p` if it is known, else `q`."
                      ((<|>) :: ImmutableWholeModel A -> ImmutableWholeModel A -> ImmutableWholeModel A)
                , nameInRootBDT $
                  valBDT "get" "Get a whole model, or `stop` if the whole model is unknown." $
                  langWholeModelGet @BottomType @A
                , nameInRootBDT $ valBDT ":=" "Set a whole model to a value. Stop if failed." setentity
                , nameInRootBDT $
                  valBDT "delete" "Delete a whole model (i.e., make unknown). Stop if failed." deleteentity
                , valBDT "onUpdate" "Do an action initially and on every update, during this lifecycle." $
                  langWholeModelSubscribe @A
                , valBDT "newMem" "Create a new whole model of memory, initially unknown." $ newMemWholeModel @A
                ]
              ]
        , headingBDT
              "SetModel"
              ""
              [ typeBDT "SetModel" "" (MkSomeGroundType setModelGroundType) []
              , hasSubtypeRelationBDT Verify "" $ functionToShim "SetModel to Model" $ langSetModelToModel @BottomType
              , namespaceBDT
                    "SetModel"
                    ""
                    [ valBDT
                          "map"
                          "Map a function on a set."
                          (contramap :: (A -> B) -> LangSetModel B -> LangSetModel A)
                    , valBDT "pure" "Convert a predicate to a set." $ predicateToLangSetModel @A
                    , valBDT "predicate" "Convert a predicate model to a set." $ predicateModelToLangSetModel @A
                    , valBDT "immut" "Convert a set to immutable." $ langSetModelImmutable @A
                    , nameInRootBDT $ valBDT "+=" "Add an entity to a set." $ langSetModelAdd @A
                    , nameInRootBDT $ valBDT "-=" "Remove an entity from a set." $ langSetModelRemove @A
                    , nameInRootBDT $
                      valBDT "member" "A model of the membership of a value in a set." $ langSetModelMember @A
                    , valBDT "empty" "The immutable empty set." $ langSetModelEmpty @Entity
                    , valBDT "full" "The immutable full set." $ langSetModelFull @Entity
                    , valBDT
                          "not"
                          "Complement of a set. The resulting set can be added to (deleting from the original set) and deleted from (adding to the original set)." $
                      langSetModelComplement @A
                    , nameInRootBDT $
                      valBDT
                          "<&>"
                          "Intersection of sets. The resulting set can be added to (adding to both sets), but not deleted from." $
                      langSetModelIntersect @A
                    , nameInRootBDT $
                      valBDT
                          "<|>"
                          "Union of sets. The resulting set can be deleted from (deleting from both sets), but not added to." $
                      langSetModelUnion @A
                    , nameInRootBDT $
                      valBDT
                          "<\\>"
                          "Difference of sets, everything in the first set but not the second. The resulting set can be added to (adding to the first and deleting from the second), but not deleted from." $
                      langSetModelDifference @A
                    , nameInRootBDT $
                      valBDT
                          "<^>"
                          "Symmetric difference of sets, everything in exactly one of the sets. The resulting set will be read-only." $
                      langSetModelSymmetricDifference @A
                    , nameInRootBDT $ valBDT "<+>" "Cartesian sum of sets." $ langSetModelCartesianSum @A @B
                    , nameInRootBDT $
                      valBDT "<*>" "Cartesian product of sets. The resulting set will be read-only." $
                      langSetModelCartesianProduct @A @B
                    ]
              ]
        , headingBDT
              "FiniteSetModel"
              ""
              [ typeBDT "FiniteSetModel" "" (MkSomeGroundType finiteSetModelGroundType) []
              , hasSubtypeRelationBDT Verify "" $
                functionToShim "FiniteSetModel to SetModel" $ langFiniteSetModelToSetModel @A @TopType
              , namespaceBDT
                    "FiniteSetModel"
                    ""
                    [ valBDT
                          "coMap"
                          "Map a function on getting from a finite set."
                          (coRangeLift :: (A -> B) -> LangFiniteSetModel '( C, A) -> LangFiniteSetModel '( C, B))
                    , valBDT
                          "contraMap"
                          "Map a function on setting to and testing a finite set."
                          (contraRangeLift :: (B -> A) -> LangFiniteSetModel '( A, C) -> LangFiniteSetModel '( B, C))
                    , valBDT "maybeMap" "Map and filter a function on a finite set." $ langFiniteSetMaybeMap @AP @AQ @B
                    , nameInRootBDT $
                      valBDT "<:&>" "Intersect a finite set with any set. The resulting finite set will be read-only." $
                      langFiniteSetModelSetIntersect @A @B
                    , nameInRootBDT $
                      valBDT
                          "<:\\>"
                          "Difference of a finite set and any set. The resulting finite set will be read-only." $
                      langFiniteSetModelSetDifference @A @B
                    , nameInRootBDT $
                      valBDT
                          "<:&:>"
                          "Intersection of finite sets. The resulting finite set can be added to, but not deleted from." $
                      langFiniteSetModelMeet @A
                    , nameInRootBDT $
                      valBDT
                          "<:|:>"
                          "Union of finite sets. The resulting finite set can be deleted from, but not added to." $
                      langFiniteSetModelJoin @A
                    , nameInRootBDT $
                      valBDT "<:+:>" "Cartesian sum of finite sets." $ langFiniteSetModelCartesianSum @AP @AQ @BP @BQ
                    , nameInRootBDT $
                      valBDT "<:*:>" "Cartesian product of finite sets. The resulting finite set will be read-only." $
                      langFiniteSetModelCartesianProduct @AP @AQ @BP @BQ
                    , valBDT
                          "fromList"
                          "Represent a model of a list as a finite set. Changing the set may scramble the order of the list." $
                      langListModelToFiniteSetModel @A
                    , valBDT "single" "The member of a single-member finite set, or unknown." $
                      langFiniteSetModelSingle @A
                    , valBDT "count" "Count of members in a finite set." $ langFiniteSetModelFunc @TopType @Int olength
                    , valBDT
                          "clear"
                          "Remove all entities from a finite set."
                          (langFiniteSetModelRemoveAll :: LangFiniteSetModel '( BottomType, TopType) -> Action ())
                    , valBDT "toList" "All members of a finite set, by an order." $ finiteSetGetOrdered @A
                    , valBDT
                          "getList"
                          "Get all members of a finite set, by an order. \
                                    \The resulting `ListModel` will behave more \"list-like\" than `toList.FiniteSetModel`."
                          getFiniteSetModelList
                    , valBDT "newMem" "Create a new finite set model of memory, initially empty." $
                      newMemFiniteSetModel @A
                    ]
              ]
        , headingBDT
              "ListModel"
              ""
              [ typeBDT "ListModel" "" (MkSomeGroundType listModelGroundType) []
              , hasSubtypeRelationBDT TrustMe "" $
                functionToShim "langListModelToModel" $ langListModelToModel @BottomType @TopType
              , hasSubtypeRelationBDT @(LangListModel '( A, A)) @(LangWholeModel '( Vector A, Vector A)) Verify "" $
                functionToShim "langListModelToWholeModel" langListModelToWholeModel
              , namespaceBDT
                    "ListModel"
                    ""
                    [ valBDT "fromWhole" "Represent a whole model as a list model." $ langWholeModelToListModel @A
                    , valBDT "getCount" "Get Count of elements in a list model." $
                      langListModelGetCount @BottomType @TopType
                    , valBDT "getItem" "Get an element of a list model." $ langListModelGetItem @BottomType @Q
                    , valBDT "insert" "Insert an element in a list model." $ langListModelInsert @P @TopType
                    , valBDT "set" "Set an element of a list model." $ langListModelSet @P @TopType
                    , valBDT "delete" "Delete an element of a list model." $ langListModelDelete @BottomType @TopType
                    , valBDT "clear" "Delete all elements of a list model." $ langListModelClear @BottomType @TopType
                    , valBDT "count" "Model of a count of elements in a list model." $
                      langListModelCountModel @BottomType @TopType
                    , valBDT
                          "item"
                          "Get a whole model of a particular item in the list. It will track the item as the list changes. Pass `True` for an existing item, `False` for a point between items." $
                      langListModelItem @P @Q
                    , valBDT "immut" "Convert a list model to immutable." $ langImmutListModel @BottomType @A @TopType
                    , valBDT "newMem" "Create a new list model of memory, initially empty." $ newMemListModel @A
                    ]
              ]
        , headingBDT
              "TextModel"
              ""
              [ typeBDT "TextModel" "" (MkSomeGroundType textModelGroundType) []
              , hasSubtypeRelationBDT @LangTextModel @(LangWholeModel '( Text, Text)) Verify "" $
                functionToShim "langTextModelToWholeModel" langTextModelToWholeModel
              , namespaceBDT
                    "TextModel"
                    ""
                    [ valBDT "fromWhole" "Represent a whole model as a text model." langWholeModelToTextModel
                    , valBDT "getLength" "Get the length of text." langTextModelGetLength
                    , valBDT "get" "Get the whole text." langTextModelGet
                    , valBDT "set" "Set the whole text." langTextModelSet
                    , valBDT "getSection" "Get a (start,length) section of the text." langTextModelGetSection
                    , valBDT "setSection" "Set a (start,length) section of the text." langTextModelSetSection
                    , valBDT
                          "section"
                          "Create a model of a (start,length) section of a text model. It will track the section as the text changes."
                          langTextModelSection
                    , valBDT "newMem" "Create a new text model of memory, initially empty." newMemTextModel
                    ]
              ]
        ]
