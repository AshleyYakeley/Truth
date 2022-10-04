{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE ApplicativeDo #-}

module Pinafore.Language.Library.Std.Model
    ( modelLibEntries
    ) where

import Changes.Core
import Pinafore.Base
import Pinafore.Context
import Pinafore.Language.Convert
import Pinafore.Language.Convert.Types
import Pinafore.Language.DocTree
import Pinafore.Language.ExprShow
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std.Convert ()
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type
import Pinafore.Language.Value
import Pinafore.Language.Var
import Shapes

-- LangModel
modelGroundType :: PinaforeGroundType '[] LangModel
modelGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangModel)|]) "Model"

instance HasPinaforeGroundType '[] LangModel where
    pinaforeGroundType = modelGroundType

-- WModel
instance (HasPinaforeType 'Positive t, HasPinaforeType 'Negative t) =>
             HasPinaforeType 'Positive (WModel (WholeUpdate (Know t))) where
    pinaforeType = mapPosShimWit (functionToShim "pinaforeModelToWholeModel" pinaforeModelToWholeModel) pinaforeType

instance (HasPinaforeType 'Positive t, HasPinaforeType 'Negative t) =>
             HasPinaforeType 'Negative (WModel (WholeUpdate (Know t))) where
    pinaforeType = mapNegShimWit (functionToShim "langWholeModelToValue" langWholeModelToValue) pinaforeType

-- PinaforeROWModel
instance (HasPinaforeType 'Negative a) => HasPinaforeType 'Negative (PinaforeROWModel (Know a)) where
    pinaforeType =
        mapNegShimWit (functionToShim "langWholeModelToReadOnlyValue" langWholeModelToReadOnlyValue) pinaforeType

instance (HasPinaforeType 'Positive a) => HasPinaforeType 'Positive (PinaforeROWModel (Know a)) where
    pinaforeType =
        mapPosShimWit (functionToShim "pinaforeROWModelToWholeModel" pinaforeROWModelToWholeModel) pinaforeType

-- SetModel
setModelGroundType :: PinaforeGroundType '[ ContraCCRVariance] LangSetModel
setModelGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangSetModel)|]) "SetModel"

instance HasPinaforeGroundType '[ ContraCCRVariance] LangSetModel where
    pinaforeGroundType = setModelGroundType

-- FiniteSetModel
finiteSetModelGroundType :: PinaforeGroundType '[ 'RangeCCRVariance] LangFiniteSetModel
finiteSetModelGroundType =
    stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangFiniteSetModel)|]) "FiniteSetModel"

instance HasPinaforeGroundType '[ 'RangeCCRVariance] LangFiniteSetModel where
    pinaforeGroundType = finiteSetModelGroundType

-- ListModel
listModelGroundType :: PinaforeGroundType '[ 'RangeCCRVariance] LangListModel
listModelGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangListModel)|]) "ListModel"

instance HasPinaforeGroundType '[ 'RangeCCRVariance] LangListModel where
    pinaforeGroundType = listModelGroundType

-- TextModel
textModelGroundType :: PinaforeGroundType '[] LangTextModel
textModelGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangTextModel)|]) "TextModel"

instance HasPinaforeGroundType '[] LangTextModel where
    pinaforeGroundType = textModelGroundType

-- ModelOrder
modelOrderGroundType :: PinaforeGroundType '[ ContraCCRVariance] LangModelOrder
modelOrderGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangModelOrder)|]) "ModelOrder"

instance HasPinaforeGroundType '[ ContraCCRVariance] LangModelOrder where
    pinaforeGroundType = modelOrderGroundType

-- LangMorphism
morphismGroundType :: PinaforeGroundType '[ 'RangeCCRVariance, 'RangeCCRVariance] LangMorphism
morphismGroundType =
    singleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangMorphism)|]) $ \ta tb ->
        (precShow 1 ta <> " ~> " <> precShow 2 tb, 2)

instance HasPinaforeGroundType '[ 'RangeCCRVariance, 'RangeCCRVariance] LangMorphism where
    pinaforeGroundType = morphismGroundType

getFiniteSetModelList :: LangModelOrder A -> LangFiniteSetModel '( A, EnA) -> View (LangListModel '( TopType, A))
getFiniteSetModelList order val =
    pinaforeUpdateOrder order $ \(model :: Model update) uorder -> do
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

setentity :: LangWholeModel '( A, TopType) -> A -> PinaforeAction ()
setentity model val = langWholeModelSet model $ Known val

deleteentity :: LangWholeModel '( BottomType, TopType) -> PinaforeAction ()
deleteentity model = langWholeModelSet model Unknown

newMemWholeModel :: forall a. PinaforeAction (LangWholeModel '( a, a))
newMemWholeModel = do
    r <- liftIO $ makeMemoryReference Unknown $ \_ -> True
    model <- actionLiftLifecycle $ makeReflectingModel r
    uh <- pinaforeUndoHandler
    return $ pinaforeModelToWholeModel $ MkWModel $ undoHandlerModel uh model

newMemFiniteSetModel :: PinaforeAction (LangFiniteSetModel '( MeetType Entity A, A))
newMemFiniteSetModel = do
    r <- liftIO $ makeMemoryReference mempty $ \_ -> True
    model <- actionLiftLifecycle $ makeReflectingModel $ convertReference r
    uh <- pinaforeUndoHandler
    return $ meetValueLangFiniteSetModel $ MkWModel $ undoHandlerModel uh model

newMemListModel :: forall a. PinaforeAction (LangListModel '( a, a))
newMemListModel = do
    r <- liftIO $ makeMemoryReference mempty $ \_ -> True
    model :: Model (ListUpdate (WholeUpdate a)) <- actionLiftLifecycle $ makeReflectingModel $ convertReference r
    uh <- pinaforeUndoHandler
    return $ FullLangListModel $ eaMap singleBiChangeLens $ MkWModel $ undoHandlerModel uh model

newMemTextModel :: PinaforeAction LangTextModel
newMemTextModel = do
    r <- liftIO $ makeMemoryReference mempty $ \_ -> True
    model :: Model (StringUpdate Text) <- actionLiftLifecycle $ makeReflectingModel $ convertReference r
    uh <- pinaforeUndoHandler
    return $ MkLangTextModel $ MkWModel $ undoHandlerModel uh model

modelLibEntries :: [DocTreeEntry BindDoc]
modelLibEntries =
    [ docTreeEntry
          "Models"
          "Models keep track of updates, and will update user interfaces constructed from them when their value changes."
          [ docTreeEntry
                "Models"
                ""
                [ mkTypeEntry "Model" "" $ MkSomeGroundType modelGroundType
                , mkValEntry "onModelUpdate" "Do this on every update, during this lifecycle." langModelSubscribe
                ]
          , docTreeEntry
                "Whole Models"
                ""
                [ mkTypeEntry
                      "WholeModel"
                      "A whole model of type `WholeModel {-p,+q}` has a setting type of `p` and a getting type of `q`." $
                  MkSomeGroundType wholeModelGroundType
                , hasSubtypeRelationEntry Verify "" $
                  functionToShim "WholeModel to Model" $ langWholeModelToModel @BottomType @TopType
                , mkValEntry
                      "pureWholeModel"
                      "A constant whole model for a value."
                      (pure :: A -> PinaforeImmutableWholeModel A)
                , mkValEntry
                      "immutWholeModel"
                      "Convert a whole model to immutable.\n`immutWholeModel r = {%r}`"
                      (id :: PinaforeImmutableWholeModel A -> PinaforeImmutableWholeModel A)
                , mkValEntry
                      "coMapWholeModel"
                      "Map a function on getting a whole model."
                      (coRangeLift :: (A -> B) -> LangWholeModel '( C, A) -> LangWholeModel '( C, B))
                , mkValEntry
                      "contraMapWholeModel"
                      "Map a function on setting a whole model."
                      (contraRangeLift :: (B -> A) -> LangWholeModel '( A, C) -> LangWholeModel '( B, C))
                , mkValEntry "maybeLensMapWholeModel" "Map getter & pushback functions on a whole model." $
                  maybeLensLangWholeModel @AP @AQ @BP @BQ
                , mkValEntry "lensMapWholeModel" "Map getter & pushback functions on a whole model." $
                  fLensLangWholeModel @AP @AQ @B
                , mkValEntry "toMaybeWholeModel" "Map known/unknown to `Maybe` for a whole model." $
                  langToMaybeWholeModel @A @B
                , mkValEntry "fromMaybeWholeModel" "Map `Maybe` to known/unknown for a whole model." $
                  langFromMaybeWholeModel @A @B
                , mkValEntry "forWholeModel" "Traverse a list to make a model of a list." $
                  (for :: [A] -> (A -> PinaforeImmutableWholeModel B) -> PinaforeImmutableWholeModel [B])
                , mkValEntry "pairWholeModel" "Combine whole models." $ langPairWholeModels @AP @AQ @BP @BQ
                , mkValEntry
                      "applyWholeModel"
                      "Combine getting of whole models.\n`applyWholeModel f x` = `{%f %x}`"
                      ((<*>) :: PinaforeImmutableWholeModel (A -> B) -> PinaforeImmutableWholeModel A -> PinaforeImmutableWholeModel B)
                , mkValEntry
                      "unknown"
                      "The unknown whole model, representing missing information."
                      (empty :: PinaforeImmutableWholeModel BottomType)
                , mkValEntry "known" "True if the whole model is known." $ \(val :: PinaforeROWModel (Know TopType)) ->
                      (eaMapReadOnlyWhole (Known . isKnown) val :: PinaforeROWModel (Know Bool))
                , mkValEntry
                      "??"
                      "`p ?? q` = `p` if it is known, else `q`."
                      ((<|>) :: PinaforeImmutableWholeModel A -> PinaforeImmutableWholeModel A -> PinaforeImmutableWholeModel A)
                , mkValEntry "get" "Get a whole model, or `stop` if the whole model is unknown." $
                  langWholeModelGet @BottomType @A
                , mkValEntry ":=" "Set a whole model to a value. Stop if failed." setentity
                , mkValEntry "delete" "Delete a whole model (i.e., make unknown). Stop if failed." deleteentity
                , mkValEntry "onWholeModelUpdate" "Do an action initially and on every update, during this lifecycle." $
                  langWholeModelSubscribe @A
                , mkValEntry "newMemWholeModel" "Create a new whole model of memory, initially unknown." $
                  newMemWholeModel @A
                ]
          , docTreeEntry
                "Set Models"
                ""
                [ mkTypeEntry "SetModel" "" $ MkSomeGroundType setModelGroundType
                , hasSubtypeRelationEntry Verify "" $
                  functionToShim "SetModel to Model" $ langSetModelToModel @BottomType
                , mkValEntry
                      "mapSetModel"
                      "Map a function on a set."
                      (contramap :: (A -> B) -> LangSetModel B -> LangSetModel A)
                , mkValEntry "pureSetModel" "Convert a predicate to a set." $ predicateToLangSetModel @A
                , mkValEntry "predicateSetModel" "Convert a predicate model to a set." $ predicateModelToLangSetModel @A
                , mkValEntry "immutSetModel" "Convert a set to immutable." $ langSetModelImmutable @A
                , mkValEntry "+=" "Add an entity to a set." $ langSetModelAdd @A
                , mkValEntry "-=" "Remove an entity from a set." $ langSetModelRemove @A
                , mkValEntry "member" "A model of the membership of a value in a set." $ langSetModelMember @A
                , mkValEntry
                      "notSetModel"
                      "Complement of a set. The resulting set can be added to (deleting from the original set) and deleted from (adding to the original set)." $
                  langSetModelComplement @A
                , mkValEntry
                      "<&>"
                      "Intersection of sets. The resulting set can be added to (adding to both sets), but not deleted from." $
                  langSetModelIntersect @A
                , mkValEntry
                      "<|>"
                      "Union of sets. The resulting set can be deleted from (deleting from both sets), but not added to." $
                  langSetModelUnion @A
                , mkValEntry
                      "<\\>"
                      "Difference of sets, everything in the first set but not the second. The resulting set can be added to (adding to the first and deleting from the second), but not deleted from." $
                  langSetModelDifference @A
                , mkValEntry
                      "<^>"
                      "Symmetric difference of sets, everything in exactly one of the sets. The resulting set will be read-only." $
                  langSetModelSymmetricDifference @A
                , mkValEntry "<+>" "Cartesian sum of sets." $ langSetModelCartesianSum @A @B
                , mkValEntry "<*>" "Cartesian product of sets. The resulting set will be read-only." $
                  langSetModelCartesianProduct @A @B
                ]
          , docTreeEntry
                "Finite Set Models"
                ""
                [ mkTypeEntry "FiniteSetModel" "" $ MkSomeGroundType finiteSetModelGroundType
                , hasSubtypeRelationEntry Verify "" $
                  functionToShim "FiniteSetModel to SetModel" $ langFiniteSetModelToSetModel @A @TopType
                , mkValEntry
                      "coMapFiniteSetModel"
                      "Map a function on getting from a finite set."
                      (coRangeLift :: (A -> B) -> LangFiniteSetModel '( C, A) -> LangFiniteSetModel '( C, B))
                , mkValEntry
                      "contraMapFiniteSetModel"
                      "Map a function on setting to and testing a finite set."
                      (contraRangeLift :: (B -> A) -> LangFiniteSetModel '( A, C) -> LangFiniteSetModel '( B, C))
                , mkValEntry "maybeMapFiniteSetModel" "Map and filter a function on a finite set." $
                  langFiniteSetMaybeMap @AP @AQ @B
                , mkValEntry "<:&>" "Intersect a finite set with any set. The resulting finite set will be read-only." $
                  langFiniteSetModelSetIntersect @A @B
                , mkValEntry
                      "<:\\>"
                      "Difference of a finite set and any set. The resulting finite set will be read-only." $
                  langFiniteSetModelSetDifference @A @B
                , mkValEntry
                      "<:&:>"
                      "Intersection of finite sets. The resulting finite set can be added to, but not deleted from." $
                  langFiniteSetModelMeet @A
                , mkValEntry
                      "<:|:>"
                      "Union of finite sets. The resulting finite set can be deleted from, but not added to." $
                  langFiniteSetModelJoin @A
                , mkValEntry "<:+:>" "Cartesian sum of finite sets." $ langFiniteSetModelCartesianSum @AP @AQ @BP @BQ
                , mkSupertypeEntry "<:+:>" "Cartesian sum of finite sets." $ langFiniteSetModelCartesianSum @A @A @B @B
                , mkValEntry "<:*:>" "Cartesian product of finite sets. The resulting finite set will be read-only." $
                  langFiniteSetModelCartesianProduct @AP @AQ @BP @BQ
                , mkSupertypeEntry
                      "<:*:>"
                      "Cartesian product of finite sets. The resulting finite set will be read-only." $
                  langFiniteSetModelCartesianProduct @A @A @B @B
                , mkValEntry "finiteSetModelList" "All members of a finite set, by an order." $ pinaforeSetGetOrdered @A
                , mkValEntry
                      "getFiniteSetModelList"
                      "Get all members of a finite set, by an order. \
                    \The resulting `ListModel` will behave more \"list-like\" than `finiteSetModelList`."
                      getFiniteSetModelList
                , mkValEntry
                      "listFiniteSetModel"
                      "Represent a model of a list as a finite set. Changing the set may scramble the order of the list." $
                  langListModelToFiniteSetModel @A
                , mkValEntry "finiteSetModelSingle" "The member of a single-member finite set, or unknown." $
                  langFiniteSetModelSingle @A
                , mkValEntry "finiteSetModelCount" "Count of members in a finite set." $
                  langFiniteSetModelFunc @TopType @Int olength
                , mkValEntry
                      "finiteSetModelClear"
                      "Remove all entities from a finite set."
                      (langFiniteSetModelRemoveAll :: LangFiniteSetModel '( BottomType, TopType) -> PinaforeAction ())
                , mkValEntry
                      "newMemFiniteSetModel"
                      "Create a new finite set model of memory, initially empty."
                      newMemFiniteSetModel
                ]
          , docTreeEntry
                "List Models"
                ""
                [ mkTypeEntry "ListModel" "" $ MkSomeGroundType listModelGroundType
                , hasSubtypeRelationEntry TrustMe "" $
                  functionToShim "langListModelToModel" $ langListModelToModel @BottomType @TopType
                , hasSubtypeRelationEntry @(LangListModel '( A, A)) @(LangWholeModel '( Vector A, Vector A)) Verify "" $
                  functionToShim "langListModelToWholeModel" langListModelToWholeModel
                , mkValEntry "wholeModelList" "Represent a whole model as a list model." $ langWholeModelToListModel @A
                , mkValEntry "listModelGetCount" "Get Count of elements in a list model." langListModelGetCount
                , mkValEntry "listModelGetItem" "Get an element of a list model." $ langListModelGetItem @Q
                , mkValEntry "listModelInsert" "Insert an element in a list model." $ langListModelInsert @P
                , mkValEntry "listModelSet" "Set an element of a list model." $ langListModelSet @P
                , mkValEntry "listModelDelete" "Delete an element of a list model." langListModelDelete
                , mkValEntry "listModelClear" "Delete all elements of a list model." langListModelClear
                , mkValEntry "listModelCount" "Model of a count of elements in a list model." langListModelCountModel
                , mkValEntry
                      "listModelItem"
                      "Get a whole model of a particular item in the list. It will track the item as the list changes. Pass `True` for an existing item, `False` for a point between items." $
                  langListModelItem @P @Q
                , mkValEntry "immutListModel" "Convert a list model to immutable." $ langImmutListModel @A
                , mkValEntry "newMemListModel" "Create a new list model of memory, initially empty." $
                  newMemListModel @A
                ]
          , docTreeEntry
                "Text Models"
                ""
                [ mkTypeEntry "TextModel" "" $ MkSomeGroundType textModelGroundType
                , hasSubtypeRelationEntry @LangTextModel @(LangWholeModel '( Text, Text)) Verify "" $
                  functionToShim "langTextModelToWholeModel" langTextModelToWholeModel
                , mkValEntry "wholeModelText" "Represent a whole model as a text model." langWholeModelToTextModel
                , mkValEntry "textModelGetLength" "Get the length of text." langTextModelGetLength
                , mkValEntry "textModelGet" "Get the whole text." langTextModelGet
                , mkValEntry "textModelSet" "Set the whole text." langTextModelSet
                , mkValEntry "textModelGetSection" "Get a (start,length) section of the text." langTextModelGetSection
                , mkValEntry "textModelSetSection" "Set a (start,length) section of the text." langTextModelSetSection
                , mkValEntry
                      "textModelSection"
                      "Create a model of a (start,length) section of a text model. It will track the section as the text changes."
                      langTextModelSection
                , mkValEntry "newMemTextModel" "Create a new text model of memory, initially empty." newMemTextModel
                ]
          ]
    , docTreeEntry
          "Morphisms"
          "Morphisms relate entities."
          [ mkTypeEntry "~>" "" $ MkSomeGroundType morphismGroundType
          , mkValEntry "identity" "The identity morphism." $ identityLangMorphism @X @Y
          , mkValEntry "!." "Compose morphisms." $ composeLangMorphism @AP @AQ @BX @BY @CP @CQ
          , mkSupertypeEntry "!." "Compose morphisms." $ composeLangMorphism @A @A @B @B @C @C
          , mkValEntry "!**" "Pair morphisms. Models from these morphisms are undeleteable." $
            pairLangMorphism @AP @AQ @BP @BQ @CP @CQ
          , mkSupertypeEntry "!**" "Pair morphisms. Models from these morphisms are undeleteable." $
            pairLangMorphism @A @A @B @B @C @C
          , mkValEntry "!++" "Either morphisms. Models from these morphisms are undeleteable." $
            eitherLangMorphism @AP @AQ @BP @BQ @CP @CQ
          , mkSupertypeEntry "!++" "Either morphisms. Models from these morphisms are undeleteable." $
            eitherLangMorphism @A @A @B @B @C @C
          , mkValEntry "!$" "Apply a morphism to a model." $ applyLangMorphismModel @AP @AQ @BP @BQ
          , mkSupertypeEntry "!$" "Apply a morphism to a model." $ applyLangMorphismModel @A @A @B @B
          , mkValEntry "!$%" "Apply a morphism to an immutable model.\n`m !$% r = m !$ immutWholeModel r`" $
            applyLangMorphismImmutModel @A @BP @BQ
          , mkSupertypeEntry "!$%" "Apply a morphism to an immutable model.\n`m !$% r = m !$ immutWholeModel r`" $
            applyLangMorphismImmutModel @A @B @B
          , mkValEntry "!$$" "Apply a morphism to a set." $ applyLangMorphismSet @A @B
          , mkValEntry "!@" "Co-apply a morphism to a model." $ inverseApplyLangMorphismModel @A @BX @BY
          , mkSupertypeEntry "!@" "Co-apply a morphism to a model." $ inverseApplyLangMorphismModel @A @B @B
          , mkValEntry "!@%" "Co-apply a morphism to an immutable model.\n`m !@% r = m !@ immutWholeModel r`" $
            inverseApplyLangMorphismImmutModel @A @B
          , mkValEntry "!@@" "Co-apply a morphism to a set." $ inverseApplyLangMorphismSet @A @BX @BY
          , mkSupertypeEntry "!@@" "Co-apply a morphism to a set." $ inverseApplyLangMorphismSet @A @B @B
          , mkSpecialFormEntry
                "property"
                "A property for this anchor. `A` and `B` are types that are subtypes of `Entity`."
                ["@A", "@B", "<anchor>"]
                "A ~> B" $
            MkSpecialForm
                (ConsListType AnnotNonpolarType $ ConsListType AnnotNonpolarType $ ConsListType AnnotAnchor NilListType) $ \(MkSome ta, (MkSome tb, (anchor, ()))) -> do
                eta <- getMonoEntityType ta
                etb <- getMonoEntityType tb
                let
                    bta =
                        biRangeSomeFor
                            (nonpolarToNegative @PinaforeTypeSystem ta, nonpolarToPositive @PinaforeTypeSystem ta)
                    btb =
                        biRangeSomeFor
                            (nonpolarToNegative @PinaforeTypeSystem tb, nonpolarToPositive @PinaforeTypeSystem tb)
                    in case (bta, btb) of
                           (MkSomeFor (MkRangeType rtap rtaq) (MkRange praContra praCo), MkSomeFor (MkRangeType rtbp rtbq) (MkRange prbContra prbCo)) -> let
                               typef =
                                   typeToDolan $
                                   MkDolanGroundedType morphismGroundType $
                                   ConsCCRArguments (RangeCCRPolarArgument rtap rtaq) $
                                   ConsCCRArguments (RangeCCRPolarArgument rtbp rtbq) NilCCRArguments
                               morphism =
                                   propertyMorphism (monoEntityAdapter eta) (monoEntityAdapter etb) (MkPredicate anchor)
                               pinamorphism =
                                   MkLangMorphism $
                                   storageModelBased pinaforeStorageModel $
                                   cfmap4 (MkCatDual $ shimToFunction praContra) $
                                   cfmap3 (shimToFunction praCo) $
                                   cfmap2 (MkCatDual $ shimToFunction prbContra) $
                                   cfmap1 (shimToFunction prbCo) morphism
                               anyval = MkSomeOf typef pinamorphism
                               in return anyval
          ]
    , docTreeEntry
          "ModelOrders"
          ""
          [ mkTypeEntry "ModelOrder" "" $ MkSomeGroundType modelOrderGroundType
          , hasSubtypeRelationEntry Verify "" $ functionToShim "Order to ModelOrder" $ pureLangModelOrder @A
          , mkValEntry "orders" "Join `ModelOrder`s by priority." $ modelOrders @A
          , mkValEntry
                "mapOrder"
                "Map a function on a `ModelOrder`."
                (contramap :: (B -> A) -> LangModelOrder A -> LangModelOrder B)
          , mkValEntry "orderOn" "Order by a `ModelOrder` on a particular morphism." $ langModelOrderOn @B @A
          , mkValEntry "reverseOrder" "Reverse a `ModelOrder`." $ reverseLangModelOrder @A
          , mkValEntry "orderWhole" "Order two whole models." $ langModelOrderCompare @A
          ]
    ]
