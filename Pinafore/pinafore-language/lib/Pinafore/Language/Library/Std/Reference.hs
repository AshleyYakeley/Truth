{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE ApplicativeDo #-}

module Pinafore.Language.Library.Std.Reference
    ( refLibEntries
    ) where

import Changes.Core
import Pinafore.Base
import Pinafore.Context
import Pinafore.Language.Convert
import Pinafore.Language.Convert.Types
import Pinafore.Language.DocTree
import Pinafore.Language.ExprShow
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std.Convert ()
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type
import Pinafore.Language.Value
import Pinafore.Language.Var
import Shapes

-- WModel
instance (HasPinaforeType 'Positive t, HasPinaforeType 'Negative t) =>
             HasPinaforeType 'Positive (WModel (WholeUpdate (Know t))) where
    pinaforeType = mapPosShimWit (functionToShim "pinaforeRefToWholeRef" pinaforeRefToWholeRef) pinaforeType

instance (HasPinaforeType 'Positive t, HasPinaforeType 'Negative t) =>
             HasPinaforeType 'Negative (WModel (WholeUpdate (Know t))) where
    pinaforeType = mapNegShimWit (functionToShim "langWholeRefToValue" langWholeRefToValue) pinaforeType

-- PinaforeROWRef
instance (HasPinaforeType 'Negative a) => HasPinaforeType 'Negative (PinaforeROWRef (Know a)) where
    pinaforeType = mapNegShimWit (functionToShim "langWholeRefToReadOnlyValue" langWholeRefToReadOnlyValue) pinaforeType

instance (HasPinaforeType 'Positive a) => HasPinaforeType 'Positive (PinaforeROWRef (Know a)) where
    pinaforeType = mapPosShimWit (functionToShim "pinaforeROWRefToWholeRef" pinaforeROWRefToWholeRef) pinaforeType

-- SetRef
setRefGroundType :: PinaforeGroundType '[ ContraCCRVariance] LangSetRef
setRefGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangSetRef)|]) "SetRef"

instance HasPinaforeGroundType '[ ContraCCRVariance] LangSetRef where
    pinaforeGroundType = setRefGroundType

-- FiniteSetRef
finiteSetRefGroundType :: PinaforeGroundType '[ 'RangeCCRVariance] LangFiniteSetRef
finiteSetRefGroundType =
    stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangFiniteSetRef)|]) "FiniteSetRef"

instance HasPinaforeGroundType '[ 'RangeCCRVariance] LangFiniteSetRef where
    pinaforeGroundType = finiteSetRefGroundType

-- ListRef
listRefGroundType :: PinaforeGroundType '[ 'RangeCCRVariance] LangListRef
listRefGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangListRef)|]) "ListRef"

instance HasPinaforeGroundType '[ 'RangeCCRVariance] LangListRef where
    pinaforeGroundType = listRefGroundType

-- TextRef
textRefGroundType :: PinaforeGroundType '[] LangTextRef
textRefGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangTextRef)|]) "TextRef"

instance HasPinaforeGroundType '[] LangTextRef where
    pinaforeGroundType = textRefGroundType

-- RefOrder
refOrderGroundType :: PinaforeGroundType '[ ContraCCRVariance] LangRefOrder
refOrderGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangRefOrder)|]) "RefOrder"

instance HasPinaforeGroundType '[ ContraCCRVariance] LangRefOrder where
    pinaforeGroundType = refOrderGroundType

-- LangMorphism
morphismGroundType :: PinaforeGroundType '[ 'RangeCCRVariance, 'RangeCCRVariance] LangMorphism
morphismGroundType =
    singleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangMorphism)|]) $ \ta tb ->
        (precShow 1 ta <> " ~> " <> precShow 2 tb, 2)

instance HasPinaforeGroundType '[ 'RangeCCRVariance, 'RangeCCRVariance] LangMorphism where
    pinaforeGroundType = morphismGroundType

getSetList :: LangRefOrder A -> LangFiniteSetRef '( A, EnA) -> View (LangListRef '( TopType, A))
getSetList order val =
    pinaforeUpdateOrder order $ \(model :: Model update) uorder -> do
        let
            uo :: UpdateOrder (ContextUpdate update (ConstWholeUpdate EnA))
            uo =
                mapUpdateOrder
                    (liftContextChangeLens $ fromReadOnlyRejectingChangeLens . funcChangeLens (Known . meet2))
                    uorder
            rows :: Model (FiniteSetUpdate EnA)
            rows = unWModel $ unLangFiniteSetRef $ contraRangeLift meet2 val
            pkSub :: Model (ContextUpdate update (FiniteSetUpdate EnA))
            pkSub = contextModels model rows
        colSub :: Model (ContextUpdate update (OrderedListUpdate (ConstWholeUpdate EnA))) <-
            viewFloatMapModel (contextOrderedSetLens uo) pkSub
        return $
            OrderedLangListRef $
            eaMap (liftOrderedListChangeLens (constWholeChangeLens meet2) . tupleChangeLens SelectContent) $
            MkWModel colSub

setentity :: LangWholeRef '( A, TopType) -> A -> PinaforeAction ()
setentity ref val = langWholeRefSet ref $ Known val

deleteentity :: LangWholeRef '( BottomType, TopType) -> PinaforeAction ()
deleteentity ref = langWholeRefSet ref Unknown

newMemWhole :: forall a. PinaforeAction (LangWholeRef '( a, a))
newMemWhole = do
    r <- liftIO $ makeMemoryReference Unknown $ \_ -> True
    model <- actionLiftLifecycle $ makeReflectingModel r
    uh <- pinaforeUndoHandler
    return $ pinaforeRefToWholeRef $ MkWModel $ undoHandlerModel uh model

newMemFiniteSet :: PinaforeAction (LangFiniteSetRef '( MeetType Entity A, A))
newMemFiniteSet = do
    r <- liftIO $ makeMemoryReference mempty $ \_ -> True
    model <- actionLiftLifecycle $ makeReflectingModel $ convertReference r
    uh <- pinaforeUndoHandler
    return $ meetValueLangFiniteSetRef $ MkWModel $ undoHandlerModel uh model

newMemList :: forall a. PinaforeAction (LangListRef '( a, a))
newMemList = do
    r <- liftIO $ makeMemoryReference mempty $ \_ -> True
    model :: Model (ListUpdate (WholeUpdate a)) <- actionLiftLifecycle $ makeReflectingModel $ convertReference r
    uh <- pinaforeUndoHandler
    return $ FullLangListRef $ eaMap singleBiChangeLens $ MkWModel $ undoHandlerModel uh model

newMemText :: PinaforeAction LangTextRef
newMemText = do
    r <- liftIO $ makeMemoryReference mempty $ \_ -> True
    model :: Model (StringUpdate Text) <- actionLiftLifecycle $ makeReflectingModel $ convertReference r
    uh <- pinaforeUndoHandler
    return $ MkLangTextRef $ MkWModel $ undoHandlerModel uh model

refLibEntries :: [DocTreeEntry BindDoc]
refLibEntries =
    [ docTreeEntry
          "References"
          "References keep track of updates, and will update user interfaces constructed from them when their value changes."
          [ docTreeEntry
                "Whole References"
                ""
                [ mkTypeEntry
                      "WholeRef"
                      "A whole reference of type `WholeRef {-p,+q}` has a setting type of `p` and a getting type of `q`." $
                  MkBoundType wholeRefGroundType
                , mkValEntry
                      "pureWhole"
                      "A constant whole reference for a value."
                      (pure :: A -> PinaforeImmutableWholeRef A)
                , mkValEntry
                      "immutWhole"
                      "Convert a whole reference to immutable.\n`immutWhole r = {%r}`"
                      (id :: PinaforeImmutableWholeRef A -> PinaforeImmutableWholeRef A)
                , mkValEntry
                      "coMapWhole"
                      "Map a function on getting a whole reference."
                      (coRangeLift :: (A -> B) -> LangWholeRef '( C, A) -> LangWholeRef '( C, B))
                , mkValEntry
                      "contraMapWhole"
                      "Map a function on setting a whole reference."
                      (contraRangeLift :: (B -> A) -> LangWholeRef '( A, C) -> LangWholeRef '( B, C))
                , mkValEntry "maybeLensMapWhole" "Map getter & pushback functions on a whole reference." $
                  maybeLensLangWholeRef @AP @AQ @BP @BQ
                , mkValEntry "lensMapWhole" "Map getter & pushback functions on a whole reference." $
                  fLensLangWholeRef @AP @AQ @B
                , mkValEntry "toMaybeWhole" "Map known/unknown to `Maybe` for a whole reference." $
                  langToMaybeWholeRef @A @B
                , mkValEntry "fromMaybeWhole" "Map `Maybe` to known/unknown for a whole reference." $
                  langFromMaybeWholeRef @A @B
                , mkValEntry "forWhole" "Traverse a list to make a reference to a list." $
                  (for :: [A] -> (A -> PinaforeImmutableWholeRef B) -> PinaforeImmutableWholeRef [B])
                , mkValEntry "pairWhole" "Combine whole references." $ langPairWholeRefs @AP @AQ @BP @BQ
                , mkValEntry
                      "applyWhole"
                      "Combine getting of whole references.\n`applyWhole f x` = `{%f %x}`"
                      ((<*>) :: PinaforeImmutableWholeRef (A -> B) -> PinaforeImmutableWholeRef A -> PinaforeImmutableWholeRef B)
                , mkValEntry
                      "unknown"
                      "The unknown whole reference, representing missing information."
                      (empty :: PinaforeImmutableWholeRef BottomType)
                , mkValEntry "known" "True if the whole reference is known." $ \(val :: PinaforeROWRef (Know TopType)) ->
                      (eaMapReadOnlyWhole (Known . isKnown) val :: PinaforeROWRef (Know Bool))
                , mkValEntry
                      "??"
                      "`p ?? q` = `p` if it is known, else `q`."
                      ((<|>) :: PinaforeImmutableWholeRef A -> PinaforeImmutableWholeRef A -> PinaforeImmutableWholeRef A)
                , mkValEntry "get" "Get a whole reference, or `stop` if the whole reference is unknown." $
                  langWholeRefGet @BottomType @A
                , mkValEntry ":=" "Set a whole reference to a value. Stop if failed." setentity
                , mkValEntry "delete" "Delete a whole reference (i.e., make unknown). Stop if failed." deleteentity
                , mkValEntry "subscribeWhole" "Do an action initially and on every update, until closed." $
                  langWholeRefSubscribe @A
                , mkValEntry "newMemWhole" "Create a new whole reference to memory, initially unknown." $ newMemWhole @A
                ]
          , docTreeEntry
                "Set References"
                ""
                [ mkTypeEntry "SetRef" "" $ MkBoundType setRefGroundType
                , mkValEntry "mapSet" "Map a function on a set." (contramap :: (A -> B) -> LangSetRef B -> LangSetRef A)
                , mkValEntry "pureSet" "Convert a predicate to a set." $ predicateToLangSetRef @A
                , mkValEntry "refSet" "Convert a predicate reference to a set." $ predicateRefToLangSetRef @A
                , mkValEntry "immutSet" "Convert a set to immutable." $ langSetRefImmutable @A
                , mkValEntry "+=" "Add an entity to a set." $ langSetRefAdd @A
                , mkValEntry "-=" "Remove an entity from a set." $ langSetRefRemove @A
                , mkValEntry "member" "A reference to the membership of a value in a set." $ langSetRefMember @A
                , mkValEntry
                      "notSet"
                      "Complement of a set. The resulting set can be added to (deleting from the original set) and deleted from (adding to the original set)." $
                  langSetRefComplement @A
                , mkValEntry
                      "<&>"
                      "Intersection of sets. The resulting set can be added to (adding to both sets), but not deleted from." $
                  langSetRefIntersect @A
                , mkValEntry
                      "<|>"
                      "Union of sets. The resulting set can be deleted from (deleting from both sets), but not added to." $
                  langSetRefUnion @A
                , mkValEntry
                      "<\\>"
                      "Difference of sets, everything in the first set but not the second. The resulting set can be added to (adding to the first and deleting from the second), but not deleted from." $
                  langSetRefDifference @A
                , mkValEntry
                      "<^>"
                      "Symmetric difference of sets, everything in exactly one of the sets. The resulting set will be read-only." $
                  langSetRefSymmetricDifference @A
                , mkValEntry "<+>" "Cartesian sum of sets." $ langSetRefCartesianSum @A @B
                , mkValEntry "<*>" "Cartesian product of sets. The resulting set will be read-only." $
                  langSetRefCartesianProduct @A @B
                ]
          , docTreeEntry
                "Finite Set References"
                ""
                [ mkTypeEntry "FiniteSetRef" "" $ MkBoundType finiteSetRefGroundType
                , hasSubtypeRelationEntry Verify "" $
                  functionToShim "FiniteSetRef to SetRef" $ langFiniteSetRefToSetRef @A @TopType
                , mkValEntry
                      "coMapFiniteSet"
                      "Map a function on getting from a finite set."
                      (coRangeLift :: (A -> B) -> LangFiniteSetRef '( C, A) -> LangFiniteSetRef '( C, B))
                , mkValEntry
                      "contraMapFiniteSet"
                      "Map a function on setting to and testing a finite set."
                      (contraRangeLift :: (B -> A) -> LangFiniteSetRef '( A, C) -> LangFiniteSetRef '( B, C))
                , mkValEntry "maybeMapFiniteSet" "Map and filter a function on a finite set." $
                  langFiniteSetMaybeMap @AP @AQ @B
                , mkValEntry "<:&>" "Intersect a finite set with any set. The resulting finite set will be read-only." $
                  langFiniteSetRefSetIntersect @A @B
                , mkValEntry
                      "<:\\>"
                      "Difference of a finite set and any set. The resulting finite set will be read-only." $
                  langFiniteSetRefSetDifference @A @B
                , mkValEntry
                      "<:&:>"
                      "Intersection of finite sets. The resulting finite set can be added to, but not deleted from." $
                  langFiniteSetRefMeet @A
                , mkValEntry
                      "<:|:>"
                      "Union of finite sets. The resulting finite set can be deleted from, but not added to." $
                  langFiniteSetRefJoin @A
                , mkValEntry "<:+:>" "Cartesian sum of finite sets." $ langFiniteSetRefCartesianSum @AP @AQ @BP @BQ
                , mkSupertypeEntry "<:+:>" "Cartesian sum of finite sets." $ langFiniteSetRefCartesianSum @A @A @B @B
                , mkValEntry "<:*:>" "Cartesian product of finite sets. The resulting finite set will be read-only." $
                  langFiniteSetRefCartesianProduct @AP @AQ @BP @BQ
                , mkSupertypeEntry
                      "<:*:>"
                      "Cartesian product of finite sets. The resulting finite set will be read-only." $
                  langFiniteSetRefCartesianProduct @A @A @B @B
                , mkValEntry "setList" "All members of a finite set, by an order." $ pinaforeSetGetOrdered @A
                , mkValEntry
                      "getSetList"
                      "Get all members of a finite set, by an order. \
                    \The resulting `ListRef` will behave more \"list-like\" than `setList`."
                      getSetList
                , mkValEntry
                      "listSet"
                      "Represent a reference to a list as a finite set. Changing the set may scramble the order of the list." $
                  langListRefToFiniteSetRef @A
                , mkValEntry "setSingle" "The member of a single-member finite set, or unknown." $
                  langFiniteSetRefSingle @A
                , mkValEntry "setCount" "Count of members in a finite set." $ langFiniteSetRefFunc @TopType @Int olength
                , mkValEntry
                      "setClear"
                      "Remove all entities from a finite set."
                      (langFiniteSetRefRemoveAll :: LangFiniteSetRef '( BottomType, TopType) -> PinaforeAction ())
                , mkValEntry
                      "newMemFiniteSet"
                      "Create a new finite set reference to memory, initially empty."
                      newMemFiniteSet
                ]
          , docTreeEntry
                "List References"
                ""
                [ mkTypeEntry "ListRef" "" $ MkBoundType listRefGroundType
                , hasSubtypeRelationEntry @(LangWholeRef '( Vector A, Vector A)) @(LangListRef '( A, A)) Verify "" $
                  functionToShim "langWholeRefToListRef" langWholeRefToListRef
                , mkValEntry "listWhole" "Represent a list reference as a whole reference." $ langListRefToWholeRef @A
                , mkValEntry "listGetCount" "Get Count of elements in a list reference." langListRefGetCount
                , mkValEntry "listGetItem" "Get an element of a list reference." $ langListRefGetItem @Q
                , mkValEntry "listInsert" "Insert an element in a list reference." $ langListRefInsert @P
                , mkValEntry "listSet" "Set an element of a list reference." $ langListRefSet @P
                , mkValEntry "listDelete" "Delete an element of a list reference." langListRefDelete
                , mkValEntry "listClear" "Delete all elements of a list reference." langListRefClear
                , mkValEntry "listCountRef" "Reference to a count of elements in a list reference." langListRefCountRef
                , mkValEntry
                      "listGetItemRef"
                      "Get a whole reference to a particular item in the list. It will track the item as the list changes. Pass `True` for an existing item, `False` for a point between items." $
                  langListRefItem @P @Q
                , mkValEntry "newMemList" "Create a new list reference to memory, initially empty." $ newMemList @A
                ]
          , docTreeEntry
                "Text References"
                ""
                [ mkTypeEntry "TextRef" "" $ MkBoundType textRefGroundType
                , hasSubtypeRelationEntry @LangTextRef @(LangWholeRef '( Text, Text)) Verify "" $
                  functionToShim "langTextRefToWholeRef" langTextRefToWholeRef
                , mkValEntry "wholeRefText" "Represent a whole reference as a text reference." langWholeRefToTextRef
                , mkValEntry "textRefGetLength" "Get the length of text." langTextRefGetLength
                , mkValEntry "textRefGet" "Get the whole text." langTextRefGet
                , mkValEntry "textRefSet" "Set the whole text." langTextRefSet
                , mkValEntry "textRefGetSection" "Get a (start,length) section of the text." langTextRefGetSection
                , mkValEntry "textRefSetSection" "Set a (start,length) section of the text." langTextRefSetSection
                , mkValEntry
                      "textRefSection"
                      "Create a reference to a (start,length) section of a text reference. It will track the section as the text changes."
                      langTextRefSection
                , mkValEntry "newMemText" "Create a new text reference to memory, initially empty." newMemText
                ]
          ]
    , docTreeEntry
          "Morphisms"
          "Morphisms relate entities."
          [ mkTypeEntry "~>" "" $ MkBoundType morphismGroundType
          , mkValEntry "identity" "The identity morphism." $ identityLangMorphism @X @Y
          , mkValEntry "!." "Compose morphisms." $ composeLangMorphism @AP @AQ @BX @BY @CP @CQ
          , mkSupertypeEntry "!." "Compose morphisms." $ composeLangMorphism @A @A @B @B @C @C
          , mkValEntry "!**" "Pair morphisms. References from these morphisms are undeleteable." $
            pairLangMorphism @AP @AQ @BP @BQ @CP @CQ
          , mkSupertypeEntry "!**" "Pair morphisms. References from these morphisms are undeleteable." $
            pairLangMorphism @A @A @B @B @C @C
          , mkValEntry "!++" "Either morphisms. References from these morphisms are undeleteable." $
            eitherLangMorphism @AP @AQ @BP @BQ @CP @CQ
          , mkSupertypeEntry "!++" "Either morphisms. References from these morphisms are undeleteable." $
            eitherLangMorphism @A @A @B @B @C @C
          , mkValEntry "!$" "Apply a morphism to a reference." $ applyLangMorphismRef @AP @AQ @BP @BQ
          , mkSupertypeEntry "!$" "Apply a morphism to a reference." $ applyLangMorphismRef @A @A @B @B
          , mkValEntry "!$%" "Apply a morphism to an immutable reference.\n`m !$% r = m !$ immutWhole r`" $
            applyLangMorphismImmutRef @A @BP @BQ
          , mkSupertypeEntry "!$%" "Apply a morphism to an immutable reference.\n`m !$% r = m !$ immutWhole r`" $
            applyLangMorphismImmutRef @A @B @B
          , mkValEntry "!$$" "Apply a morphism to a set." $ applyLangMorphismSet @A @B
          , mkValEntry "!@" "Co-apply a morphism to a reference." $ inverseApplyLangMorphismRef @A @BX @BY
          , mkSupertypeEntry "!@" "Co-apply a morphism to a reference." $ inverseApplyLangMorphismRef @A @B @B
          , mkValEntry "!@%" "Co-apply a morphism to an immutable reference.\n`m !@% r = m !@ immutWhole r`" $
            inverseApplyLangMorphismImmutRef @A @B
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
          "RefOrders"
          ""
          [ mkTypeEntry "RefOrder" "" $ MkBoundType refOrderGroundType
          , hasSubtypeRelationEntry Verify "" $ functionToShim "Order to RefOrder" $ pureLangRefOrder @A
          , mkValEntry "orders" "Join `RefOrder`s by priority." $ refOrders @A
          , mkValEntry
                "mapOrder"
                "Map a function on a `RefOrder`."
                (contramap :: (B -> A) -> LangRefOrder A -> LangRefOrder B)
          , mkValEntry "orderOn" "Order by a `RefOrder` on a particular morphism." $ langRefOrderOn @B @A
          , mkValEntry "reverseOrder" "Reverse a `RefOrder`." $ reverseLangRefOrder @A
          , mkValEntry "orderWhole" "Order two whole references." $ langRefOrderCompare @A
          ]
    ]
