{-# LANGUAGE ApplicativeDo #-}

module Pinafore.Language.Library.Std.Reference
    ( refLibEntries
    ) where

import Changes.Core
import Pinafore.Base
import Pinafore.Language.DocTree
import Pinafore.Language.Library.Defs
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type
import Pinafore.Language.Value
import Pinafore.Language.Var
import Shapes

setentity :: LangWholeRef '( A, TopType) -> A -> PinaforeAction ()
setentity ref val = langWholeRefSet ref (Known val)

deleteentity :: LangWholeRef '( BottomType, TopType) -> PinaforeAction ()
deleteentity ref = langWholeRefSet ref Unknown

newMemWhole :: PinaforeAction (LangWholeRef '( A, A))
newMemWhole = do
    r <- liftIO $ makeMemoryReference Unknown $ \_ -> True
    model <- liftLifeCycle $ makeReflectingModel r
    uh <- pinaforeUndoHandler
    return $ pinaforeRefToWholeRef $ MkWModel $ undoHandlerModel uh model

newMemFiniteSet :: PinaforeAction (LangFiniteSetRef '( MeetType Entity A, A))
newMemFiniteSet = do
    r <- liftIO $ makeMemoryReference mempty $ \_ -> True
    model <- liftLifeCycle $ makeReflectingModel $ convertReference r
    uh <- pinaforeUndoHandler
    return $ meetValueLangFiniteSetRef $ MkWModel $ undoHandlerModel uh model

refLibEntries :: [DocTreeEntry BindDoc]
refLibEntries =
    [ docTreeEntry
          "References"
          "References keep track of updates, and will update user interfaces constructed from them when their value changes."
          [ docTreeEntry
                "Whole References"
                "A whole reference of type `WholeRef {-p,+q}` has a setting type of `p` and a getting type of `q`."
                [ mkValEntry
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
                      "Combine getting of whole references. `applyWhole f x` = `{%f %x}`"
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
                  langWholeRefGet @A
                , mkValEntry ":=" "Set a whole reference to a value. Stop if failed." setentity
                , mkValEntry "delete" "Delete a whole reference (i.e., make unknown). Stop if failed." deleteentity
                , mkValEntry "subscribeWhole" "Do an action initially and on every update, until closed." $
                  langWholeRefSubscribe @A
                , mkValEntry "newMemWhole" "Create a new whole reference to memory, initially unknown." newMemWhole
                ]
          , docTreeEntry
                "Set References"
                ""
                [ mkValEntry "mapSet" "Map a function on a set." (contramap :: (A -> B) -> LangSetRef B -> LangSetRef A)
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
                [ mkSubtypeRelationEntry "FiniteSetRef -a" "SetRef a" "" $
                  pure $
                  simpleSubtypeConversionEntry FiniteSetRefPinaforeGroundType SetRefPinaforeGroundType $
                  MkSubtypeConversion $ \_ (ConsDolanArguments (MkRangeType t _) NilDolanArguments) ->
                      return $
                      MkSubtypeArguments (ConsDolanArguments t NilDolanArguments) $
                      pure $ functionToShim "FiniteSetRef to SetRef" $ langFiniteSetRefToSetRef
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
                , mkValEntry "members" "Get all members of a finite set, by an order." $ pinaforeSetGetOrdered @A
                , mkValEntry
                      "listFiniteSet"
                      "Represent a reference to a list as a finite set. Changing the set may scramble the order of the list." $
                  langListRefToFiniteSetRef @A
                , mkValEntry "single" "The member of a single-member finite set, or unknown." $
                  langFiniteSetRefSingle @A
                , mkValEntry "count" "Count of members in a finite set." $ langFiniteSetRefFunc @TopType @Int olength
                , mkValEntry
                      "removeAll"
                      "Remove all entities from a finite set."
                      (langFiniteSetRefRemoveAll :: LangFiniteSetRef '( BottomType, TopType) -> PinaforeAction ())
                , mkValEntry
                      "newMemFiniteSet"
                      "Create a new finite set reference to memory, initially empty."
                      newMemFiniteSet
                ]
          ]
    , docTreeEntry
          "Morphisms"
          "Morphisms relate entities."
          [ mkValEntry "identity" "The identity morphism." $ identityLangMorphism @X @Y
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
          , mkValEntry "!$%" "Apply a morphism to an immutable reference. `m !$% r = m !$ immutWhole r`" $
            applyLangMorphismImmutRef @A @BP @BQ
          , mkSupertypeEntry "!$%" "Apply a morphism to an immutable reference. `m !$% r = m !$ immutWhole r`" $
            applyLangMorphismImmutRef @A @B @B
          , mkValEntry "!$$" "Apply a morphism to a set." $ applyLangMorphismSet @A @B
          , mkValEntry "!@" "Co-apply a morphism to a reference." $ inverseApplyLangMorphismRef @A @BX @BY
          , mkSupertypeEntry "!@" "Co-apply a morphism to a reference." $ inverseApplyLangMorphismRef @A @B @B
          , mkValEntry "!@%" "Co-apply a morphism to an immutable reference. `m !@% r = m !@ immutWhole r`" $
            inverseApplyLangMorphismImmutRef @A @B
          , mkValEntry "!@@" "Co-apply a morphism to a set." $ inverseApplyLangMorphismSet @A @BX @BY
          , mkSupertypeEntry "!@@" "Co-apply a morphism to a set." $ inverseApplyLangMorphismSet @A @B @B
          , mkSpecialFormEntry
                "property"
                "A property for this anchor. `A` and `B` are types that are subtypes of `Entity`."
                "@A @B <anchor>"
                "A ~> B" $
            MkSpecialForm
                (ConsListType AnnotMonoEntityType $
                 ConsListType AnnotMonoEntityType $ ConsListType AnnotAnchor NilListType) $ \(MkAnyW eta, (MkAnyW etb, (anchor, ()))) -> do
                etan <- monoEntityToNegativePinaforeType eta
                etbn <- monoEntityToNegativePinaforeType etb
                let
                    bta = biRangeAnyF (etan, monoToPositiveDolanType eta)
                    btb = biRangeAnyF (etbn, monoToPositiveDolanType etb)
                    in case (bta, btb, monoEntityTypeEq eta, monoEntityTypeEq etb) of
                           (MkAnyF rta (MkRange praContra praCo), MkAnyF rtb (MkRange prbContra prbCo), Dict, Dict) ->
                               withSubrepresentative rangeTypeInKind rta $
                               withSubrepresentative rangeTypeInKind rtb $ let
                                   typef =
                                       singleDolanShimWit $
                                       mkShimWit $
                                       GroundDolanSingularType MorphismPinaforeGroundType $
                                       ConsDolanArguments rta $ ConsDolanArguments rtb NilDolanArguments
                                   morphism =
                                       propertyMorphism
                                           (monoEntityAdapter eta)
                                           (monoEntityAdapter etb)
                                           (MkPredicate anchor)
                                   pinamorphism =
                                       MkLangMorphism $
                                       cfmap3 (MkCatDual $ shimToFunction praContra) $
                                       cfmap2 (shimToFunction praCo) $
                                       cfmap1 (MkCatDual $ shimToFunction prbContra) $
                                       fmap (shimToFunction prbCo) morphism
                                   anyval = MkAnyValue typef pinamorphism
                                   in return anyval
          ]
    , docTreeEntry
          "RefOrders"
          ""
          [ mkSubtypeRelationEntry "a -> a -> Ordering" "RefOrder a" "" $
            pure $
            simpleSubtypeConversionEntry FuncPinaforeGroundType RefOrderPinaforeGroundType $
            MkSubtypeConversion $ \(sc :: _ pola polb) (ConsDolanArguments t1 (ConsDolanArguments t2o NilDolanArguments)) ->
                invertPolarity @pola $
                invertPolarity @polb $ do
                    MkVarType var <- varRenamerTGenerateUVar []
                    let
                        vara :: PinaforeType (InvertPolarity pola) _
                        vara = singleDolanType $ VarDolanSingularType var
                        varb :: PinaforeType (InvertPolarity polb) _
                        varb = singleDolanType $ VarDolanSingularType var
                        vconv = iJoinMeetR1 @(InvertPolarity polb) . iJoinMeetL1 @(InvertPolarity pola)
                    return $
                        MkSubtypeArguments (ConsDolanArguments vara NilDolanArguments) $ do
                            conv1 <- subtypeConvert (subtypeInverted sc) varb t1
                            conv2 <-
                                subtypeConvert sc t2o $
                                singleDolanType $
                                GroundDolanSingularType FuncPinaforeGroundType $
                                ConsDolanArguments varb $
                                ConsDolanArguments
                                    (singleDolanType $
                                     GroundDolanSingularType
                                         (EntityPinaforeGroundType NilListType $
                                          LiteralEntityGroundType OrderingLiteralType)
                                         NilDolanArguments)
                                    NilDolanArguments
                            return $
                                (functionToShim "Order to RefOrder" pureRefOrder) .
                                applyCoPolyShim
                                    (applyContraPolyShim cid $ conv1 . vconv)
                                    (applyCoPolyShim (applyContraPolyShim cid vconv) (iJoinMeetL1 @polb) .
                                     iJoinMeetL1 @polb . conv2)
          , mkValEntry "refOrders" "Join RefOrders by priority." $ refOrders @A
          , mkValEntry
                "mapRefOrder"
                "Map a function on a RefOrder."
                (contramap :: (B -> A) -> LangRefOrder A -> LangRefOrder B)
          , mkValEntry "refOrderOn" "Order by a RefOrder on a particular morphism." $ refOrderOn @B @A
          , mkValEntry "reverseRef" "Reverse a RefOrder." $ reverseRefOrder @A
          , mkValEntry "orderWholeRef" "Order two whole references." $ langRefOrderCompare @A
          ]
    ]
