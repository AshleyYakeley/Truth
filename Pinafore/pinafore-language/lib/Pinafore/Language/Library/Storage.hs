{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Storage
    ( storageLibSection
    ) where

import Import
import Pinafore.Context
import Pinafore.Language.Convert
import Pinafore.Language.Convert.Types
import Pinafore.Language.Expression
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Library.Model
import Pinafore.Language.Library.Optics
import Pinafore.Language.Library.Types
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type
import Pinafore.Language.Value

-- QStore
storeGroundType :: QGroundType '[] QStore
storeGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily QStore)|]) "Store"

instance HasQGroundType '[] QStore where
    qGroundType = storeGroundType

langStoreToModel :: QStore -> LangModel
langStoreToModel store = MkLangModel $ MkWModel $ qStoreModel store

openDefaultStore :: (?qcontext :: InvocationInfo) => View QStore
openDefaultStore = do
    model <- iiDefaultStorageModel ?qcontext
    liftIO $ mkQStore model

storeFetch :: StoreAdapter a -> QStore -> Entity -> Action a
storeFetch adapter store e =
    actionLiftViewKnow $
    viewRunResource (qStoreModel store) $ \aModel -> aModelRead aModel $ QStorageReadEntity adapter e

storageLibSection :: LibraryStuff InvocationInfo
storageLibSection =
    headingBDS "Storage" "" $
    [ typeBDS "Store" "Storage of information." (MkSomeGroundType storeGroundType) []
    , namespaceBDS
          "Store"
          [ addNameInRootBDS $
            specialFormBDS
                "property"
                "A property for this anchor. `A` and `B` are types that are subtypes of `Entity`."
                ["@A", "@B", "<anchor>"]
                "Store -> Property A B" $
            MkQSpecialForm
                (ConsListType AnnotNonpolarType $ ConsListType AnnotNonpolarType $ ConsListType AnnotAnchor NilListType) $ \(MkSome ta, (MkSome tb, (anchor, ()))) -> do
                eta <- getMonoStorableType ta
                etb <- getMonoStorableType tb
                Compose saa <- monoStoreAdapter eta
                Compose sab <- monoStoreAdapter etb
                MkShimWit rtap (MkPolarShim praContra) <- return $ nonpolarToNegative @QTypeSystem ta
                MkShimWit rtaq (MkPolarShim praCo) <- return $ nonpolarToPositive @QTypeSystem ta
                MkShimWit rtbp (MkPolarShim prbContra) <- return $ nonpolarToNegative @QTypeSystem tb
                MkShimWit rtbq (MkPolarShim prbCo) <- return $ nonpolarToPositive @QTypeSystem tb
                let
                    typem =
                        typeToDolan $
                        MkDolanGroundedType propertyGroundType $
                        ConsCCRArguments (RangeCCRPolarArgument rtap rtaq) $
                        ConsCCRArguments (RangeCCRPolarArgument rtbp rtbq) NilCCRArguments
                    typef = qFunctionPosWitness qType typem
                    propertyexpr = liftA2 (\isaa isab -> predicateProperty isaa isab (MkPredicate anchor)) saa sab
                    pinapropertyexpr =
                        fmap
                            (\property ->
                                 \qstore ->
                                     MkLangProperty $
                                     storageModelBased qstore $
                                     cfmap4 (MkCatDual $ shimToFunction praContra) $
                                     cfmap3 (shimToFunction praCo) $
                                     cfmap2 (MkCatDual $ shimToFunction prbContra) $
                                     cfmap1 (shimToFunction prbCo) property)
                            propertyexpr
                return $ MkSealedExpression typef pinapropertyexpr
          , hasSubtypeRelationBDS Verify "" $ functionToShim "Store to Model" langStoreToModel
          , specialFormBDS
                "cell"
                "Storage of a single value, of the given type, identified by the given anchor. Actually equivalent to `fn store => property @Unit @A <anchor> store !$ {()}`"
                ["@A", "<anchor>"]
                "Store -> WholeModel A" $
            MkQSpecialForm (ConsListType AnnotNonpolarType $ ConsListType AnnotAnchor NilListType) $ \(MkSome ta, (anchor, ())) -> do
                eta <- getMonoStorableType ta
                Compose saa <- monoStoreAdapter eta
                MkShimWit rtap (MkPolarShim praContra) <- return $ nonpolarToNegative @QTypeSystem ta
                MkShimWit rtaq (MkPolarShim praCo) <- return $ nonpolarToPositive @QTypeSystem ta
                let
                    typem =
                        typeToDolan $
                        MkDolanGroundedType wholeModelGroundType $
                        ConsCCRArguments (RangeCCRPolarArgument rtap rtaq) NilCCRArguments
                    stype = qFunctionPosWitness qType typem
                    propertyexpr =
                        fmap (\isaa -> predicateProperty (asLiteralStoreAdapter @()) isaa (MkPredicate anchor)) saa
                    sexpr =
                        fmap
                            (\property ->
                                 \qstore -> let
                                     lprop =
                                         MkLangProperty $
                                         storageModelBased qstore $
                                         cfmap3 (termf @(->)) $
                                         cfmap2 (MkCatDual $ shimToFunction praContra) $
                                         cfmap1 (shimToFunction praCo) property
                                     in applyLangAttributeModel (langPropertyAttribute lprop) $
                                        immutableToWholeModel $ pure ())
                            propertyexpr
                return $ MkSealedExpression stype sexpr
          , specialFormBDS
                "set"
                "Storage of a set of values, of the given type, identified by the given anchor. Actually equivalent to `fn store => property @A @Unit <anchor> store !@ {()}`"
                ["@A", "<anchor>"]
                "Store -> FiniteSetModel {-Entity,A}" $
            MkQSpecialForm (ConsListType AnnotNonpolarType $ ConsListType AnnotAnchor NilListType) $ \(MkSome (ta :: _ a), (anchor, ())) -> do
                eta <- getMonoStorableType ta
                Compose saa <- monoStoreAdapter eta
                MkShimWit rtap (MkPolarShim praContra) <- return $ nonpolarToNegative @QTypeSystem ta
                MkShimWit rtaq (MkPolarShim praCo) <- return $ nonpolarToPositive @QTypeSystem ta
                MkShimWit rtaep (MkPolarShim econv) <-
                    return $ joinMeetShimWit (qType @QPolyShim @Negative @Entity) (mkShimWit rtap)
                let
                    typem =
                        typeToDolan $
                        MkDolanGroundedType finiteSetModelGroundType $
                        ConsCCRArguments (RangeCCRPolarArgument rtaep rtaq) NilCCRArguments
                    stype = qFunctionPosWitness qType typem
                    propertyexpr :: QOpenExpression (StorageLensProperty a a () () QStorageUpdate)
                    propertyexpr =
                        fmap (\isaa -> predicateProperty isaa (asLiteralStoreAdapter @()) (MkPredicate anchor)) saa
                    sexpr =
                        liftA2
                            (\property isaa ->
                                 \qstore -> let
                                     lprop :: LangProperty '( a, MeetType Entity a) '( (), TopType)
                                     lprop =
                                         MkLangProperty $
                                         storageModelBased qstore $
                                         cfmap3 (meetf (storeAdapterConvert isaa) id) $ cfmap1 (termf @(->)) property
                                     lfsm :: LangFiniteSetModel '( MeetType Entity a, a)
                                     lfsm = inverseApplyLangPropertyImmutModel lprop $ pure ()
                                     in cfmap
                                            (MkCatRange
                                                 (shimToFunction $ iMeetPair id praContra . econv)
                                                 (shimToFunction praCo))
                                            lfsm)
                            propertyexpr
                            saa
                return $ MkSealedExpression stype sexpr
          , specialFormBDS
                "fetch"
                "Fetch the full value of an `Entity` from storage, or stop. Note values are removed from storage when no triple refers to them."
                ["@A"]
                "Store -> Entity -> Action A" $
            MkQSpecialForm (ConsListType AnnotNonpolarType NilListType) $ \(MkSome (ta :: _ a), ()) -> do
                eta <- getMonoStorableType ta
                Compose saa <- monoStoreAdapter eta
                let
                    stype :: QShimWit 'Positive (QStore -> Entity -> Action a)
                    stype =
                        qFunctionPosWitness qType $
                        qFunctionPosWitness qType $ actionShimWit $ nonpolarToPositive @QTypeSystem ta
                    sexpr :: QOpenExpression (QStore -> Entity -> Action a)
                    sexpr = fmap storeFetch saa
                return $ MkSealedExpression stype sexpr
          , valBDS
                "openDefault"
                "Open the default `Store`. Will be closed at the end of the lifecycle."
                openDefaultStore
          ]
    ]
