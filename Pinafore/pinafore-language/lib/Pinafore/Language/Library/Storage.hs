{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Storage
    ( storageLibSection
    )
where

import Pinafore.Storage
import System.FilePath

import Import
import Pinafore.Context
import Pinafore.Language.Convert
import Pinafore.Language.Convert.Types
import Pinafore.Language.Expression
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Library.Model
import Pinafore.Language.Library.Optics
import Pinafore.Language.Library.Types
import Pinafore.Language.Type
import Pinafore.Language.Value

-- QStore
storeGroundType :: QGroundType '[] QStore
storeGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily QStore)|]) "Store"

instance HasQGroundType '[] QStore where
    qGroundType = storeGroundType

langStoreToModel :: QStore -> LangModel
langStoreToModel store = MkLangModel $ MkWModel $ qStoreModel store

openLocalStore :: (Maybe Text, (Bool, ())) -> View QStore
openLocalStore (mDataDir, (cache, ())) = do
    rc <- viewGetResourceContext
    viewLiftLifecycle $ do
        sqlReference <-
            liftIO $ do
                dataDir <- ensurePinaforeDir $ fmap unpack mDataDir
                sqliteTableReference $ dataDir </> "tables.sqlite3"
        tableReference1 <- exclusiveResource rc sqlReference
        tableReference <-
            if cache
                then do
                    tableReferenceF <- cacheReference rc 500000 tableReference1 -- half-second delay before writing
                    return $ tableReferenceF rc
                else return tableReference1
        (model, ()) <- makeSharedModel $ reflectingPremodel $ qTableEntityReference tableReference
        liftIO $ mkQStore model

openTempStore :: View QStore
openTempStore = do
    tableStateReference :: Reference (WholeEdit QTableSubject) <-
        liftIO $ makeMemoryReference (MkQTableSubject [] [] [] []) $ \_ -> True
    let
        tableReference :: Reference QTableEdit
        tableReference = convertReference tableStateReference
    (model, ()) <- viewLiftLifecycle $ makeSharedModel $ reflectingPremodel $ qTableEntityReference tableReference
    liftIO $ mkQStore model

storeFetch :: StoreAdapter a -> QStore -> Entity -> Action a
storeFetch adapter store e =
    actionLiftViewKnow
        $ viewRunResource (qStoreModel store)
        $ \aModel -> aModelRead aModel $ QStorageReadEntity adapter e

storageLibSection :: LibraryStuff
storageLibSection =
    headingBDS "Storage" ""
        $ [ typeBDS "Store" "Storage of information." (MkSomeGroundType storeGroundType) []
          , namespaceBDS
                "Store"
                [ addNameInRootBDS
                    $ valBDS
                        "property"
                        "`!{property @A @B <anchor>}: Store -> Property A B`  \nA property for this anchor. `A` and `B` are types that are subtypes of `Entity`."
                    $ \(MkLangType ta) (MkLangType tb) anchor -> do
                        eta <- getMonoStorableType ta
                        etb <- getMonoStorableType tb
                        saaexpr <- monoStoreAdapter eta
                        sabexpr <- monoStoreAdapter etb
                        MkShimWit rtap (MkPolarShim praContra) <- return $ nonpolarToNegative @QTypeSystem ta
                        MkShimWit rtaq (MkPolarShim praCo) <- return $ nonpolarToPositive @QTypeSystem ta
                        MkShimWit rtbp (MkPolarShim prbContra) <- return $ nonpolarToNegative @QTypeSystem tb
                        MkShimWit rtbq (MkPolarShim prbCo) <- return $ nonpolarToPositive @QTypeSystem tb
                        let
                            typem =
                                typeToDolan
                                    $ MkDolanGroundedType propertyGroundType
                                    $ ConsCCRArguments (RangeCCRPolarArgument rtap rtaq)
                                    $ ConsCCRArguments (RangeCCRPolarArgument rtbp rtbq) NilCCRArguments
                            typef = qFunctionPosWitness qType typem
                            propertyexpr = liftA2 (\saa sab -> predicateProperty saa sab (MkPredicate anchor)) saaexpr sabexpr
                            pinapropertyexpr =
                                fmap
                                    ( \property ->
                                        \qstore ->
                                            MkLangProperty
                                                $ storageModelBased qstore
                                                $ cfmap4 (MkCatDual $ shimToFunction praContra)
                                                $ cfmap3 (shimToFunction praCo)
                                                $ cfmap2 (MkCatDual $ shimToFunction prbContra)
                                                $ cfmap1 (shimToFunction prbCo) property
                                    )
                                    propertyexpr
                        return $ MkLangExpression $ MkSealedExpression typef pinapropertyexpr
                , hasSubtypeRelationBDS Verify "" $ functionToShim "Store to Model" langStoreToModel
                , valBDS
                    "cell"
                    "`!{cell @A}: Store -> WholeModel A`  \nStorage of a single value, of the given type, identified by the given anchor. Actually equivalent to `fn store => !{property @Unit @A <anchor>} store !$ {()}`"
                    $ \(MkLangType ta) anchor -> do
                        eta <- getMonoStorableType ta
                        saaexpr <- monoStoreAdapter eta
                        MkShimWit rtap (MkPolarShim praContra) <- return $ nonpolarToNegative @QTypeSystem ta
                        MkShimWit rtaq (MkPolarShim praCo) <- return $ nonpolarToPositive @QTypeSystem ta
                        let
                            typem =
                                typeToDolan
                                    $ MkDolanGroundedType wholeModelGroundType
                                    $ ConsCCRArguments (RangeCCRPolarArgument rtap rtaq) NilCCRArguments
                            stype = qFunctionPosWitness qType typem
                            propertyexpr =
                                fmap (\saa -> predicateProperty (asLiteralStoreAdapter @()) saa (MkPredicate anchor)) saaexpr
                            sexpr =
                                fmap
                                    ( \property ->
                                        \qstore -> let
                                            lprop =
                                                MkLangProperty
                                                    $ storageModelBased qstore
                                                    $ cfmap3 (termf @(->))
                                                    $ cfmap2 (MkCatDual $ shimToFunction praContra)
                                                    $ cfmap1 (shimToFunction praCo) property
                                            in applyLangAttributeModel (langPropertyAttribute lprop)
                                                $ immutableToWholeModel
                                                $ pure ()
                                    )
                                    propertyexpr
                        return $ MkLangExpression $ MkSealedExpression stype sexpr
                , valBDS
                    "set"
                    "`!{set @A <anchor>}: Store -> FiniteSetModel {-Entity,A}`  \nStorage of a set of values, of the given type, identified by the given anchor. Actually equivalent to `fn store => !{property @A @Unit <anchor>} store !@ {()}`"
                    $ \(MkLangType (ta :: _ a)) anchor -> do
                        eta <- getMonoStorableType ta
                        saaexpr <- monoStoreAdapter eta
                        MkShimWit rtap (MkPolarShim praContra) <- return $ nonpolarToNegative @QTypeSystem ta
                        MkShimWit rtaq (MkPolarShim praCo) <- return $ nonpolarToPositive @QTypeSystem ta
                        MkShimWit rtaep (MkPolarShim econv) <-
                            return $ joinMeetShimWit (qType @QPolyShim @'Negative @Entity) (mkShimWit rtap)
                        let
                            typem =
                                typeToDolan
                                    $ MkDolanGroundedType finiteSetModelGroundType
                                    $ ConsCCRArguments (RangeCCRPolarArgument rtaep rtaq) NilCCRArguments
                            stype = qFunctionPosWitness qType typem
                            propertyexpr :: QOpenExpression (StorageLensProperty a a () () QStorageUpdate)
                            propertyexpr =
                                fmap (\saa -> predicateProperty saa (asLiteralStoreAdapter @()) (MkPredicate anchor)) saaexpr
                            sexpr =
                                liftA2
                                    ( \property saa ->
                                        \qstore -> let
                                            lprop :: LangProperty '(a, MeetType Entity a) '((), TopType)
                                            lprop =
                                                MkLangProperty
                                                    $ storageModelBased qstore
                                                    $ cfmap3 (meetf (storeAdapterConvert saa) id)
                                                    $ cfmap1 (termf @(->)) property
                                            lfsm :: LangFiniteSetModel '(MeetType Entity a, a)
                                            lfsm = inverseApplyLangPropertyImmutModel lprop $ pure ()
                                            in cfmap
                                                ( MkCatRange
                                                    (shimToFunction $ iMeetPair id praContra . econv)
                                                    (shimToFunction praCo)
                                                )
                                                lfsm
                                    )
                                    propertyexpr
                                    saaexpr
                        return $ MkLangExpression $ MkSealedExpression stype sexpr
                , valBDS
                    "fetch"
                    "`!{fetch @A}: Store -> Entity -> Action A`  \nFetch the full value of an `Entity` from storage, or stop. Note values are removed from storage when no triple refers to them."
                    $ \(MkLangType (ta :: _ a)) -> do
                        eta <- getMonoStorableType ta
                        saaexpr <- monoStoreAdapter eta
                        let
                            stype :: QShimWit 'Positive (QStore -> Entity -> Action a)
                            stype =
                                qFunctionPosWitness qType
                                    $ qFunctionPosWitness qType
                                    $ actionShimWit
                                    $ nonpolarToPositive @QTypeSystem ta
                            sexpr :: QOpenExpression (QStore -> Entity -> Action a)
                            sexpr = fmap storeFetch saaexpr
                        return $ MkLangExpression $ MkSealedExpression stype sexpr
                , recordValueBDS
                    "openLocal"
                    "Open a `Store` from a directory. Will be closed at the end of the lifecycle."
                    ( ConsListType
                        ( ValueDocSignature
                            @(Maybe Text)
                            "path"
                            "Path to directory, or `Nothing` to use the data directory."
                            qType
                            $ Just
                            $ pure Nothing
                        )
                        $ ConsListType
                            ( ValueDocSignature @Bool "cache" "Whether to cache commits (default: `True`)." qType
                                $ Just
                                $ pure True
                            )
                            NilListType
                    )
                    openLocalStore
                , valBDS "openTemp" "Open a `Store` from memory. Nothing will be persisted." openTempStore
                ]
          ]
