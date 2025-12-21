module Test.Storage
    ( testStorage
    )
where

import Changes.Core
import Pinafore.Base
import Shapes
import Shapes.Test

import Pinafore.Storage

data TestContext = MkTestContext
    { putProperty :: forall s v. StoreAdapter s -> StoreAdapter v -> Predicate -> s -> Know v -> IO ()
    , readProperty ::
        forall s v.
        (Show v, Eq v) => StoreAdapter s -> StoreAdapter v -> Predicate -> s -> Know v -> IO ()
    , readCount :: forall s v. Eq s => StoreAdapter s -> StoreAdapter v -> Predicate -> v -> Int -> IO ()
    , checkEmpty :: String -> IO ()
    , checkNonEmpty :: String -> IO ()
    }

testStorageCase :: String -> (TestContext -> IO ()) -> TestTree
testStorageCase name action =
    testTree name $ do
        storageRef <- makeMemoryReference (MkQTableSubject [] [] [] []) $ \_ -> True
        let
            getState :: IO QTableSubject
            getState = runResource emptyResourceContext storageRef $ \aref -> refRead aref ReadWhole
            checkEmpty :: String -> IO ()
            checkEmpty msg = do
                state <- getState
                case state of
                    MkQTableSubject [] [] [] [] -> return ()
                    MkQTableSubject ps rs fs ls ->
                        fail $ msg <> ": non-empty state: " <> show (length ps, length rs, length fs, length ls)
            checkNonEmpty :: String -> IO ()
            checkNonEmpty msg = do
                state <- getState
                case state of
                    MkQTableSubject [] [] [] [] -> fail $ msg <> ": empty state"
                    _ -> return ()
            tableRef = convertReference storageRef
            entityRef = qTableEntityReference tableRef
            putProperty :: forall s v. StoreAdapter s -> StoreAdapter v -> Predicate -> s -> Know v -> IO ()
            putProperty st vt p s kv =
                runResource emptyResourceContext entityRef $ \aref ->
                    pushOrFail "can't push table edit" noEditSource $ refEdit aref $ pure $ MkQStorageEdit st vt p s kv
            readProperty ::
                forall s v.
                (Show v, Eq v) =>
                StoreAdapter s ->
                StoreAdapter v ->
                Predicate ->
                s ->
                Know v ->
                IO ()
            readProperty st vt p s kv = do
                e <- runResource emptyResourceContext entityRef $ \aref -> refRead aref $ QStorageReadGet st p s
                case kv of
                    Known v -> do
                        assertEqual "entity" (storeAdapterConvert vt v) e
                        kv' <-
                            runResource emptyResourceContext entityRef $ \aref -> refRead aref $ QStorageReadEntity vt e
                        assertEqual "typed" (Known v) kv'
                    Unknown -> return ()
            readCount ::
                forall s v.
                Eq s =>
                StoreAdapter s ->
                StoreAdapter v ->
                Predicate ->
                v ->
                Int ->
                IO ()
            readCount st vt p v expcount = do
                fset <-
                    runResource emptyResourceContext entityRef $ \aref ->
                        refRead aref $ QStorageReadLookup p (storeAdapterConvert vt v)
                assertEqual "entity count" expcount $ length fset
                tset <-
                    runResource emptyResourceContext entityRef $ \aref ->
                        listSetForF fset $ \e -> fmap knowToMaybe $ refRead aref $ QStorageReadEntity st e
                assertEqual "typed count" expcount $ length tset
            testContext = MkTestContext{..}
        action testContext

testAddRemoveProperty :: (Eq s, Eq v, Show v) => StoreAdapter s -> StoreAdapter v -> s -> v -> TestContext -> IO ()
testAddRemoveProperty st vt s v MkTestContext{..} = do
    pA <- fmap MkPredicate randomIO
    checkEmpty "0"
    readCount st vt pA v 0
    --
    readProperty st vt pA s Unknown
    checkNonEmpty "1"
    --
    putProperty st vt pA s (Known v)
    checkNonEmpty "2"
    readProperty st vt pA s (Known v)
    readCount st vt pA v 1
    --
    putProperty st vt pA s Unknown
    checkEmpty "3"
    readCount st vt pA v 0

textAdapter :: StoreAdapter Text
textAdapter = asLiteralStoreAdapter

maybeTextAdapter :: StoreAdapter (Maybe Text)
maybeTextAdapter = let
    justAnchor = codeAnchor "pinafore-base:Just"
    justAdapter = constructorStoreAdapter justAnchor $ ConsListType textAdapter NilListType
    nothingAnchor = codeAnchor "pinafore-base:Nothing"
    nothingAdapter = constructorStoreAdapter nothingAnchor NilListType
    from :: Either (a, ()) () -> Maybe a
    from (Left (a, ())) = Just a
    from (Right ()) = Nothing
    to :: Maybe a -> Either (a, ()) ()
    to (Just a) = Left (a, ())
    to Nothing = Right ()
    in invmap from to $ justAdapter <+++> nothingAdapter

listTextAdapter :: StoreAdapter [Text]
listTextAdapter = let
    nilAnchor = codeAnchor "pinafore-base:Nil"
    nilAdapter = constructorStoreAdapter nilAnchor NilListType
    consAnchor = codeAnchor "pinafore-base:Cons"
    consAdapter = constructorStoreAdapter consAnchor $ ConsListType textAdapter $ ConsListType listAdapter NilListType
    listAdapter = invmap from to $ nilAdapter <+++> consAdapter
    from :: Either () (a, ([a], ())) -> [a]
    from (Left ()) = []
    from (Right (a, (aa, ()))) = a : aa
    to :: [a] -> Either () (a, ([a], ()))
    to [] = Left ()
    to (a : aa) = Right (a, (aa, ()))
    in listAdapter

enumAdapter :: StoreAdapter Bool
enumAdapter = let
    falseAnchor = codeAnchor "pinafore-base:False"
    falseAdapter = constructorStoreAdapter falseAnchor NilListType
    trueAnchor = codeAnchor "pinafore-base:True"
    trueAdapter = constructorStoreAdapter trueAnchor NilListType
    from :: Either () () -> Bool
    from (Left ()) = False
    from (Right ()) = True
    to :: Bool -> Either () ()
    to False = Left ()
    to True = Right ()
    in invmap from to $ falseAdapter <+++> trueAdapter

testStorage :: TestTree
testStorage =
    testTree
        "storage"
        [ testStorageCase "empty" $ \MkTestContext{..} -> checkEmpty "0"
        , testStorageCase "property-plain-plain" $ \tc -> do
            e1 <- newEntity
            e2 <- newEntity
            testAddRemoveProperty plainStoreAdapter plainStoreAdapter e1 e2 tc
        , testStorageCase "property-plain-plain-same" $ \tc -> do
            e <- newEntity
            testAddRemoveProperty plainStoreAdapter plainStoreAdapter e e tc
        , testStorageCase "property-plain-text" $ \tc -> do
            e1 <- newEntity
            testAddRemoveProperty plainStoreAdapter textAdapter e1 "hello" tc
        , testStorageCase "property-text-plain" $ \tc -> do
            e2 <- newEntity
            testAddRemoveProperty textAdapter plainStoreAdapter "hello" e2 tc
        , testStorageCase "property-text-text" $ \tc -> do
            testAddRemoveProperty textAdapter textAdapter "this" "that" tc
        , testStorageCase "property-text-text-same" $ \tc -> do
            testAddRemoveProperty textAdapter textAdapter "this" "this" tc
        , testStorageCase "property-plain-mtext" $ \tc -> do
            e1 <- newEntity
            testAddRemoveProperty plainStoreAdapter maybeTextAdapter e1 (Just "hello") tc
        , testStorageCase "property-plain-bool1" $ \tc -> do
            e1 <- newEntity
            testAddRemoveProperty plainStoreAdapter enumAdapter e1 False tc
        , testStorageCase "property-plain-bool2" $ \tc -> do
            e1 <- newEntity
            testAddRemoveProperty plainStoreAdapter enumAdapter e1 True tc
        , testStorageCase "property-plain-ltext" $ \tc -> do
            e1 <- newEntity
            testAddRemoveProperty plainStoreAdapter listTextAdapter e1 ["abc", "def"] tc
        ]
