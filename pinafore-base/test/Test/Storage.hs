module Test.Storage
    ( testStorage
    ) where

import Changes.Core
import Pinafore.Base
import Pinafore.Storage
import Shapes
import Test.Tasty
import Test.Tasty.HUnit

data TestContext = MkTestContext
    { putProperty :: forall s v. EntityAdapter s -> EntityAdapter v -> Predicate -> s -> Know v -> IO ()
    , readProperty :: forall s v.
                          (Show v, Eq v) => EntityAdapter s -> EntityAdapter v -> Predicate -> s -> Know v -> IO ()
    , readCount :: forall s v. EntityAdapter s -> EntityAdapter v -> Predicate -> v -> Int -> IO ()
    , checkEmpty :: String -> IO ()
    , checkNonEmpty :: String -> IO ()
    }

testStorageCase :: String -> (TestContext -> IO ()) -> TestTree
testStorageCase name action =
    testCase name $ do
        stateRef <- makeMemoryReference (MkPinaforeTableSubject [] [] [] []) $ \_ -> True
        let
            getState :: IO PinaforeTableSubject
            getState = runResource emptyResourceContext stateRef $ \aref -> refRead aref ReadWhole
            checkEmpty :: String -> IO ()
            checkEmpty msg = do
                state <- getState
                case state of
                    MkPinaforeTableSubject [] [] [] [] -> return ()
                    MkPinaforeTableSubject ps rs fs ls ->
                        fail $ msg <> ": non-empty state: " <> show (length ps, length rs, length fs, length ls)
            checkNonEmpty :: String -> IO ()
            checkNonEmpty msg = do
                state <- getState
                case state of
                    MkPinaforeTableSubject [] [] [] [] -> fail $ msg <> ": empty state"
                    _ -> return ()
            tableRef = convertReference stateRef
            entityRef = pinaforeTableEntityReference tableRef
            putProperty :: forall s v. EntityAdapter s -> EntityAdapter v -> Predicate -> s -> Know v -> IO ()
            putProperty st vt p s kv =
                runResource emptyResourceContext entityRef $ \aref ->
                    pushOrFail "can't push table edit" noEditSource $
                    refEdit aref $ pure $ MkPinaforeStorageEdit st vt p s kv
            readProperty ::
                   forall s v. (Show v, Eq v)
                => EntityAdapter s
                -> EntityAdapter v
                -> Predicate
                -> s
                -> Know v
                -> IO ()
            readProperty st vt p s kv = do
                e <- runResource emptyResourceContext entityRef $ \aref -> refRead aref $ PinaforeStorageReadGet st p s
                case kv of
                    Known v -> do
                        assertEqual "entity" (entityAdapterConvert vt v) e
                        kv' <-
                            runResource emptyResourceContext entityRef $ \aref ->
                                refRead aref $ PinaforeStorageReadEntity vt e
                        assertEqual "typed" (Known v) kv'
                    Unknown -> return ()
            readCount :: forall s v. EntityAdapter s -> EntityAdapter v -> Predicate -> v -> Int -> IO ()
            readCount st vt p v expcount = do
                fset <-
                    runResource emptyResourceContext entityRef $ \aref ->
                        refRead aref $ PinaforeStorageReadLookup p (entityAdapterConvert vt v)
                assertEqual "entity count" expcount $ length fset
                tset <-
                    runResource emptyResourceContext entityRef $ \aref ->
                        forf fset $ \e -> fmap knowToMaybe $ refRead aref $ PinaforeStorageReadEntity st e
                assertEqual "typed count" expcount $ length tset
            testContext = MkTestContext {..}
        action testContext

testAddRemoveProperty :: (Eq v, Show v) => EntityAdapter s -> EntityAdapter v -> s -> v -> TestContext -> IO ()
testAddRemoveProperty st vt s v MkTestContext {..} = do
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

textAdapter :: EntityAdapter Text
textAdapter = literalEntityAdapter

maybeTextAdapter :: EntityAdapter (Maybe Text)
maybeTextAdapter = let
    justAnchor = codeAnchor "pinafore-base:Just"
    justAdapter = constructorEntityAdapter justAnchor $ ConsListType textAdapter NilListType
    nothingAnchor = codeAnchor "pinafore-base:Nothing"
    nothingAdapter = constructorEntityAdapter nothingAnchor NilListType
    from :: Either (a, ()) () -> Maybe a
    from (Left (a, ())) = Just a
    from (Right ()) = Nothing
    to :: Maybe a -> Either (a, ()) ()
    to (Just a) = Left (a, ())
    to Nothing = Right ()
    in isoMap from to $ justAdapter <+++> nothingAdapter

listTextAdapter :: EntityAdapter [Text]
listTextAdapter = let
    nilAnchor = codeAnchor "pinafore-base:Nil"
    nilAdapter = constructorEntityAdapter nilAnchor NilListType
    consAnchor = codeAnchor "pinafore-base:Cons"
    consAdapter = constructorEntityAdapter consAnchor $ ConsListType textAdapter $ ConsListType listAdapter NilListType
    listAdapter = isoMap from to $ nilAdapter <+++> consAdapter
    from :: Either () (a, ([a], ())) -> [a]
    from (Left ()) = []
    from (Right (a, (aa, ()))) = a : aa
    to :: [a] -> Either () (a, ([a], ()))
    to [] = Left ()
    to (a:aa) = Right (a, (aa, ()))
    in listAdapter

enumAdapter :: EntityAdapter Bool
enumAdapter = let
    falseAnchor = codeAnchor "pinafore-base:False"
    falseAdapter = constructorEntityAdapter falseAnchor NilListType
    trueAnchor = codeAnchor "pinafore-base:True"
    trueAdapter = constructorEntityAdapter trueAnchor NilListType
    from :: Either () () -> Bool
    from (Left ()) = False
    from (Right ()) = True
    to :: Bool -> Either () ()
    to False = Left ()
    to True = Right ()
    in isoMap from to $ falseAdapter <+++> trueAdapter

testStorage :: TestTree
testStorage =
    testGroup
        "storage"
        [ testStorageCase "empty" $ \MkTestContext {..} -> checkEmpty "0"
        , testStorageCase "property-plain-plain" $ \tc -> do
              e1 <- newEntity
              e2 <- newEntity
              testAddRemoveProperty plainEntityAdapter plainEntityAdapter e1 e2 tc
        , testStorageCase "property-plain-plain-same" $ \tc -> do
              e <- newEntity
              testAddRemoveProperty plainEntityAdapter plainEntityAdapter e e tc
        , testStorageCase "property-plain-text" $ \tc -> do
              e1 <- newEntity
              testAddRemoveProperty plainEntityAdapter textAdapter e1 "hello" tc
        , testStorageCase "property-text-plain" $ \tc -> do
              e2 <- newEntity
              testAddRemoveProperty textAdapter plainEntityAdapter "hello" e2 tc
        , testStorageCase "property-text-text" $ \tc -> do
              testAddRemoveProperty textAdapter textAdapter "this" "that" tc
        , testStorageCase "property-text-text-same" $ \tc -> do
              testAddRemoveProperty textAdapter textAdapter "this" "this" tc
        , testStorageCase "property-plain-mtext" $ \tc -> do
              e1 <- newEntity
              testAddRemoveProperty plainEntityAdapter maybeTextAdapter e1 (Just "hello") tc
        , testStorageCase "property-plain-bool1" $ \tc -> do
              e1 <- newEntity
              testAddRemoveProperty plainEntityAdapter enumAdapter e1 False tc
        , testStorageCase "property-plain-bool2" $ \tc -> do
              e1 <- newEntity
              testAddRemoveProperty plainEntityAdapter enumAdapter e1 True tc
        , testStorageCase "property-plain-ltext" $ \tc -> do
              e1 <- newEntity
              testAddRemoveProperty plainEntityAdapter listTextAdapter e1 ["abc", "def"] tc
        ]
