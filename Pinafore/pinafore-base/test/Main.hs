module Main
    ( main
    )
where

import Changes.Core
import Shapes
import Shapes.Test

import Pinafore.Base
import Test.Anchor
import Test.Numeric

tests :: TestTree
tests = testTree "pinafore-base" [testNumeric, testAnchor, testOverlay, testInverseFiniteSet]

testOverlay :: TestTree
testOverlay = testTree "storage overlay" $ do
    p <- MkPredicate <$> randomIO
    s <- newEntity
    t <- newEntity
    a <- newEntity
    b <- newEntity
    let
        storage :: [(Entity, Entity)] -> Readable IO QStorageRead
        storage rows = \case
            QStorageReadGet adapter _ subject -> return $ maybeToKnow $ lookup (storeAdapterConvert adapter subject) rows
            QStorageReadLookup _ value -> return $ setFromList [subject | (subject, v) <- rows, v == value]
            QStorageReadEntity _ _ -> return Unknown
        paired :: [(Entity, Entity)] -> [(Entity, Entity)] -> Readable IO (PairUpdateReader QStorageUpdate QStorageUpdate)
        paired upper _lower (MkTupleUpdateReader SelectFirst rt) = storage upper rt
        paired _upper lower (MkTupleUpdateReader SelectSecond rt) = storage lower rt
        rd :: Readable IO (PairUpdateReader QStorageUpdate QStorageUpdate)
        rd = paired [(s, a)] [(s, b), (t, b)]
        qsrGet subject = QStorageReadGet plainStoreAdapter p subject
        checkUpdate :: String -> [(Entity, Know Entity)] -> PairUpdate QStorageUpdate QStorageUpdate -> Readable IO (PairUpdateReader QStorageUpdate QStorageUpdate) -> IO ()
        checkUpdate label expected update rr = do
            updates <- clUpdate overlayStorageLens update rr
            assertEqual label expected $ fmap (\(MkQStorageUpdate _ subject value) -> (subject, value)) updates
    clRead overlayStorageLens rd (qsrGet s) >>= assertEqual "first store wins" (Known a)
    clRead overlayStorageLens rd (qsrGet t) >>= assertEqual "fallback read" (Known b)
    matches <- clRead overlayStorageLens rd $ QStorageReadLookup p b
    assertEqual "shadowed inverse lookup" [t] $ setToList matches
    foundDuplicates <- clRead overlayStorageLens (paired [(s, b)] [(s, b)]) $ QStorageReadLookup p b
    assertEqual "deduplicated inverse lookup" [s] $ setToList foundDuplicates
    checkUpdate "hidden lower update" [] (MkTupleUpdate SelectSecond $ MkQStorageUpdate p s $ Known b) rd
    checkUpdate "visible lower update" [(t, Known b)] (MkTupleUpdate SelectSecond $ MkQStorageUpdate p t $ Known b) rd
    checkUpdate "deletion reveals fallback" [(s, Known b)] (MkTupleUpdate SelectFirst $ MkQStorageUpdate p s Unknown) $ paired [] [(s, b)]
    checkUpdate "deletion without fallback" [(s, Unknown)] (MkTupleUpdate SelectFirst $ MkQStorageUpdate p s Unknown) $ paired [] []
    edits <- clPutEdits overlayStorageLens [MkQStorageEdit plainStoreAdapter plainStoreAdapter p s $ Known a] rd
    case edits of
        Just [MkTupleUpdateEdit SelectFirst edit] ->
            applyEdit edit (storage []) (qsrGet s) >>= assertEqual "write to first store" (Known a)
        _ -> fail "overlay edits must target only the first store"

-- Inverse-property models expose unit-valued maps, while preimage lookup
-- remains set-valued. Check reads and incremental updates at that boundary.
testInverseFiniteSet :: TestTree
testInverseFiniteSet = testTree "inverse finite sets" $ do
    let
        property :: StorageLensProperty Char Char Char Char (NoUpdate ())
        property = identityStorageLensProperty
        singleLens = storageLensPropertyInverseChangeLens property
        singleSource :: Know Char -> Readable IO (ContextUpdateReader (NoUpdate ()) (BiWholeUpdate (Know Char) (Know Char)))
        singleSource value = subjectToReadable $ MkWithContext () value
        setLens = storageLensPropertyInverseChangeLensSet property
        setSource :: String -> Readable IO (ContextUpdateReader (NoUpdate ()) (FiniteSetUpdate Char))
        setSource values = subjectToReadable $ MkWithContext () $ listSetToMap (const ()) $ setFromList values
        keysOf :: Readable IO (FiniteSetReader Char) -> IO String
        keysOf mr = fmap (sort . toList) $ mr MapReadKeys
    keysOf (clRead singleLens $ singleSource $ Known 'a') >>= assertEqual "single preimage" "a"
    keysOf (clRead singleLens $ singleSource Unknown) >>= assertEqual "unknown preimage" ""
    for_ [Known 'b', Unknown] $ \value -> do
        updates <- clUpdate singleLens (MkTupleUpdate SelectContent $ MkBiWholeUpdate value) $ singleSource value
        let
            result :: Readable IO (FiniteSetReader Char)
            result = applyEdits (fmap updateEdit updates) $ clRead singleLens $ singleSource $ Known 'a'
        expected <- keysOf $ clRead singleLens $ singleSource value
        keysOf result >>= assertEqual "single replacement" expected
    clRead setLens (setSource "a") (MapReadKey 'a') >>= assertEqual "present" (Just 'a')
    clRead setLens (setSource "a") (MapReadKey 'b') >>= assertEqual "absent" Nothing
    for_ [(MapUpdateSet 'b' (), "ab"), (MapUpdateDelete 'a', ""), (MapUpdateClear, "")] $ \(update, values) -> do
        updates <- clUpdate setLens (MkTupleUpdate SelectContent update) $ setSource values
        let
            result :: Readable IO (FiniteSetReader Char)
            result = applyEdits (fmap updateEdit updates) $ clRead setLens $ setSource "a"
        keysOf result >>= assertEqual "set update" values
    edits <- clPutEdits setLens [MapEditSet 'b' ()] $ setSource "a"
    case edits of
        Just [MkTupleUpdateEdit SelectContent (MapEditSet 'b' ())] -> return ()
        _ -> fail "inverse insertion must insert a unit-valued map entry"

main :: IO ()
main = testMain tests
