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
tests = testTree "pinafore-base" [testNumeric, testAnchor, testOverlay]

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

main :: IO ()
main = testMain tests
