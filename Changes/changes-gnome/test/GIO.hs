module GIO
    ( gioTests
    ) where

import Changes.Core
import Changes.World.GNOME.GIO.File
import Changes.World.GNOME.GIO.ReferenceStream
import Foreign.C
import GI.GLib qualified as GI
import GI.Gio qualified as GI
import Shapes
import Shapes.Test

fileTest :: TestTree
fileTest =
    testTree "file" $ do
        f <- GI.fileNewForPath "test/somefile"
        ref <- giFileReference f
        mtbs <- runResource emptyResourceContext ref $ \aref -> readableToSubject $ refRead aref
        case mtbs of
            Nothing -> fail "no read"
            Just (_, bs) -> assertEqual "" "AAABBBCD\n" $ decodeUtf8 bs

noCancellable :: Maybe GI.Cancellable
noCancellable = Nothing

modelTest :: TestTree
modelTest =
    testTree "model" $
    runLifecycle $
    runView $ do
        wref :: Reference (WholeEdit LazyByteString) <-
            liftIO $ makeMemoryReference (fromList [7, 4, 6, 1, 2]) (\_ -> True)
        let
            bref :: Reference ByteStringEdit
            bref = convertReference wref
        mis :: ReferenceInputStream <- byteStringReferenceInputStream bref
        let
            checkVal ::
                   forall a. (Eq a, Show a)
                => a
                -> View a
                -> View ()
            checkVal expected call = do
                found <- call
                liftIO $ assertEqual "" expected found
            iRead :: CSize -> View [Word8]
            iRead n = do
                rb <- GI.inputStreamReadBytes mis n noCancellable
                Just bs <- GI.bytesGetData rb
                return $ unpack bs
            iSkip :: CSize -> View Int64
            iSkip n = GI.inputStreamSkip mis n noCancellable
            iSeek :: GI.SeekType -> Int64 -> View ()
            iSeek stype n = GI.seekableSeek mis n stype noCancellable
            iTell :: View Int64
            iTell = GI.seekableTell mis
        checkVal 0 iTell
        checkVal [7, 4, 6] $ iRead 3
        checkVal 3 iTell
        checkVal [1, 2] $ iRead 10
        checkVal 5 iTell
        iSeek GI.SeekTypeSet 1
        checkVal 1 iTell
        checkVal [4, 6] $ iRead 2
        checkVal 1 $ iSkip 1
        checkVal 4 iTell
        iSeek GI.SeekTypeCur (-1)
        checkVal 3 iTell
        iSeek GI.SeekTypeEnd (-3)
        checkVal 2 iTell
        checkVal [6, 1, 2] $ iRead 4
        checkVal 5 iTell

gioTests :: TestTree
gioTests = testTree "gio" [fileTest, modelTest]
