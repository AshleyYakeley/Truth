module GIO
    ( gioTests
    ) where

import Changes.Core
import Changes.World.GNOME.GIO.File
import Changes.World.GNOME.GIO.ReferenceStream
import qualified GI.Gio as GI
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

modelTest :: TestTree
modelTest =
    expectFailBecause "ISSUE #285" $
    testTree "model" $
    runLifecycle $
    runView $ do
        wref :: Reference (WholeEdit LazyByteString) <-
            liftIO $ makeMemoryReference (fromList [7, 4, 6, 1, 2]) (\_ -> True)
        let
            bref :: Reference ByteStringEdit
            bref = convertReference wref
        mis :: ReferenceInputStream <- byteStringReferenceInputStream bref
        _b <- GI.inputStreamReadBytes mis 3 (Nothing @GI.Cancellable)
        return ()

gioTests :: TestTree
gioTests = testTree "gio" [fileTest, modelTest]
