module GIO
    ( gioTests
    ) where

import Changes.Core
import Changes.World.GNOME.GIO
import qualified GI.Gio as GI
import Shapes
import Shapes.Test

gioTests :: TestTree
gioTests =
    testTree
        "gio"
        [ testTree "file" $ do
              f <- GI.fileNewForPath "test/somefile"
              ref <- giFileReference f
              mtbs <- runResource emptyResourceContext ref $ \aref -> readableToSubject $ refRead aref
              case mtbs of
                  Nothing -> fail "no read"
                  Just (_, bs) -> assertEqual "" "AAABBBCD\n" $ decodeUtf8 bs
        ]
