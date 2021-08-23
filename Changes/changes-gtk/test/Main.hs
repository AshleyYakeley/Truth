module Main
    ( main
    ) where

import qualified GI.Cairo.Render.Matrix as RM
import Shapes
import Shapes.Test

{- no tests currently
import Changes.Core
import Changes.UI.GTK
-}
matrixTest :: TestTree
matrixTest =
    testTree "matrix" $ let
        m = RM.translate 1 2 $ RM.scale 4 (0.5) $ RM.translate 2 1 $ RM.identity
        p0 = (3, 5)
        p1 = RM.transformPoint m p0
        p2 = RM.transformPoint (RM.invert m) p1
        in assertEqual "" p0 p2

tests :: TestTree
tests = testTree "changes-gtk" [matrixTest]

main :: IO ()
main = testMain tests
