module Main
    ( main
    ) where

import Shapes
import Test.Tasty
import Test.Tasty.HUnit
import Truth.Core
import Truth.UI.GTK

testGView :: forall sel. UISpec sel -> TestTree
testGView uispec =
    testCase (show uispec) $
    case getMaybeView uispec of
        Just _ -> return ()
        Nothing -> assertFailure "not matched"

data UIUnknown sel where
    MkUIUnknown :: UIUnknown sel

instance Show (UIUnknown sel) where
    show MkUIUnknown = "unknown"

instance UIType UIUnknown where
    uiWitness = $(iowitness [t|UIUnknown|])

testGViews :: TestTree
testGViews =
    testGroup
        "GView"
        [ testGView $ (nullUISpec :: UISpec UnitEdit)
        , testGView $ verticalUISpec [(MkUISpec MkUIUnknown :: UISpec UnitEdit, False)]
        ]

tests :: TestTree
tests = testGroup "truth-gtk" [testGViews]

main :: IO ()
main = defaultMain tests
