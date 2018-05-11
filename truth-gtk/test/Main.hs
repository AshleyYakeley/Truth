module Main
    ( main
    ) where

import Shapes
import Test.Tasty
import Test.Tasty.HUnit
import Truth.Core
import Truth.UI.GTK

testGView ::
       forall edit seledit. ApplicableEdit edit
    => UISpec seledit edit
    -> TestTree
testGView uispec =
    testCase (show uispec) $
    case getMaybeView uispec of
        Just _ -> return ()
        Nothing -> assertFailure "not matched"

data UIUnknown seledit edit where
    MkUIUnknown :: UIUnknown seledit edit

instance Show (UIUnknown seledit edit) where
    show MkUIUnknown = "unknown"

instance UIType UIUnknown where
    uiWitness = $(iowitness [t|UIUnknown|])

testGViews :: TestTree
testGViews =
    testGroup
        "GView"
        [ testGView $ (uiNull :: UISpec UnitEdit (WholeEdit String))
        , testGView $ uiVertical [(MkUISpec MkUIUnknown :: UISpec UnitEdit (WholeEdit String), False)]
        ]

tests :: TestTree
tests = testGroup "truth-gtk" [testGViews]

main :: IO ()
main = defaultMain tests
