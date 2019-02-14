module Main
    ( main
    ) where

import Shapes
import Test.Tasty
import Test.Tasty.HUnit
import Truth.Core
import Truth.UI.GTK

testGView ::
       forall edit sel. ApplicableEdit edit
    => UISpec sel edit
    -> TestTree
testGView uispec =
    testCase (show uispec) $
    case getMaybeView uispec of
        Just _ -> return ()
        Nothing -> assertFailure "not matched"

data UIUnknown sel edit where
    MkUIUnknown :: UIUnknown sel edit

instance Show (UIUnknown sel edit) where
    show MkUIUnknown = "unknown"

instance UIType UIUnknown where
    uiWitness = $(iowitness [t|UIUnknown|])

testGViews :: TestTree
testGViews =
    testGroup
        "GView"
        [ testGView $ (nullUISpec :: UISpec UnitEdit (WholeEdit String))
        , testGView $ verticalUISpec [(MkUISpec MkUIUnknown :: UISpec UnitEdit (WholeEdit String), False)]
        ]

tests :: TestTree
tests = testGroup "truth-gtk" [testGViews]

main :: IO ()
main = defaultMain tests
