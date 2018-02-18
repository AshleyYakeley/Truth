module Main
    ( main
    ) where

import Shapes
import Test.Tasty
import Test.Tasty.HUnit
import Truth.Core
import Truth.UI.GTK

testGView ::
       forall edit. ApplicableEdit edit
    => UISpec edit
    -> TestTree
testGView uispec =
    testCase (show uispec) $
    case getMaybeView uispec of
        Just _ -> return ()
        Nothing -> assertFailure "not matched"

data UIUnknown edit where
    MkUIUnknown :: UIUnknown edit

instance Show (UIUnknown edit) where
    show MkUIUnknown = "unknown"

instance UIType UIUnknown where
    uiWitness = $(iowitness [t|UIUnknown|])

testGViews :: TestTree
testGViews =
    testGroup
        "GView"
        [ testGView $ (uiVertical [] :: UISpec (WholeEdit String))
        , testGView $ uiVertical [(MkUISpec MkUIUnknown :: UISpec (WholeEdit String), False)]
        ]

tests :: TestTree
tests = testGroup "truth-gtk" [testGViews]

main :: IO ()
main = defaultMain tests
