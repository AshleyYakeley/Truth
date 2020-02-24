module Main
    ( main
    ) where

import Shapes
import Test.Tasty
import Test.Tasty.HUnit
import Truth.Core
import Truth.UI.GTK

testGView :: String -> CVUISpec -> TestTree
testGView name luispec =
    testCase name $
    runLifeCycle $ do
        uispec <- uitRunCreateView nullUIToolkit emptyResourceContext luispec
        case getMaybeView uispec of
            Just _ -> return ()
            Nothing -> liftIO $ assertFailure "not matched"

data UIUnknown where
    MkUIUnknown :: UIUnknown

instance Show UIUnknown where
    show MkUIUnknown = "unknown"

instance UIType UIUnknown where
    uiWitness = $(iowitness [t|UIUnknown|])

testGViews :: TestTree
testGViews =
    testGroup
        "GView"
        [ testGView "null" $ (nullUISpec :: CVUISpec)
        , testGView "vertical layout" $ verticalUISpec [(return $ MkUISpec MkUIUnknown :: CVUISpec, False)]
        ]

tests :: TestTree
tests = testGroup "truth-gtk" [testGViews]

main :: IO ()
main = defaultMain tests
