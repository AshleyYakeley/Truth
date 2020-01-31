module Main
    ( main
    ) where

import Shapes
import Test.Tasty
import Test.Tasty.HUnit
import Truth.Core
import Truth.UI.GTK

testGView :: forall sel. String -> LUISpec sel -> TestTree
testGView name luispec =
    testCase name $
    runLifeCycle $ do
        uispec <- luispec
        case getMaybeView uispec of
            Just _ -> return ()
            Nothing -> liftIO $ assertFailure "not matched"

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
        [ testGView "null" $ (nullUISpec :: LUISpec UnitEdit)
        , testGView "vertical layout" $ verticalUISpec [(return $ MkUISpec MkUIUnknown :: LUISpec UnitEdit, False)]
        ]

tests :: TestTree
tests = testGroup "truth-gtk" [testGViews]

main :: IO ()
main = defaultMain tests
