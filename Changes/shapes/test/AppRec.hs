module AppRec
    ( testAppRec
    )
where

import Shapes
import Shapes.Test

test1 :: TestTree
test1 =
    testTree "1" $ do
        wit3 <- newIOWitness
        let
            a1 :: AppRec Maybe [Int]
            a1 = pure [3, 4, 5]
            a2 :: AppRec Maybe [Int]
            a2 = liftA2 (<>) a1 (appKnotRec a3)
            a3 :: AppKnot Maybe [Int]
            a3 = knotAppRec wit3 $ fmap (\i2 -> [1, 2] <> i2) a2
            found :: Maybe [Int]
            found = fmap (take 12) $ appKnotResult a3
        assertEqual "" (Just [1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2]) found

test2 :: TestTree
test2 =
    testTree "2" $ do
        wit2 <- newIOWitness
        wit3 <- newIOWitness
        let
            a1 :: AppRec Maybe [Int]
            a1 = pure [3, 4, 5]
            a2 :: AppKnot Maybe [Int]
            a2 = knotAppRec wit2 $ liftA2 (<>) a1 (appKnotRec a3)
            a3 :: AppKnot Maybe [Int]
            a3 = knotAppRec wit3 $ fmap (\i2 -> [1, 2] <> i2) (appKnotRec a2)
            found :: Maybe [Int]
            found = fmap (take 12) $ appKnotResult a3
        assertEqual "" (Just [1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2]) found

testAppRec :: TestTree
testAppRec = testTree "apprec" [test1, test2]
