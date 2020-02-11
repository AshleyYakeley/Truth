module Lens
    ( testLens
    ) where

import Shapes
import Test.Tasty

--import Test.Tasty.Golden
--import Truth.Core
import Test.Tasty.HUnit

{-
contextOrderedSetLens ::
       forall updateX updateN cont seq.
       ( Index seq ~ Int
       , HasKeyUpdate cont updateN
       , FullSubjectReader (UpdateReader updateN)
       , Element cont ~ UpdateSubject updateN
       --, ApplicableEdit (UpdateEdit updateX)
       , ApplicableEdit (UpdateEdit updateN)
       , IsUpdate updateN
       )
    => UpdateOrder (ContextUpdate updateX updateN)
    -> FloatingEditLens (ContextUpdate updateX (KeyUpdate cont updateN)) (ContextUpdate updateX (OrderedListUpdate seq updateN))
-}
-- updateN ~ ConstWholeUpdate String
-- arrange a list of characters in an order
-- the order is given in the context as a map from Char to Int
testContextOrderedSetLens :: TestTree
testContextOrderedSetLens = testCase "contextOrderedSetLens" $ fail "fail"

testLens :: TestTree
testLens = testGroup "lens" [testContextOrderedSetLens]
