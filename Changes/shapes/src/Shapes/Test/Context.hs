module Shapes.Test.Context
    ( ContextTestTree (..)
    , runContextTestTree
    , tGroup
    , tGroupSeq
    , tModify
    , tContext
    )
where

import Shapes
import Shapes.Test

newtype ContextTestTree c
    = MkContextTestTree (c -> TestTree)

runContextTestTree :: c -> ContextTestTree c -> TestTree
runContextTestTree c (MkContextTestTree test) = test c

tGroup :: String -> [ContextTestTree c] -> ContextTestTree c
tGroup name tests = MkContextTestTree $ \c -> testGroup name $ fmap (\(MkContextTestTree test) -> test c) tests

tGroupSeq :: String -> [ContextTestTree c] -> ContextTestTree c
tGroupSeq name tests = MkContextTestTree $ \c -> inOrderTestGroup name $ fmap (\(MkContextTestTree test) -> test c) tests

tModify :: (TestTree -> TestTree) -> ContextTestTree c -> ContextTestTree c
tModify f (MkContextTestTree ct) = MkContextTestTree $ \c -> f $ ct c

tContext :: (c -> c) -> ContextTestTree c -> ContextTestTree c
tContext f (MkContextTestTree ct) = MkContextTestTree $ \c -> ct $ f c
