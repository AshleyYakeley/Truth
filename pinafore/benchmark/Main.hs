module Main
    ( main
    ) where

import Criterion.Main
import Pinafore
import Pinafore.Test
import Shapes

benchmarkText :: Text -> Benchmark
benchmarkText text =
    env (fmap const makeTestPinaforeContext) $ \pc ->
        bgroup
            (show $ unpack text)
            [ bench "check" $ nfIO $ pinaforeInterpretFile (pc ()) "<test>" text >> return ()
            , env (fmap const $ pinaforeInterpretFile (pc ()) "<test>" text) $ \action -> bench "run" $ nfIO (action ())
            ]

main :: IO ()
main =
    defaultMain
        [ benchmarkText "runref {pass}"
        , benchmarkText "runref $ pureref pass"
        , benchmarkText "get {pass} $ \\v -> v"
        , benchmarkText "get {false} $ \\v -> pass"
        , benchmarkText "get (pureref false) $ \\v -> pass"
        ]
