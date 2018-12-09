module Main
    ( main
    ) where

import Criterion.Main
import Pinafore
import Pinafore.Test
import Shapes

benchmarkText :: Text -> Benchmark
benchmarkText text =
    env (fmap const makeTestPinaforeContext) $ \tpc -> let
        (pc, _) = tpc ()
        in bgroup
               (show $ unpack text)
               [ bench "check" $ nfIO $ pinaforeInterpretFile pc "<test>" text >> return ()
               , env (fmap const $ pinaforeInterpretFile pc "<test>" text) $ \action -> bench "run" $ nfIO (action ())
               ]

main :: IO ()
main =
    defaultMain
        [ benchmarkText "runref {pass}"
        , benchmarkText "runref $ pureref pass"
        , benchmarkText "get {pass} $ \\v -> v"
        , benchmarkText "get {false} $ \\v -> pass"
        , benchmarkText "get (pureref false) $ \\v -> pass"
        , benchmarkText "let p = 3 in for [p,p,p,p, p,p,p,p, p,p,p,p, p,p,p,p ] $ \\v -> pass"
        , benchmarkText "let a=b; b=c; c=d; d=e; e=f; f=g; g=pass in a"
        , benchmarkText "id $ id $ id $ id $ id $ id $ id $ id pass"
        , benchmarkText
              "let const a b = a; ui_labelled n ui = ui_horizontal [(ui_label n,false),(ui,true)] in const pass $ ui_labelled {\"Address: \"} $ ui_labelled {\"Address: \"} $ ui_labelled {\"Address: \"} $ ui_labelled {\"Address: \"} $ ui_labelled {\"Address: \"} ui_blank"
        , benchmarkText "let const a b = a; r = 3:r in const pass r"
        , benchmarkText
              "let cpass x = pass; a = 3; b = [a,a,a,a,a,a,a,a]; c = [b,b,b,b,b,b,b,b]; d = [c,c,c,c,c,c,c,c] in cpass d"
        , benchmarkText
              "let cpass x = pass; d = [c,c,c,c,c,c,c,c]; c = [b,b,b,b,b,b,b,b]; b = [a,a,a,a,a,a,a,a]; a = 3 in cpass d"
        , benchmarkText
              "let cpass x = pass in let a = 3 in let b = [a,a,a,a,a,a,a,a] in let c = [b,b,b,b,b,b,b,b] in let d = [c,c,c,c,c,c,c,c] in cpass d"
        , benchmarkText
              "let cpass x = pass in let f = \\a -> let b = [a,a,a,a,a,a,a,a] in let c = [b,b,b,b,b,b,b,b] in let d = [c,c,c,c,c,c,c,c] in d in cpass (f 3)"
        ]
