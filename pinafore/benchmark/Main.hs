module Main
    ( main
    ) where

import Criterion.Main
import Pinafore
import Pinafore.Test
import Shapes

benchHash :: Text -> Benchmark
benchHash text = bench (show $ unpack text) $ nf literalToEntity text

benchHashes :: Benchmark
benchHashes =
    bgroup
        "hash"
        [ benchHash ""
        , benchHash "1"
        , benchHash "1234567890"
        , benchHash
              "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
        ]

benchScript :: Text -> Benchmark
benchScript text =
    env (fmap const makeTestPinaforeContext) $ \tpc -> let
        (pc, _) = tpc ()
        in bgroup
               (show $ unpack text)
               [ bench "check" $ nfIO $ pinaforeInterpretFile pc "<test>" text >> return ()
               , env (fmap const $ pinaforeInterpretFile pc "<test>" text) $ \action -> bench "run" $ nfIO (action ())
               ]

benchScripts :: Benchmark
benchScripts =
    bgroup
        "script"
        [ benchScript "runref {pass}"
        , benchScript "runref $ pureref pass"
        , benchScript "get {pass} $ \\v -> v"
        , benchScript "get {false} $ \\v -> pass"
        , benchScript "get (pureref false) $ \\v -> pass"
        , benchScript "let p = 3 in for [p,p,p,p, p,p,p,p, p,p,p,p, p,p,p,p ] $ \\v -> pass"
        , benchScript "let a=b; b=c; c=d; d=e; e=f; f=g; g=pass in a"
        , benchScript "id $ id $ id $ id $ id $ id $ id $ id pass"
        , benchScript
              "let const a b = a; ui_labelled n ui = ui_horizontal [(ui_label n,false),(ui,true)] in const pass $ ui_labelled {\"Address: \"} $ ui_labelled {\"Address: \"} $ ui_labelled {\"Address: \"} $ ui_labelled {\"Address: \"} $ ui_labelled {\"Address: \"} ui_blank"
        , benchScript "let const a b = a; r = 3:r in const pass r"
        , benchScript
              "let cpass x = pass; a = 3; b = [a,a,a,a,a,a,a,a]; c = [b,b,b,b,b,b,b,b]; d = [c,c,c,c,c,c,c,c] in cpass d"
        , benchScript
              "let cpass x = pass; d = [c,c,c,c,c,c,c,c]; c = [b,b,b,b,b,b,b,b]; b = [a,a,a,a,a,a,a,a]; a = 3 in cpass d"
        , benchScript
              "let cpass x = pass in let a = 3 in let b = [a,a,a,a,a,a,a,a] in let c = [b,b,b,b,b,b,b,b] in let d = [c,c,c,c,c,c,c,c] in cpass d"
        , benchScript
              "let cpass x = pass in let f = \\a -> let b = [a,a,a,a,a,a,a,a] in let c = [b,b,b,b,b,b,b,b] in let d = [c,c,c,c,c,c,c,c] in d in cpass (f 3)"
        , benchScript $
          pack $
          "let g r = get r $ \\x -> pass; q = [" <> intercalate "," (replicate 50 "g (pureref 1)") <> "] in for q id"
        , benchScript $
          pack $
          "let g1 r = get r $ \\x -> pass; g2 r = get r $ \\x -> pass; q = [" <>
          intercalate "," (replicate 25 "g1 (pureref 1)" <> replicate 25 "g2 (pureref 1)") <> "] in for q id"
        , benchScript $
          pack $
          "let g r = get r $ \\x -> pass in let q = [" <>
          intercalate "," (replicate 50 "g (pureref 1)") <> "] in for q id"
        , benchScript $
          pack $
          "let g r = get r $ \\x -> pass in let q = [" <>
          intercalate "," (fmap (\(i :: Int) -> "g (pureref " <> show i <> ")") [1 .. 50]) <> "] in for q id"
        , benchScript $
          pack $
          "let g r = get r $ \\x -> pass; q = [" <>
          intercalate "," (replicate 50 "get (pureref 1) $ \\x -> pass") <> "] in for q id"
        , benchScript $
          pack $
          "let g r = list pass (\\x y -> pass) r; q = [" <> intercalate "," (replicate 50 "g [1]") <> "] in for q id"
        ]

main :: IO ()
main = defaultMain [benchHashes, benchScripts]
