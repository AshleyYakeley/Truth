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
    env (fmap const (runLifeCycle makeTestPinaforeContext)) $ \tpc -> let
        ((pc, _), _) = tpc ()
        in let
               ?pinafore = pc
               in bgroup
                      (show $ unpack text)
                      [ bench "check" $ nfIO $ pinaforeInterpretFile "<test>" text >> return ()
                      , env (fmap const $ pinaforeInterpretFile "<test>" text) $ \action ->
                            bench "run" $ nfIO (action ())
                      ]

benchScripts :: Benchmark
benchScripts =
    bgroup
        "script"
        [ benchScript "runref {return ()}"
        , benchScript "runref $ pureref $ return ()"
        , benchScript "get {return ()} >>= \\v -> v"
        , benchScript "get {False} >>= \\v -> return ()"
        , benchScript "get (pureref False) >>= \\v -> return ()"
        , benchScript "let p = 3 in for_ [p,p,p,p, p,p,p,p, p,p,p,p, p,p,p,p ] $ \\v -> return ()"
        , benchScript "let a=b; b=c; c=d; d=e; e=f; f=g; g=return () in a"
        , benchScript "id $ id $ id $ id $ id $ id $ id $ id $ return ()"
        , benchScript
              "let const a b = a; ui_labelled n ui = ui_horizontal [(ui_label n,False),(ui,True)] in const (return ()) $ ui_labelled {\"Address: \"} $ ui_labelled {\"Address: \"} $ ui_labelled {\"Address: \"} $ ui_labelled {\"Address: \"} $ ui_labelled {\"Address: \"} ui_blank"
        , benchScript "let const a b = a; r = 3:r in const (return ()) r"
        , benchScript
              "let cpass x = return (); a = 3; b = [a,a,a,a,a,a,a,a]; c = [b,b,b,b,b,b,b,b]; d = [c,c,c,c,c,c,c,c] in cpass d"
        , benchScript
              "let cpass x = return (); d = [c,c,c,c,c,c,c,c]; c = [b,b,b,b,b,b,b,b]; b = [a,a,a,a,a,a,a,a]; a = 3 in cpass d"
        , benchScript
              "let cpass x = return () in let a = 3 in let b = [a,a,a,a,a,a,a,a] in let c = [b,b,b,b,b,b,b,b] in let d = [c,c,c,c,c,c,c,c] in cpass d"
        , benchScript
              "let cpass x = return () in let f = \\a -> let b = [a,a,a,a,a,a,a,a] in let c = [b,b,b,b,b,b,b,b] in let d = [c,c,c,c,c,c,c,c] in d in cpass (f 3)"
        , benchScript $
          pack $
          "let g r = get r >>= \\x -> return (); q = [" <>
          intercalate "," (replicate 50 "g (pureref 1)") <> "] in for_ q id"
        , benchScript $
          pack $
          "let g1 r = get r >>= \\x -> return (); g2 r = get r >>= \\x -> return (); q = [" <>
          intercalate "," (replicate 25 "g1 (pureref 1)" <> replicate 25 "g2 (pureref 1)") <> "] in for_ q id"
        , benchScript $
          pack $
          "let g r = get r >>= \\x -> return () in let q = [" <>
          intercalate "," (replicate 50 "g (pureref 1)") <> "] in for_ q id"
        , benchScript $
          pack $
          "let g r = get r >>= \\x -> return () in let q = [" <>
          intercalate "," (fmap (\(i :: Int) -> "g (pureref " <> show i <> ")") [1 .. 50]) <> "] in for_ q id"
        , benchScript $
          pack $
          "let g r = get r >>= \\x -> return (); q = [" <>
          intercalate "," (replicate 50 "get (pureref 1) >>= \\x -> return ()") <> "] in for_ q id"
        , benchScript $
          pack $
          "let g r = list (return ()) (\\x y -> return ()) r; q = [" <>
          intercalate "," (replicate 50 "g [1]") <> "] in for_ q id"
        ]

main :: IO ()
main = defaultMain [benchHashes, benchScripts]
