module Main
    ( main
    ) where

import Changes.Core
import Criterion.Main
import Pinafore
import Pinafore.Libs
import Pinafore.Test
import Shapes

nullViewIO :: View --> IO
nullViewIO va = runLifecycle $ runView va

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

getBenchEnv :: IO (() -> LibraryContext)
getBenchEnv = do
    (library, _) <-
        getLifeState $ do
            (ii, _) <- makeTestInvocationInfo stdout
            return $ mkLibraryContext ii $ libraryFetchModule $ fmap (contramap $ \_ -> ()) extraLibrary
    return $ \() -> library

benchScript :: Text -> Benchmark
benchScript text =
    env getBenchEnv $ \tpc -> let
        library = tpc ()
        in let
               ?library = library
               in bgroup
                      (show $ unpack text)
                      [ bench "check" $ nfIO $ fromInterpretResult $ qInterpretText "<test>" text >> return ()
                      , env (fmap const $ fromInterpretResult $ qInterpretText "<test>" text) $ \action ->
                            bench "run" $ nfIO (nullViewIO $ action ())
                      ]

benchScripts :: Benchmark
benchScripts =
    bgroup
        "script"
        [ benchScript "do a <- return $ return (); a end"
        , benchScript "do a <- get {return ()}; a end"
        , benchScript "do a <- get $ pureWholeModel $ return (); a end"
        , benchScript "get {return ()} >>= fn v => v"
        , benchScript "get {False} >>= fn v => return ()"
        , benchScript "get (pureWholeModel False) >>= fn v => return ()"
        , benchScript "let p = 3 in for_ [p,p,p,p, p,p,p,p, p,p,p,p, p,p,p,p ] $ fn v => return ()"
        , benchScript "let rec a=b; b=c; c=d; d=e; e=f; f=g; g=return () end in a"
        , benchScript "id $ id $ id $ id $ id $ id $ id $ id $ return ()"
        , benchScript
              "let import \"pinafore-gnome\"; using GTK; const = fns a b => a; ui_labelled = fns n ui => horizontal [label n, layoutGrow ui] in const (return ()) $ ui_labelled {\"Address: \"} $ ui_labelled {\"Address: \"} $ ui_labelled {\"Address: \"} $ ui_labelled {\"Address: \"} $ ui_labelled {\"Address: \"} GTK.blank"
        , benchScript "let const = fns a b => a; rec r = 3::r end in const (return ()) r"
        , benchScript
              "let cpass = fn x => return (); a = 3; b = [a,a,a,a,a,a,a,a]; c = [b,b,b,b,b,b,b,b]; d = [c,c,c,c,c,c,c,c] in cpass d"
        , benchScript
              "let cpass = fn x => return (); rec d = [c,c,c,c,c,c,c,c]; c = [b,b,b,b,b,b,b,b]; b = [a,a,a,a,a,a,a,a]; a = 3 end in cpass d"
        , benchScript
              "let cpass = fn x => return () in let a = 3 in let b = [a,a,a,a,a,a,a,a] in let c = [b,b,b,b,b,b,b,b] in let d = [c,c,c,c,c,c,c,c] in cpass d"
        , benchScript
              "let cpass = fn x => return () in let f = fn a => let b = [a,a,a,a,a,a,a,a] in let c = [b,b,b,b,b,b,b,b] in let d = [c,c,c,c,c,c,c,c] in d in cpass (f 3)"
        , benchScript $
          pack $
          "let g = fn r => get r >>= fn x => return (); q = [" <>
          intercalate "," (replicate 50 "g (pureWholeModel 1)") <> "] in for_ q id"
        , benchScript $
          pack $
          "let g1 = fn r => get r >>= fn x => return (); g2 = fn r => get r >>= fn x => return (); q = [" <>
          intercalate "," (replicate 25 "g1 (pureWholeModel 1)" <> replicate 25 "g2 (pureWholeModel 1)") <>
          "] in for_ q id"
        , benchScript $
          pack $
          "let g = fn r => get r >>= fn x => return () in let q = [" <>
          intercalate "," (replicate 50 "g (pureWholeModel 1)") <> "] in for_ q id"
        , benchScript $
          pack $
          "let g = fn r => get r >>= fn x => return () in let q = [" <>
          intercalate "," (fmap (\(i :: Int) -> "g (pureWholeModel " <> show i <> ")") [1 .. 50]) <> "] in for_ q id"
        , benchScript $
          pack $
          "let g = fn r => get r >>= fn x => return (); q = [" <>
          intercalate "," (replicate 50 "get (pureWholeModel 1) >>= fn x => return ()") <> "] in for_ q id"
        , benchScript $
          pack $
          "let g = fn r => list (return ()) (fns x y => return ()) r; q = [" <>
          intercalate "," (replicate 50 "g [1]") <> "] in for_ q id"
        ]

interpretUpdater :: Text -> IO ()
interpretUpdater text =
    runTester defaultTester $ do
        action <- testerLiftView $ qInterpretTextAtType "<test>" text
        (sendUpdate, ref) <- testerLiftAction action
        testerLiftView $
            runEditor (unWModel $ immutableModelToRejectingModel ref) $
            checkUpdateEditor (Known (1 :: Integer)) $ unliftActionOrFail sendUpdate

benchUpdate :: Text -> Benchmark
benchUpdate text =
    env (fmap const $ getLifeState $ makeTestInvocationInfo stdout) $ \tpc -> let
        ((pc, _), _) = tpc ()
        in let
               ?qcontext = pc
               in bench (show $ unpack text) $ nfIO $ interpretUpdater text

benchUpdates :: Benchmark
benchUpdates =
    bgroup
        "update"
        [ benchUpdate "do ref <- newMemWholeModel; return (ref := 1, ref) end"
        , benchUpdate
              "let id = fn x => x in do ref <- newMemWholeModel; return (ref := 1, id (id (id (id (id (id (id (id (id (id (ref))))))))))) end"
        , benchUpdate
              "let id = fn x => x in do ref <- newMemWholeModel; return (ref := 1, id $ id $ id $ id $ id $ id $ id $ id $ id $ id $ ref) end"
        ]

main :: IO ()
main = defaultMain [benchHashes, benchScripts, benchUpdates]
