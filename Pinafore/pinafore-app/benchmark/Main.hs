module Main
    ( main
    ) where

import Changes.Core
import Criterion.Main
import qualified Paths_pinafore_lib_script
import Pinafore.Libs
import Pinafore.Main
import Pinafore.Test.Internal
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

getBenchEnv :: IO (() -> InterpretBehaviour)
getBenchEnv = let
    ibLoadModule = libraryLoadModule appLibrary
    ibSloppy = False
    in return $ \() -> MkInterpretBehaviour {..}

benchScript :: Text -> Benchmark
benchScript text =
    env getBenchEnv $ \tpc -> let
        behaviour = tpc ()
        in let
               ?behaviour = behaviour
               in bgroup
                      (show $ unpack text)
                      [ bench "check" $
                        nfIO $ fromInterpretResult $ qInterpretScriptText "<test>" text [] [] >> return ()
                      , env (fmap const $ fromInterpretResult $ qInterpretScriptText "<test>" text [] []) $ \action ->
                            bench "run" $ nfIO (nullViewIO $ action ())
                      ]

benchScripts :: Benchmark
benchScripts =
    bgroup
        "script"
        [ benchScript "do {a <- pure $ pure (); a}"
        , benchScript "do {a <- get ap{pure.Action ()}; a}"
        , benchScript "do {a <- get $ pure.WholeModel $ pure (); a}"
        , benchScript "get ap{pure.Action ()} >>= fn v => v"
        , benchScript "get ap{False} >>= fn v => pure ()"
        , benchScript "get (pure.WholeModel False) >>= fn v => pure ()"
        , benchScript "let {p = 3} for_ [p,p,p,p, p,p,p,p, p,p,p,p, p,p,p,p ] $ fn v => pure ()"
        , benchScript "let rec {a=b; b=c; c=d; d=e; e=f; f=g; g=pure ()} a"
        , benchScript "id $ id $ id $ id $ id $ id $ id $ id $ pure ()"
        , benchScript
              "import \"gnome\" with GTK, Widget.GTK let {const = fn a, b => a; ui_labelled = fn n, ui => horizontal [label n, grow.Layout ui]} const (pure ()) $ ui_labelled ap{\"Address: \"} $ ui_labelled ap{\"Address: \"} $ ui_labelled ap{\"Address: \"} $ ui_labelled ap{\"Address: \"} $ ui_labelled ap{\"Address: \"} blank.Widget.GTK"
        , benchScript "let {const = fn a, b => a; let rec {r = 3::r}} const (pure ()) r"
        , benchScript
              "let {cpass = fn x => pure (); a = 3; b = [a,a,a,a,a,a,a,a]; c = [b,b,b,b,b,b,b,b]; d = [c,c,c,c,c,c,c,c]} cpass d"
        , benchScript
              "let {cpass = fn x => pure (); let rec {d = [c,c,c,c,c,c,c,c]; c = [b,b,b,b,b,b,b,b]; b = [a,a,a,a,a,a,a,a]; a = 3}} cpass d"
        , benchScript
              "let {cpass = fn x => pure ()} let {a = 3} let {b = [a,a,a,a,a,a,a,a]} let {c = [b,b,b,b,b,b,b,b]} let {d = [c,c,c,c,c,c,c,c]} cpass d"
        , benchScript
              "let {cpass = fn x => pure ()} let {f = fn a => let {b = [a,a,a,a,a,a,a,a]} let {c = [b,b,b,b,b,b,b,b]} let {d = [c,c,c,c,c,c,c,c]} d} cpass (f 3)"
        , benchScript $
          pack $
          "let {g = fn r => get r >>= fn x => pure (); q = [" <>
          intercalate "," (replicate 50 "g (pure.WholeModel 1)") <> "]} for_ q id"
        , benchScript $
          pack $
          "let {g1 = fn r => get r >>= fn x => pure (); g2 = fn r => get r >>= fn x => pure (); q = [" <>
          intercalate "," (replicate 25 "g1 (pure.WholeModel 1)" <> replicate 25 "g2 (pure.WholeModel 1)") <>
          "]} for_ q id"
        , benchScript $
          pack $
          "let {g = fn r => get r >>= fn x => pure ()} let {q = [" <>
          intercalate "," (replicate 50 "g (pure.WholeModel 1)") <> "]} for_ q id"
        , benchScript $
          pack $
          "let {g = fn r => get r >>= fn x => pure ()} let {q = [" <>
          intercalate "," (fmap (\(i :: Int) -> "g (pure.WholeModel " <> show i <> ")") [1 .. 50]) <> "]} for_ q id"
        , benchScript $
          pack $
          "let {g = fn r => get r >>= fn x => pure (); q = [" <>
          intercalate "," (replicate 50 "get (pure.WholeModel 1) >>= fn x => pure ()") <> "]} for_ q id"
        , benchScript $
          pack $
          "let {g = fn r => from.List (pure ()) (fn x, y => pure ()) r; q = [" <>
          intercalate "," (replicate 50 "g [1]") <> "]} for_ q id"
        ]

interpretUpdater :: Text -> IO ()
interpretUpdater text =
    runTester defaultTester $ do
        action <- testerLiftView $ qInterpretTextAtType "<test>" text [] []
        (sendUpdate, ref) <- testerLiftAction action
        testerLiftView $
            runEditor (unWModel $ immutableModelToRejectingModel ref) $
            checkUpdateEditor (Known (1 :: Integer)) $ unliftActionOrFail sendUpdate

benchUpdate :: Text -> Benchmark
benchUpdate text = bench (show $ unpack text) $ nfIO $ interpretUpdater text

benchUpdates :: Benchmark
benchUpdates =
    bgroup
        "update"
        [ benchUpdate "do {ref <- newMem.WholeModel; pure (ref := 1, ref)}"
        , benchUpdate
              "let {id = fn x => x} do {ref <- newMem.WholeModel; pure (ref := 1, id (id (id (id (id (id (id (id (id (id (ref)))))))))))}"
        , benchUpdate
              "let {id = fn x => x} do {ref <- newMem.WholeModel; pure (ref := 1, id $ id $ id $ id $ id $ id $ id $ id $ id $ id $ ref)}"
        ]

benchInterpretFile :: FilePath -> Benchmark
benchInterpretFile fpath =
    bench fpath $
    nfIO $ do
        scriptLibDir <- Paths_pinafore_lib_script.getDataDir
        runTester defaultTester {tstLibrary = appLibrary} $
            testerLoad (directoryLoadModule scriptLibDir) $ do
                _ <- testerInterpretScriptFile fpath []
                return ()

benchFiles :: Benchmark
benchFiles =
    bgroup
        "file"
        [ benchInterpretFile "examples/events"
        , benchInterpretFile "examples/contacts"
        , benchInterpretFile "examples/clock"
        ]

main :: IO ()
main = runWithOptions defaultExecutionOptions $ defaultMain [benchHashes, benchScripts, benchUpdates, benchFiles]
