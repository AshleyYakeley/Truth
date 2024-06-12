module Main
    ( main
    ) where

import Changes.Core
import Criterion.Main
import Paths_pinafore_lib_stdlib
import Pinafore.Libs
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

getBenchEnv :: IO (() -> LibraryContext)
getBenchEnv = do
    (library, _) <-
        getLifeState $ do
            (ii, _) <- makeTestInvocationInfo stdout
            return $ mkLibraryContext ii (libraryLoadModule () extraLibrary)
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
        [ benchScript "do a <- pure $ pure (); a end"
        , benchScript "do a <- get {pure.Action ()}; a end"
        , benchScript "do a <- get $ pure.WholeModel $ pure (); a end"
        , benchScript "get {pure.Action ()} >>= fn v => v"
        , benchScript "get {False} >>= fn v => pure ()"
        , benchScript "get (pure.WholeModel False) >>= fn v => pure ()"
        , benchScript "let p = 3 in for_ [p,p,p,p, p,p,p,p, p,p,p,p, p,p,p,p ] $ fn v => pure ()"
        , benchScript "let rec a=b; b=c; c=d; d=e; e=f; f=g; g=pure () in a"
        , benchScript "id $ id $ id $ id $ id $ id $ id $ id $ pure ()"
        , benchScript
              "import \"gnome\" in with GTK, Widget.GTK in let const = fn a, b => a; ui_labelled = fn n, ui => horizontal [label n, layoutGrow ui] in const (pure ()) $ ui_labelled {\"Address: \"} $ ui_labelled {\"Address: \"} $ ui_labelled {\"Address: \"} $ ui_labelled {\"Address: \"} $ ui_labelled {\"Address: \"} blank.Widget.GTK"
        , benchScript "let const = fn a, b => a; let rec r = 3::r end in const (pure ()) r"
        , benchScript
              "let cpass = fn x => pure (); a = 3; b = [a,a,a,a,a,a,a,a]; c = [b,b,b,b,b,b,b,b]; d = [c,c,c,c,c,c,c,c] in cpass d"
        , benchScript
              "let cpass = fn x => pure (); let rec d = [c,c,c,c,c,c,c,c]; c = [b,b,b,b,b,b,b,b]; b = [a,a,a,a,a,a,a,a]; a = 3 end in cpass d"
        , benchScript
              "let cpass = fn x => pure () in let a = 3 in let b = [a,a,a,a,a,a,a,a] in let c = [b,b,b,b,b,b,b,b] in let d = [c,c,c,c,c,c,c,c] in cpass d"
        , benchScript
              "let cpass = fn x => pure () in let f = fn a => let b = [a,a,a,a,a,a,a,a] in let c = [b,b,b,b,b,b,b,b] in let d = [c,c,c,c,c,c,c,c] in d in cpass (f 3)"
        , benchScript $
          pack $
          "let g = fn r => get r >>= fn x => pure (); q = [" <>
          intercalate "," (replicate 50 "g (pure.WholeModel 1)") <> "] in for_ q id"
        , benchScript $
          pack $
          "let g1 = fn r => get r >>= fn x => pure (); g2 = fn r => get r >>= fn x => pure (); q = [" <>
          intercalate "," (replicate 25 "g1 (pure.WholeModel 1)" <> replicate 25 "g2 (pure.WholeModel 1)") <>
          "] in for_ q id"
        , benchScript $
          pack $
          "let g = fn r => get r >>= fn x => pure () in let q = [" <>
          intercalate "," (replicate 50 "g (pure.WholeModel 1)") <> "] in for_ q id"
        , benchScript $
          pack $
          "let g = fn r => get r >>= fn x => pure () in let q = [" <>
          intercalate "," (fmap (\(i :: Int) -> "g (pure.WholeModel " <> show i <> ")") [1 .. 50]) <> "] in for_ q id"
        , benchScript $
          pack $
          "let g = fn r => get r >>= fn x => pure (); q = [" <>
          intercalate "," (replicate 50 "get (pure.WholeModel 1) >>= fn x => pure ()") <> "] in for_ q id"
        , benchScript $
          pack $
          "let g = fn r => from.List (pure ()) (fn x, y => pure ()) r; q = [" <>
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
        [ benchUpdate "do ref <- newMem.WholeModel; pure (ref := 1, ref) end"
        , benchUpdate
              "let id = fn x => x in do ref <- newMem.WholeModel; pure (ref := 1, id (id (id (id (id (id (id (id (id (id (ref))))))))))) end"
        , benchUpdate
              "let id = fn x => x in do ref <- newMem.WholeModel; pure (ref := 1, id $ id $ id $ id $ id $ id $ id $ id $ id $ id $ ref) end"
        ]

benchInterpretFile :: FilePath -> Benchmark
benchInterpretFile fpath =
    bench fpath $
    nfIO $ do
        libDir <- getDataDir
        runTester defaultTester $
            testerLoadLibrary extraLibrary $
            testerLoad (directoryLoadModule libDir) $
            testerLiftView $ do
                _ <- qInterpretFile fpath
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
main = defaultMain [benchHashes, benchScripts, benchUpdates, benchFiles]
