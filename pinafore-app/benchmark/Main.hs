module Main
    ( main
    ) where

import Changes.Core
import Criterion.Main
import Pinafore
import Pinafore.Language.Library.GTK
import Pinafore.Test
import Shapes

nullViewIO :: View a -> IO a
nullViewIO = ccRunView (nullChangesContext runLifeCycle) emptyResourceContext

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
    env (fmap const $ getInnerLifeState $ makeTestPinaforeContext (nullChangesContext runLifeCycle) stdout) $ \tpc -> let
        ((pc, _), _) = tpc ()
        in bgroup
               (show $ unpack text)
               [ bench "check" $
                 nfIO $
                 runWithContext pc (libraryFetchModule gtkLibrary) $
                 throwInterpretResult $ pinaforeInterpretText "<test>" text >> return ()
               , env (fmap const $
                      runWithContext pc (libraryFetchModule gtkLibrary) $
                      throwInterpretResult $ pinaforeInterpretText "<test>" text) $ \action ->
                     bench "run" $ nfIO (nullViewIO $ action ())
               ]

benchScripts :: Benchmark
benchScripts =
    bgroup
        "script"
        [ benchScript "do a <- return $ return (); a end"
        , benchScript "do a <- get {return ()}; a end"
        , benchScript "do a <- get $ pureWhole $ return (); a end"
        , benchScript "get {return ()} >>= \\v -> v"
        , benchScript "get {False} >>= \\v -> return ()"
        , benchScript "get (pureWhole False) >>= \\v -> return ()"
        , benchScript "let p = 3 in for_ [p,p,p,p, p,p,p,p, p,p,p,p, p,p,p,p ] $ \\v -> return ()"
        , benchScript "let rec a=b; b=c; c=d; d=e; e=f; f=g; g=return () end in a"
        , benchScript "id $ id $ id $ id $ id $ id $ id $ id $ return ()"
        , benchScript
              "let const a b = a; ui_labelled n ui = UI.horizontal [(False,UI.label n),(True,ui)] in const (return ()) $ ui_labelled {\"Address: \"} $ ui_labelled {\"Address: \"} $ ui_labelled {\"Address: \"} $ ui_labelled {\"Address: \"} $ ui_labelled {\"Address: \"} UI.blank"
        , benchScript "let const a b = a; rec r = 3::r end in const (return ()) r"
        , benchScript
              "let cpass x = return (); a = 3; b = [a,a,a,a,a,a,a,a]; c = [b,b,b,b,b,b,b,b]; d = [c,c,c,c,c,c,c,c] in cpass d"
        , benchScript
              "let cpass x = return (); rec d = [c,c,c,c,c,c,c,c]; c = [b,b,b,b,b,b,b,b]; b = [a,a,a,a,a,a,a,a]; a = 3 end in cpass d"
        , benchScript
              "let cpass x = return () in let a = 3 in let b = [a,a,a,a,a,a,a,a] in let c = [b,b,b,b,b,b,b,b] in let d = [c,c,c,c,c,c,c,c] in cpass d"
        , benchScript
              "let cpass x = return () in let f = \\a -> let b = [a,a,a,a,a,a,a,a] in let c = [b,b,b,b,b,b,b,b] in let d = [c,c,c,c,c,c,c,c] in d in cpass (f 3)"
        , benchScript $
          pack $
          "let g r = get r >>= \\x -> return (); q = [" <>
          intercalate "," (replicate 50 "g (pureWhole 1)") <> "] in for_ q id"
        , benchScript $
          pack $
          "let g1 r = get r >>= \\x -> return (); g2 r = get r >>= \\x -> return (); q = [" <>
          intercalate "," (replicate 25 "g1 (pureWhole 1)" <> replicate 25 "g2 (pureWhole 1)") <> "] in for_ q id"
        , benchScript $
          pack $
          "let g r = get r >>= \\x -> return () in let q = [" <>
          intercalate "," (replicate 50 "g (pureWhole 1)") <> "] in for_ q id"
        , benchScript $
          pack $
          "let g r = get r >>= \\x -> return () in let q = [" <>
          intercalate "," (fmap (\(i :: Int) -> "g (pureWhole " <> show i <> ")") [1 .. 50]) <> "] in for_ q id"
        , benchScript $
          pack $
          "let g r = get r >>= \\x -> return (); q = [" <>
          intercalate "," (replicate 50 "get (pureWhole 1) >>= \\x -> return ()") <> "] in for_ q id"
        , benchScript $
          pack $
          "let g r = list (return ()) (\\x y -> return ()) r; q = [" <>
          intercalate "," (replicate 50 "g [1]") <> "] in for_ q id"
        ]

interpretUpdater :: (?pinafore :: PinaforeContext) => Text -> IO ()
interpretUpdater text =
    withTestPinaforeContext mempty stdout $ \tc unlift _getTableState -> do
        action <- throwInterpretResult $ pinaforeInterpretTextAtType "<test>" text
        (sendUpdate, ref) <-
            ccUnliftLifeCycle tc $ ccRunView tc emptyResourceContext $ unliftPinaforeActionOrFail action
        unlift $
            runEditor emptyResourceContext (unWModel $ immutableRefToRejectingRef ref) $
            checkUpdateEditor (Known (1 :: Integer)) $
            ccUnliftLifeCycle tc $ ccRunView tc emptyResourceContext $ unliftPinaforeActionOrFail sendUpdate

benchUpdate :: Text -> Benchmark
benchUpdate text =
    env (fmap const $ getInnerLifeState $ makeTestPinaforeContext (nullChangesContext runLifeCycle) stdout) $ \tpc -> let
        ((pc, _), _) = tpc ()
        in let
               ?pinafore = pc
               in bench (show $ unpack text) $ nfIO $ interpretUpdater text

benchUpdates :: Benchmark
benchUpdates =
    bgroup
        "update"
        [ benchUpdate "do ref <- newMemWhole; return (ref := 1, ref) end"
        , benchUpdate
              "let id x = x in do ref <- newMemWhole; return (ref := 1, id (id (id (id (id (id (id (id (id (id (ref))))))))))) end"
        , benchUpdate
              "let id x = x in do ref <- newMemWhole; return (ref := 1, id $ id $ id $ id $ id $ id $ id $ id $ id $ id $ ref) end"
        ]

main :: IO ()
main = defaultMain [benchHashes, benchScripts, benchUpdates]
