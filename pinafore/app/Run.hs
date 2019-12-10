module Run
    ( runFiles
    , runInteractive
    ) where

import Pinafore
import Shapes
import Truth.Core
import Truth.UI.GTK

runFiles :: UpdateTiming -> Bool -> FilePath -> [FilePath] -> IO ()
runFiles ut fNoRun dirpath fpaths =
    truthMainGTK $ \MkTruthContext {..} -> do
        (toolkit, checkdone) <- liftIO $ quitOnWindowsClosed tcUIToolkit
        context <- standardPinaforeContext ut dirpath toolkit
        for_ fpaths $ \fpath ->
            liftIO $ do
                ptext <- readFile fpath
                action <-
                    ioRunInterpretResult $ let
                        ?pinafore = context
                        in pinaforeInterpretFile fpath $ decodeUtf8 $ toStrict ptext
                if fNoRun
                    then return ()
                    else action
        liftIO checkdone

runInteractive :: UpdateTiming -> FilePath -> IO ()
runInteractive ut dirpath =
    truthMainGTK $ \MkTruthContext {..} -> do
        context <- standardPinaforeContext ut dirpath tcUIToolkit
        let
            ?pinafore = context
            in liftIO pinaforeInteract
        liftIO $ uitExit tcUIToolkit
