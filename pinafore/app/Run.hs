module Run
    ( runFiles
    , runInteractive
    ) where

import Pinafore
import Shapes
import Truth.Core
import Truth.UI.GTK

runFiles :: Bool -> FilePath -> [FilePath] -> IO ()
runFiles fNoRun dirpath fpaths =
    truthMainGTK $ \MkTruthContext {..} -> do
        (toolkit, checkdone) <- liftIO $ quitOnWindowsClosed tcUIToolkit
        context <- standardPinaforeContext dirpath toolkit
        cvLiftView $
            for_ fpaths $ \fpath -> do
                ptext <- liftIO $ readFile fpath
                action <-
                    ioRunInterpretResult $ let
                        ?pinafore = context
                        in pinaforeInterpretFile fpath $ decodeUtf8 $ toStrict ptext
                if fNoRun
                    then return ()
                    else action
        liftIO checkdone

runInteractive :: FilePath -> IO ()
runInteractive dirpath =
    truthMainGTK $ \MkTruthContext {..} -> do
        context <- standardPinaforeContext dirpath tcUIToolkit
        let
            ?pinafore = context
            in cvLiftView pinaforeInteract
        liftIO $ uitExit tcUIToolkit
