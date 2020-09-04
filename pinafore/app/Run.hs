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
    truthMainGTK $ \tc -> do
        context <- standardPinaforeContext dirpath tc
        cvLiftView $
            for_ fpaths $ \fpath -> do
                ptext <- liftIO $ readFile fpath
                action <-
                    throwResult $ let
                        ?pinafore = context
                        in pinaforeInterpretFile fpath $ decodeUtf8 $ toStrict ptext
                if fNoRun
                    then return ()
                    else action

runInteractive :: FilePath -> IO ()
runInteractive dirpath =
    truthMainGTK $ \tc -> do
        context <- standardPinaforeContext dirpath tc
        let
            ?pinafore = context
            in cvLiftView pinaforeInteract
        cvLiftView viewExit
