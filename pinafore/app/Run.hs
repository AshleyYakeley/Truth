module Run
    ( runFiles
    , runInteractive
    ) where

import Changes.Core
import Changes.UI.GTK
import Pinafore
import Shapes

runFiles :: Bool -> FilePath -> [FilePath] -> IO ()
runFiles fNoRun dirpath fpaths =
    changesMainGTK $ \tc -> do
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
    changesMainGTK $ \tc -> do
        context <- standardPinaforeContext dirpath tc
        let
            ?pinafore = context
            in cvLiftView pinaforeInteract
        cvLiftView viewExit
