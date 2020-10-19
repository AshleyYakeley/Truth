module Run
    ( runFiles
    , runInteractive
    ) where

import Changes.Core
import Changes.UI.GTK
import Pinafore
import Shapes
import System.Environment

runFiles :: Foldable t => Bool -> FilePath -> t (FilePath, [String]) -> IO ()
runFiles fNoRun dirpath scripts =
    changesMainGTK $ \tc ->
        for_ scripts $ \(fpath, iiScriptArguments) -> do
            let iiScriptName = fpath
            iiEnvironment <- liftIO getEnvironment
            context <- standardPinaforeContext MkInvocationInfo {..} dirpath tc
            cvLiftView $ do
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
        let
            iiScriptName = ""
            iiScriptArguments = []
        iiEnvironment <- liftIO getEnvironment
        context <- standardPinaforeContext MkInvocationInfo {..} dirpath tc
        let
            ?pinafore = context
            in cvLiftView pinaforeInteract
        cvLiftView viewExit
