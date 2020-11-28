module Run
    ( runFiles
    , runInteractive
    ) where

import Changes.Core
import Changes.UI.GTK
import Pinafore
import Shapes
import System.Environment

runFiles :: Foldable t => Bool -> [FilePath] -> FilePath -> t (FilePath, [String]) -> IO ()
runFiles fNoRun includeDirs dirpath scripts =
    changesMainGTK $ \tc ->
        for_ scripts $ \(fpath, iiScriptArguments) -> do
            let iiScriptName = fpath
            iiEnvironment <- liftIO getEnvironment
            context <- standardPinaforeContext includeDirs MkInvocationInfo {..} dirpath tc
            liftToLifeCycle $ do
                action <- let
                    ?pinafore = context
                    in pinaforeInterpretFile fpath
                if fNoRun
                    then return ()
                    else action

runInteractive :: [FilePath] -> FilePath -> IO ()
runInteractive includeDirs dirpath =
    changesMainGTK $ \tc -> do
        let
            iiScriptName = ""
            iiScriptArguments = []
        iiEnvironment <- liftIO getEnvironment
        context <- standardPinaforeContext includeDirs MkInvocationInfo {..} dirpath tc
        let
            ?pinafore = context
            in liftToLifeCycle pinaforeInteract
        liftToLifeCycle viewExit
