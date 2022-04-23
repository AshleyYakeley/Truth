module Run
    ( runFiles
    , runInteractive
    ) where

import Changes.Core
import Changes.UI.GTK
import Pinafore
import Shapes
import System.Environment

runFiles :: Foldable t => (ContextOptions, ModuleOptions) -> Bool -> t (FilePath, [String]) -> IO ()
runFiles (copts, modopts) fNoRun scripts =
    changesMainGTK $ \cc ->
        for_ scripts $ \(fpath, iiScriptArguments) -> do
            let iiScriptName = fpath
            iiEnvironment <- liftIO getEnvironment
            context <- standardPinaforeContext copts MkInvocationInfo {..} cc
            action <- runWithContext context (standardFetchModule modopts) $ pinaforeInterpretFile fpath
            if fNoRun
                then return ()
                else action

runInteractive :: (ContextOptions, ModuleOptions) -> IO ()
runInteractive (copts, modopts) =
    changesMainGTK $ \cc -> do
        let
            iiScriptName = ""
            iiScriptArguments = []
        iiEnvironment <- liftIO getEnvironment
        context <- standardPinaforeContext copts MkInvocationInfo {..} cc
        runWithContext context (standardFetchModule modopts) pinaforeInteract
        viewExitUI
