module Run
    ( runFiles
    , runInteractive
    ) where

import Changes.Core
import Pinafore
import Shapes
import System.Environment

runFiles :: Foldable t => (ContextOptions, ModuleOptions) -> Bool -> t (FilePath, [String]) -> IO ()
runFiles (copts, modopts) fNoRun scripts =
    runLifeCycleT $
    runNewView $
    for_ scripts $ \(fpath, iiScriptArguments) -> do
        let iiScriptName = fpath
        iiEnvironment <- liftIO getEnvironment
        context <- standardPinaforeContext copts MkInvocationInfo {..}
        action <- runWithContext context (standardFetchModule modopts) $ pinaforeInterpretFile fpath
        if fNoRun
            then return ()
            else action

runInteractive :: (ContextOptions, ModuleOptions) -> IO ()
runInteractive (copts, modopts) =
    runLifeCycleT $
    runNewView $ do
        let
            iiScriptName = ""
            iiScriptArguments = []
        iiEnvironment <- liftIO getEnvironment
        context <- standardPinaforeContext copts MkInvocationInfo {..}
        runWithContext context (standardFetchModule modopts) pinaforeInteract
