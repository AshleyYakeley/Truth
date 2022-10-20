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
    runLifecycle $
    runNewView $
    for_ scripts $ \(fpath, iiScriptArguments) -> do
        let
            iiScriptName = fpath
            iiStdIn = stdinTextSource
            iiStdOut = stdoutTextSink
            iiStdErr = stderrTextSink
        iiEnvironment <- liftIO getEnvironment
        context <- standardQContext copts MkInvocationInfo {..}
        action <- runWithContext context (standardFetchModule modopts) $ qInterpretFile fpath
        if fNoRun
            then return ()
            else action

runInteractive :: (ContextOptions, ModuleOptions) -> IO ()
runInteractive (copts, modopts) =
    runLifecycle $
    runNewView $ do
        let
            iiScriptName = ""
            iiScriptArguments = []
            iiStdIn = stdinTextSource
            iiStdOut = stdoutTextSink
            iiStdErr = stderrTextSink
        iiEnvironment <- liftIO getEnvironment
        context <- standardQContext copts MkInvocationInfo {..}
        runWithContext context (standardFetchModule modopts) qInteract
