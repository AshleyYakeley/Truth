module Run
    ( runFiles
    , runInteractive
    ) where

import Changes.Core
import Pinafore.Main
import Shapes
import System.Environment

runFiles :: Foldable t => (StorageModelOptions, ModuleOptions) -> Bool -> t (FilePath, [String]) -> IO ()
runFiles (smopts, modopts) fNoRun scripts =
    runLifecycle $
    runView $
    for_ scripts $ \(fpath, iiScriptArguments) -> do
        let
            iiScriptName = fpath
            iiStdIn = stdinTextSource
            iiStdOut = stdoutTextSink
            iiStdErr = stderrTextSink
            iiDefaultStorageModel = standardStorageModel smopts
        iiEnvironment <- liftIO getEnvironment
        let ?library = standardLibraryContext MkInvocationInfo {..} modopts
        action <- qInterpretFile fpath
        if fNoRun
            then return ()
            else action

runInteractive :: (StorageModelOptions, ModuleOptions) -> IO ()
runInteractive (smopts, modopts) =
    runLifecycle $
    runView $ do
        let
            iiScriptName = ""
            iiScriptArguments = []
            iiStdIn = stdinTextSource
            iiStdOut = stdoutTextSink
            iiStdErr = stderrTextSink
            iiDefaultStorageModel = standardStorageModel smopts
        iiEnvironment <- liftIO getEnvironment
        let ?library = standardLibraryContext MkInvocationInfo {..} modopts
        qInteract
