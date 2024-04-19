module Run
    ( runFiles
    , runInteractive
    ) where

import Changes.Core
import Pinafore
import Shapes
import System.Environment

runFiles :: Foldable t => (StorageModelOptions, ModuleOptions, [Importer]) -> Bool -> t (FilePath, [String]) -> IO ()
runFiles (smopts, modopts, itopts) fNoRun scripts =
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
        let ?library = mkLibraryContext MkInvocationInfo {..} (standardFetchModule modopts) itopts
        action <- qInterpretFile fpath
        if fNoRun
            then return ()
            else action

runInteractive :: (StorageModelOptions, ModuleOptions, [Importer]) -> IO ()
runInteractive (smopts, modopts, itopts) =
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
        let ?library = mkLibraryContext MkInvocationInfo {..} (standardFetchModule modopts) itopts
        qInteract
