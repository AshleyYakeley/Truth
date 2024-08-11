module Run
    ( runFiles
    , runInteractive
    ) where

import Changes.Core
import GHC.Conc
import Pinafore.Main
import Shapes
import System.Environment

parallelExecutionINTERNAL :: Bool
parallelExecutionINTERNAL = True

setup :: IO ()
setup = do
    if parallelExecutionINTERNAL
        then do
            np <- getNumProcessors
            -- use all processors
            setNumCapabilities np
        else return ()

runFiles :: Foldable t => (StorageModelOptions, ModuleOptions) -> Bool -> t (FilePath, [String]) -> IO ()
runFiles (smopts, modopts) fNoRun scripts = do
    setup
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
runInteractive (smopts, modopts) = do
    setup
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
