module Run
    ( runFiles
    , runInteractive
    ) where

import Changes.Core
import Pinafore.Main
import Shapes

runFiles :: Foldable t => ModuleOptions -> Bool -> t (FilePath, [String]) -> IO ()
runFiles modopts fNoRun scripts =
    runWithOptions defaultExecutionOptions $
    runLifecycle $
    runView $
    for_ scripts $ \(fpath, args) -> do
        let ?library = standardLibraryContext modopts
        action <- qInterpretScriptFile fpath args []
        if fNoRun
            then return ()
            else action

runInteractive :: ModuleOptions -> IO ()
runInteractive modopts =
    runWithOptions defaultExecutionOptions $
    runLifecycle $
    runView $ do
        let ?library = standardLibraryContext modopts
        qInteract
