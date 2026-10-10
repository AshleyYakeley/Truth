module Run
    ( runFiles
    , runInteractive
    )
where

import Changes.Core
import Pinafore.Main
import Shapes

runFiles :: Foldable t => LibraryContext -> Bool -> t (FilePath, [String], [(Text, Text)]) -> IO ()
runFiles libraryContext fNoRun scripts =
    runWithOptions defaultExecutionOptions
        $ runLifecycle
        $ runView
        $ for_ scripts
        $ \(fpath, args, implArgs) -> do
            let ?library = libraryContext
            action <- qInterpretScriptFile fpath args implArgs
            if fNoRun
                then return ()
                else action

runInteractive :: LibraryContext -> IO ()
runInteractive libraryContext =
    runWithOptions defaultExecutionOptions
        $ runLifecycle
        $ runView
        $ do
            let ?library = libraryContext
            qInteract
