module Run
    ( runFiles
    , runInteractive
    ) where

import Changes.Core
import Pinafore.Main
import Shapes

runFiles :: Foldable t => Bool -> ModuleOptions -> Bool -> t (FilePath, [String], [(Text, Text)]) -> IO ()
runFiles ibSloppy modopts fNoRun scripts =
    runWithOptions defaultExecutionOptions $
    runLifecycle $
    runView $
    for_ scripts $ \(fpath, args, implArgs) -> do
        let ibLoadModule = standardLoadModule modopts
        let ?behaviour = MkInterpretBehaviour {..}
        action <- qInterpretScriptFile fpath args implArgs
        if fNoRun
            then return ()
            else action

runInteractive :: Bool -> ModuleOptions -> IO ()
runInteractive ibSloppy modopts =
    runWithOptions defaultExecutionOptions $
    runLifecycle $
    runView $ do
        let ibLoadModule = standardLoadModule modopts
        let ?behaviour = MkInterpretBehaviour {..}
        qInteract
