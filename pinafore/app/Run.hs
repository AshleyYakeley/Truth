module Run
    ( runFiles
    , runInteractive
    ) where

import Changes.Core
import Changes.UI.GTK
import Pinafore
import Shapes
import System.Environment

runFiles :: Foldable t => ContextOptions -> Bool -> t (FilePath, [String]) -> IO ()
runFiles copts fNoRun scripts =
    changesMainGTK $ \cc ->
        for_ scripts $ \(fpath, iiScriptArguments) -> do
            let iiScriptName = fpath
            iiEnvironment <- liftIO getEnvironment
            (context, fetchModule) <- standardPinaforeContext copts MkInvocationInfo {..} cc
            liftToLifeCycle $ do
                action <- let
                    ?pinafore = context
                    ?fetchModule = fetchModule
                    in pinaforeInterpretFile fpath
                if fNoRun
                    then return ()
                    else action

runInteractive :: ContextOptions -> IO ()
runInteractive copts =
    changesMainGTK $ \cc -> do
        let
            iiScriptName = ""
            iiScriptArguments = []
        iiEnvironment <- liftIO getEnvironment
        (context, fetchModule) <- standardPinaforeContext copts MkInvocationInfo {..} cc
        let
            ?pinafore = context
            ?fetchModule = fetchModule
            in liftToLifeCycle pinaforeInteract
        liftToLifeCycle viewExit
