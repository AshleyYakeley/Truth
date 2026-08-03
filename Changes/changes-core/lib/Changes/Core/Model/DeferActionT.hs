module Changes.Core.Model.DeferActionT
    ( DeferAction
    , deferAction
    , deferActionResourceRunner
    )
where

import Changes.Core.Import
import Changes.Core.Resource

newtype DeferAction = MkDeferAction (IO () -> IO ())

deferAction :: DeferAction -> IO () -> IO ()
deferAction (MkDeferAction addAction) = addAction

runDeferActions :: With IO DeferAction
runDeferActions call = do
    actionsVar <- newMVar []
    result <- call $ MkDeferAction $ \action -> modifyMVar_ actionsVar $ \actions -> return $ action : actions
    actions <- readMVar actionsVar
    sequence_ $ reverse actions
    return result

deferActionResourceRunner ::
    forall mc m.
    MonadIO m =>
    LifecycleT mc m (ResourceRunner (DeferAction, ()))
deferActionResourceRunner = liftIO $ newResourceRunner runDeferActions
