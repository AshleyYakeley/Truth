module Truth.Debug where

import Debug.Trace
import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.MutableEdit
import Truth.Core.Object.Object

context :: String -> String -> String
context "" b = b
context a b = a ++ ": " ++ b

traceBracketArgs :: MonadIO m => String -> String -> (r -> String) -> m r -> m r
traceBracketArgs s args showr ma = do
    liftIO $ traceIO $ s ++ " [ " ++ args
    a <- ma
    liftIO $ traceIO $ s ++ " ] " ++ showr a
    return a

traceBracket :: MonadIO m => String -> m r -> m r
traceBracket s = traceBracketArgs s "" (\_ -> "")

data EditShower edit = MkEditShower
    { showRead :: forall t. EditReader edit t -> String
    , showReadResult :: forall t. EditReader edit t -> t -> String
    , showEdits :: [edit] -> String
    }

blankEditShower :: EditShower edit
blankEditShower = MkEditShower {showRead = \_ -> "", showReadResult = \_ _ -> "", showEdits = \_ -> ""}

traceMutableEdit ::
       forall m edit. MonadIO m
    => String
    -> EditShower edit
    -> MutableEdit m edit
    -> MutableEdit m edit
traceMutableEdit prefix MkEditShower {..} muted =
    MkMutableEdit
    { mutableRead =
          \rt -> traceBracketArgs (context prefix "read") (showRead rt) (showReadResult rt) $ mutableRead muted rt
    , mutableEdit =
          \edits ->
              (fmap $ fmap $ traceBracketArgs (context prefix "edit") (showEdits edits) (\_ -> "")) $
              mutableEdit muted edits
    }

traceObject :: String -> EditShower edit -> Object edit -> Object edit
traceObject prefix es (MkObject obj) =
    MkObject $ \call -> obj $ \me -> traceBracket (context prefix "object") $ call $ traceMutableEdit prefix es me
