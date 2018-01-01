module Truth.Debug where

import Debug.Trace
import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Read

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

traceObject :: forall edit. String -> EditShower edit -> Object edit -> Object edit
traceObject prefix MkEditShower {..} (MkObject (run :: UnliftIO m) r e) = let
    run' :: UnliftIO m
    run' m = traceBracket (context prefix "object") $ run m
    r' :: MutableRead m (EditReader edit)
    r' rt = traceBracketArgs (context prefix "read") (showRead rt) (showReadResult rt) $ r rt
    e' :: [edit] -> m (Maybe (m ()))
    e' edits = (fmap $ fmap $ traceBracketArgs (context prefix "edit") (showEdits edits) (\_ -> "")) $ e edits
    in MkObject run' r' e'
