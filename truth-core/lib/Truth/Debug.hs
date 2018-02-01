module Truth.Debug where

import Debug.Trace
import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Read

contextStr :: String -> String -> String
contextStr "" b = b
contextStr a b = a ++ ": " ++ b

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
traceObject prefix MkEditShower {..} (MkObject (MkUnliftIO run :: UnliftIO m) r e) = let
    run' :: UnliftIO m
    run' = MkUnliftIO $ \m -> traceBracket (contextStr prefix "object") $ run m
    r' :: MutableRead m (EditReader edit)
    r' rt = traceBracketArgs (contextStr prefix "read") (showRead rt) (showReadResult rt) $ r rt
    e' :: [edit] -> m (Maybe (m ()))
    e' edits = (fmap $ fmap $ traceBracketArgs (contextStr prefix "edit") (showEdits edits) (\_ -> "")) $ e edits
    in MkObject run' r' e'

traceUnlift :: MonadTransConstraint MonadIO t => String -> Unlift t -> Unlift t
traceUnlift name unlift =
    MkUnlift $ \tma ->
        traceBracket (contextStr name "outside") $
        runUnlift unlift $ withTransConstraintTM @MonadIO $ traceBracket (contextStr name "inside") tma
