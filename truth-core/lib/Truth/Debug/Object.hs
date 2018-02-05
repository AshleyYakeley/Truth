module Truth.Debug.Object where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Read
import Truth.Debug

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
