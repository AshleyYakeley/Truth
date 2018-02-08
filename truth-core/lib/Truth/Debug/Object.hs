module Truth.Debug.Object where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Read
import Truth.Debug

data EditShower edit = MkEditShower
    { showRead :: forall t. EditReader edit t -> String
    , showReadResult :: forall t. EditReader edit t -> t -> String
    , showEdit :: edit -> String
    }

blankEditShower :: EditShower edit
blankEditShower = MkEditShower {showRead = \_ -> "", showReadResult = \_ _ -> "", showEdit = \_ -> "edit"}

traceObject :: forall edit. String -> EditShower edit -> Object edit -> Object edit
traceObject prefix MkEditShower {..} (MkObject (MkUnliftIO run :: UnliftIO m) r e) = let
    run' :: UnliftIO m
    run' = MkUnliftIO $ \m -> traceBracket (contextStr prefix "object") $ run m
    r' :: MutableRead m (EditReader edit)
    r' rt = traceBracketArgs (contextStr prefix "read") (showRead rt) (showReadResult rt) $ r rt
    e' :: [edit] -> m (Maybe (m ()))
    e' edits =
        (fmap $
         fmap $
         traceBracketArgs (contextStr prefix "edit") ("[" ++ intercalate "," (fmap showEdit edits) ++ "]") (\_ -> "")) $
        e edits
    in MkObject run' r' e'

showEditShower ::
       forall edit. (Show edit, AllWitnessConstraint Show (EditReader edit), WitnessConstraint Show (EditReader edit))
    => EditShower edit
showEditShower = let
    showRead rt = showAllWitness rt
    showReadResult :: forall t. EditReader edit t -> t -> String
    showReadResult rt t =
        case witnessConstraint @_ @Show rt of
            Dict -> show t
    showEdit = show
    in MkEditShower {..}

traceObject' ::
       (Show edit, AllWitnessConstraint Show (EditReader edit), WitnessConstraint Show (EditReader edit))
    => String
    -> Object edit
    -> Object edit
traceObject' prefix = traceObject prefix showEditShower
