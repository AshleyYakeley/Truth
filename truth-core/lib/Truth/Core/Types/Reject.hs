module Truth.Core.Types.Reject where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Lens
import Truth.Core.Read
import Truth.Core.Types.None
import Truth.Core.Types.ReadOnly

fromReadOnlyRejectingEditLens :: forall update. EditLens (ReadOnlyUpdate update) update
fromReadOnlyRejectingEditLens = let
    elGet :: ReadFunction (UpdateReader update) (UpdateReader update)
    elGet mr = mr
    elUpdate ::
           forall m. MonadIO m
        => ReadOnlyUpdate update
        -> MutableRead m (UpdateReader update)
        -> m [update]
    elUpdate (MkReadOnlyUpdate update) _ = return [update]
    elPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit update]
        -> MutableRead m (UpdateReader update)
        -> m (Maybe [NoEdit (UpdateReader update)])
    elPutEdits [] _ = return $ Just [] -- must allow empty update-lists so that composition works correctly
    elPutEdits (_:_) _ = return Nothing
    in MkEditLens {..}
