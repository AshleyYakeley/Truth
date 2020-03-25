module Truth.Core.Types.Reject where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Lens
import Truth.Core.Read
import Truth.Core.Types.None
import Truth.Core.Types.ReadOnly

fromReadOnlyRejectingChangeLens :: forall update. ChangeLens (ReadOnlyUpdate update) update
fromReadOnlyRejectingChangeLens = let
    clRead :: ReadFunction (UpdateReader update) (UpdateReader update)
    clRead mr = mr
    clUpdate ::
           forall m. MonadIO m
        => ReadOnlyUpdate update
        -> Readable m (UpdateReader update)
        -> m [update]
    clUpdate (MkReadOnlyUpdate update) _ = return [update]
    clPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit update]
        -> Readable m (UpdateReader update)
        -> m (Maybe [ConstEdit (UpdateReader update)])
    clPutEdits [] _ = return $ Just [] -- must allow empty update-lists so that composition works correctly
    clPutEdits (_:_) _ = return Nothing
    in MkChangeLens {..}
