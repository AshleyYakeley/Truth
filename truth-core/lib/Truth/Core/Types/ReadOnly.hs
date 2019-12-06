module Truth.Core.Types.ReadOnly where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.None

newtype ReadOnlyUpdate update =
    MkReadOnlyUpdate update

instance IsUpdate (ReadOnlyUpdate update) where
    type UpdateEdit (ReadOnlyUpdate update) = NoEdit (UpdateReader update)
    editUpdate = never

toReadOnlyEditLens :: forall update. EditLens update (ReadOnlyUpdate update)
toReadOnlyEditLens = let
    ufGet :: ReadFunction (UpdateReader update) (UpdateReader update)
    ufGet mr = mr
    ufUpdate ::
           forall m. MonadIO m
        => update
        -> MutableRead m (UpdateReader update)
        -> m [ReadOnlyUpdate update]
    ufUpdate update _ = return [MkReadOnlyUpdate update]
    elFunction = MkUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [NoEdit (UpdateReader update)]
        -> MutableRead m (UpdateReader update)
        -> m (Maybe [UpdateEdit update])
    elPutEdits [] _ = return $ Just []
    elPutEdits (e:_) _ = never e
    in MkEditLens {..}

fromReadOnlyUpdateFunction :: forall update. UpdateFunction (ReadOnlyUpdate update) update
fromReadOnlyUpdateFunction = let
    ufGet :: ReadFunction (UpdateReader update) (UpdateReader update)
    ufGet mr = mr
    ufUpdate ::
           forall m. MonadIO m
        => ReadOnlyUpdate update
        -> MutableRead m (UpdateReader update)
        -> m [update]
    ufUpdate (MkReadOnlyUpdate update) _ = return [update]
    in MkUpdateFunction {..}
