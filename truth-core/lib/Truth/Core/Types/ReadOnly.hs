module Truth.Core.Types.ReadOnly where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.None

newtype ReadOnlyUpdate update = MkReadOnlyUpdate
    { unReadOnlyUpdate :: update
    }

instance IsUpdate (ReadOnlyUpdate update) where
    type UpdateEdit (ReadOnlyUpdate update) = NoEdit (UpdateReader update)
    editUpdate = never

toReadOnlyUpdateFunction :: forall update. UpdateFunction update (ReadOnlyUpdate update)
toReadOnlyUpdateFunction = let
    ufGet :: ReadFunction (UpdateReader update) (UpdateReader update)
    ufGet mr = mr
    ufUpdate ::
           forall m. MonadIO m
        => update
        -> MutableRead m (UpdateReader update)
        -> m [ReadOnlyUpdate update]
    ufUpdate update _ = return [MkReadOnlyUpdate update]
    in MkUpdateFunction {..}

toReadOnlyEditLens :: forall update. EditLens update (ReadOnlyUpdate update)
toReadOnlyEditLens = let
    elFunction = toReadOnlyUpdateFunction
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

updateFunctionToEditLens ::
       forall updateA updateB. UpdateFunction updateA updateB -> EditLens updateA (ReadOnlyUpdate updateB)
updateFunctionToEditLens f = let
    elFunction :: UpdateFunction updateA (ReadOnlyUpdate updateB)
    elFunction = toReadOnlyUpdateFunction . f
    elPutEdits ::
           forall m. MonadIO m
        => [NoEdit (UpdateReader updateB)]
        -> MutableRead m (UpdateReader updateA)
        -> m (Maybe [UpdateEdit updateA])
    elPutEdits [] _ = return $ Just []
    elPutEdits (e:_) _ = never e
    in MkEditLens {..}

editLensToUpdateFunction ::
       forall updateA updateB. EditLens updateA (ReadOnlyUpdate updateB) -> UpdateFunction updateA updateB
editLensToUpdateFunction lens = fromReadOnlyUpdateFunction . elFunction lens
