module Truth.Core.Edit.Partial where

import Truth.Core.Edit.FullEdit
import Truth.Core.Edit.Function
import Truth.Core.Edit.Lens
import Truth.Core.Edit.Unlift
import Truth.Core.Edit.Update
import Truth.Core.Import
import Truth.Core.Read

data PartialUpdate update
    = KnownPartialUpdate update
    | UnknownPartialUpdate

instance IsUpdate update => IsUpdate (PartialUpdate update) where
    type UpdateEdit (PartialUpdate update) = UpdateEdit update
    editUpdate edit = KnownPartialUpdate $ editUpdate edit

partialFullEditLens ::
       forall update. (IsUpdate update, FullEdit (UpdateEdit update))
    => EditLens (PartialUpdate update) update
partialFullEditLens = let
    ufGet ::
           forall m t. MonadIO m
        => MutableRead m (UpdateReader update)
        -> UpdateReader update t
        -> IdentityT m t
    ufGet mr rt = lift $ mr rt
    ufUpdate ::
           forall m. MonadIO m
        => PartialUpdate update
        -> MutableRead m (UpdateReader update)
        -> IdentityT m [update]
    ufUpdate (KnownPartialUpdate update) _ = return [update]
    ufUpdate UnknownPartialUpdate mr =
        lift $ do
            edits <- getReplaceEdits mr
            return $ fmap editUpdate edits
    elFunction :: AnUpdateFunction IdentityT (PartialUpdate update) update
    elFunction = MkAnUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit update]
        -> MutableRead m (UpdateReader update)
        -> IdentityT m (Maybe [UpdateEdit update])
    elPutEdits edits _ = return $ Just edits
    in MkCloseUnlift identityUnlift MkAnEditLens {..}
