module Truth.Core.Types.Reject where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.None
import Truth.Core.Types.ReadOnly

fromReadOnlyRejectingEditLens :: forall update. EditLens (ReadOnlyUpdate update) update
fromReadOnlyRejectingEditLens = let
    elFunction = fromReadOnlyUpdateFunction
    elPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit update]
        -> MutableRead m (UpdateReader update)
        -> m (Maybe [NoEdit (UpdateReader update)])
    elPutEdits [] _ = return $ Just [] -- must allow empty update-lists so that composition works correctly
    elPutEdits (_:_) _ = return Nothing
    in MkEditLens {..}

updateFunctionToRejectingEditLens :: forall updateA updateB. UpdateFunction updateA updateB -> EditLens updateA updateB
updateFunctionToRejectingEditLens f = fromReadOnlyRejectingEditLens . updateFunctionToEditLens f

constEditLens ::
       forall updateA updateB. SubjectReader (UpdateReader updateB)
    => UpdateSubject updateB
    -> EditLens updateA updateB
constEditLens b = updateFunctionToRejectingEditLens $ constUpdateFunction b
