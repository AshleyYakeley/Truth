module Changes.Core.Edit.Update where

import Changes.Core.Edit.Edit
import Changes.Core.Edit.FullEdit
import Changes.Core.Import
import Changes.Core.Read

type family UpdateEdit (update :: Type) :: Type

class IsUpdate (update :: Type) where
    editUpdate :: UpdateEdit update -> update

type UpdateReader update = EditReader (UpdateEdit update)

type UpdateSubject update = ReaderSubject (UpdateReader update)

class IsUpdate update => IsEditUpdate update where
    updateEdit :: update -> UpdateEdit update

newtype EditUpdate edit =
    MkEditUpdate edit
    deriving newtype (Eq, Countable, Searchable)

instance Finite edit => Finite (EditUpdate edit) where
    allValues = fmap MkEditUpdate allValues

deriving newtype instance Empty edit => Empty (EditUpdate edit)

instance Show edit => Show (EditUpdate edit) where
    show (MkEditUpdate edit) = show edit

type instance UpdateEdit (EditUpdate edit) = edit

instance IsUpdate (EditUpdate edit) where
    editUpdate = MkEditUpdate

instance IsEditUpdate (EditUpdate edit) where
    updateEdit (MkEditUpdate edit) = edit

class FullUpdate update where
    -- | Updates do not have to be precise, only comprehensive
    replaceUpdate ::
           forall m. (MonadIO m)
        => Readable m (UpdateReader update)
        -> (update -> m ())
        -> m ()
    default replaceUpdate ::
        (IsEditUpdate update, FullEdit (UpdateEdit update)) =>
                forall m. (MonadIO m) => Readable m (UpdateReader update) -> (update -> m ()) -> m ()
    replaceUpdate rd pushUpdate = replaceEdit rd $ \edit -> pushUpdate $ editUpdate edit

getReplaceUpdates ::
       forall m update. (FullUpdate update, MonadIO m)
    => Readable m (UpdateReader update)
    -> m [update]
getReplaceUpdates mr = execWriterT $ replaceUpdate (hoistReadable lift mr) $ tell . pure

getReplaceUpdatesFromSubject ::
       forall m update. (SubjectReader (UpdateReader update), FullUpdate update, MonadIO m)
    => UpdateSubject update
    -> m [update]
getReplaceUpdatesFromSubject subj = getReplaceUpdates $ subjectToReadable subj

instance FullEdit edit => FullUpdate (EditUpdate edit)
