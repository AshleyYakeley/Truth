module Truth.Core.Edit.Update where

import Truth.Core.Edit.Edit
import Truth.Core.Import
import Truth.Core.Read

class IsUpdate (update :: Type) where
    type UpdateEdit update :: Type
    editUpdate :: UpdateEdit update -> update

type UpdateReader update = EditReader (UpdateEdit update)

type UpdateSubject update = ReaderSubject (UpdateReader update)

class IsUpdate update => IsEditUpdate update where
    updateEdit :: update -> UpdateEdit update

newtype EditUpdate edit =
    MkEditUpdate edit
    deriving (Eq, Countable, Searchable)

instance Finite edit => Finite (EditUpdate edit) where
    allValues = fmap MkEditUpdate allValues

deriving instance Empty edit => Empty (EditUpdate edit)

instance Show edit => Show (EditUpdate edit) where
    show (MkEditUpdate edit) = show edit

instance IsUpdate (EditUpdate edit) where
    type UpdateEdit (EditUpdate edit) = edit
    editUpdate = MkEditUpdate

instance IsEditUpdate (EditUpdate edit) where
    updateEdit (MkEditUpdate edit) = edit

data PartialUpdate update
    = KnownPartialUpdate update
    | UnknownPartialUpdate

instance IsUpdate update => IsUpdate (PartialUpdate update) where
    type UpdateEdit (PartialUpdate update) = UpdateEdit update
    editUpdate edit = KnownPartialUpdate $ editUpdate edit
