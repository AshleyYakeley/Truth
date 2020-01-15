module Truth.Core.Edit.Update where

import Truth.Core.Edit.Edit
import Truth.Core.Import
import Truth.Core.Read

type family UpdateEdit (update :: Type) :: Type

class IsUpdate (update :: Type) where
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

type instance UpdateEdit (EditUpdate edit) = edit

instance IsUpdate (EditUpdate edit) where
    editUpdate = MkEditUpdate

instance IsEditUpdate (EditUpdate edit) where
    updateEdit (MkEditUpdate edit) = edit
