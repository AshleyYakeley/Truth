module Truth.Core.Edit.Opaque where

import Truth.Core.Edit.Update
import Truth.Core.Import

data OpaqueUpdate (edit :: Type) =
    MkOpaqueUpdate

type instance UpdateEdit (OpaqueUpdate edit) = edit

instance IsUpdate (OpaqueUpdate edit) where
    editUpdate _edit = MkOpaqueUpdate
